/*
 * IPWorks EDI Translator 2022 .NET Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks EDI Translator in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkseditranslator
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 * 
 */

using System.Collections.Generic;
﻿using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorksEDITranslator;

class edifactparserDemo
{
  private static Edifactreader edifactreader = new nsoftware.async.IPWorksEDITranslator.Edifactreader();

  static async Task Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: edifactparser /f file");
      Console.WriteLine("  file         the edi file to parse");
      Console.WriteLine("\nExample: edifactparser /f ./INVOIC.edi");
    }
    else
    {
      edifactreader.OnEndFunctionalGroup += (s, e) => { Console.WriteLine("EndFunctionalGroup: " + e.Tag); };
      edifactreader.OnEndInterchange += (s, e) => { Console.WriteLine("EndInterchange: " + e.Tag); };
      edifactreader.OnEndLoop += (s, e) => { Console.WriteLine("EndLoop"); };
      edifactreader.OnEndTransaction += (s, e) => { Console.WriteLine("EndTransaction: " + e.Tag); };
      edifactreader.OnError += (s, e) => { Console.WriteLine("Error " + e.ErrorCode + ": " + e.Description); };
      edifactreader.OnResolveSchema += (s, e) => { Console.WriteLine("ResolveSchema: " + e.TransactionCode); };
      edifactreader.OnSegment += (s, e) => { Console.WriteLine("Segment: " + e.Name); };
      edifactreader.OnStartFunctionalGroup += (s, e) => { Console.WriteLine("StartFunctionalGroup: " + e.Tag); };
      edifactreader.OnStartInterchange += (s, e) => { Console.WriteLine("StartInterchange: " + e.Tag); };
      edifactreader.OnStartLoop += (s, e) => { Console.WriteLine("StartLoop: " + e.Name); };
      edifactreader.OnStartTransaction += (s, e) => { Console.WriteLine("StartTransaction: " + e.Tag); };
      edifactreader.OnWarning += (s, e) => { Console.WriteLine("Warning " + e.WarnCode + ": " + e.Message); };

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        edifactreader.InputFile = myArgs["f"];

        await edifactreader.Config("Encoding=iso-8859-1");
        edifactreader.SchemaFormat = EdifactreaderSchemaFormats.schemaJSON;
        await edifactreader.LoadSchema("../../../RSSBus_D97A_INVOIC.json");

        // This demo provides information about the parsed document through the events.
        // To navigate the document using the XPath property, first set:
        // edifactreader.Config("ResolveXPathOnSet=true");
        await edifactreader.Parse();
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }
}


class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}