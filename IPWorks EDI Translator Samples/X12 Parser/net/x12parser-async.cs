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

class x12parserDemo
{
  private static X12reader x12reader = new nsoftware.async.IPWorksEDITranslator.X12reader();

  static async Task Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: x12parser /f file");
      Console.WriteLine("  file         the x12 file to parse");
      Console.WriteLine("\nExample: x12parser /f ./x12.txt");
    }
    else
    {
      x12reader.OnEndFunctionalGroup += (s, e) => { Console.WriteLine("EndFunctionalGroup: " + e.Tag); };
      x12reader.OnEndInterchange += (s, e) => { Console.WriteLine("EndInterchange: " + e.Tag); };
      x12reader.OnEndLoop += (s, e) => { Console.WriteLine("EndLoop"); };
      x12reader.OnEndTransaction += (s, e) => { Console.WriteLine("EndTransaction: " + e.Tag); };
      x12reader.OnError += (s, e) => { Console.WriteLine("Error " + e.ErrorCode + ": " + e.Description); };
      x12reader.OnResolveSchema += (s, e) => { Console.WriteLine("ResolveSchema: " + e.TransactionCode); };
      x12reader.OnSegment += (s, e) => { Console.WriteLine("Segment: " + e.Name); };
      x12reader.OnStartFunctionalGroup += (s, e) => { Console.WriteLine("StartFunctionalGroup: " + e.Tag); };
      x12reader.OnStartInterchange += (s, e) => { Console.WriteLine("StartInterchange: " + e.Tag); };
      x12reader.OnStartLoop += (s, e) => { Console.WriteLine("StartLoop: " + e.Name); };
      x12reader.OnStartTransaction += (s, e) => { Console.WriteLine("StartTransaction: " + e.Tag); };
      x12reader.OnWarning += (s, e) => { Console.WriteLine("Warning " + e.WarnCode + ": " + e.Message); };

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        x12reader.InputFile = myArgs["f"];

        await x12reader.Config("Encoding=iso-8859-1");
        x12reader.SchemaFormat = X12readerSchemaFormats.schemaJSON;
        await x12reader.LoadSchema("../../../RSSBus_00401_810.json");

        // This demo provides information about the parsed document through the events.
        // To navigate the document using the XPath property, first set:
        // x12reader.Config("ResolveXPathOnSet=true");
        await x12reader.Parse();
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