/*
 * IPWorks EDI Translator 2024 .NET Edition - Sample Project
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

﻿using System;
using nsoftware.IPWorksEDITranslator;

class edifactparserDemo
{
  private static EDIFACTReader edifactreader = new nsoftware.IPWorksEDITranslator.EDIFACTReader();

  static void Main(string[] args)
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
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        edifactreader.InputFile = myArgs["f"];

        edifactreader.Config("Encoding=iso-8859-1");
        edifactreader.SchemaFormat = EDIFACTReaderSchemaFormats.schemaJSON;
        edifactreader.LoadSchema("../../../RSSBus_D97A_INVOIC.json");

        // This demo provides information about the parsed document through the events.
        // To navigate the document using the XPath property, first set:
        // edifactreader.Config("ResolveXPathOnSet=true");
        edifactreader.Parse();
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
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}