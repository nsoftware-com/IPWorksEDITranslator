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

class edifacttranslatorDemo
{
  private static Edifacttranslator edifacttranslator = new nsoftware.async.IPWorksEDITranslator.Edifacttranslator();

  static async Task Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: edifacttranslator /tm translationmode /f inputfile /o outputfile");
      Console.WriteLine("  translationmode  1 to translate EDI to XML or 2 to translate XML to EDI");
      Console.WriteLine("  inputfile        the file to translate");
      Console.WriteLine("  outputfile       the file to which the translated data will be written\n");
      Console.WriteLine("Example: edifacttranslator /tm 1 /f ./INVOIC.edi /o ./INVOICE.xml");
      Console.WriteLine("Example: edifacttranslator /tm 2 /f ./INVOICE.xml /o ./INVOIC_fromxml.edi");
    }
    else
    {
      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        switch (myArgs["tm"])
        {
          case "1":
            Console.WriteLine("Converting EDIFACT File to XML File.");
            await edifacttranslator.Reset();
            await edifacttranslator.LoadSchema("../../../RSSBus_D97A_INVOIC.json");
            edifacttranslator.InputFile = myArgs["f"];
            edifacttranslator.OutputFile = myArgs["o"];
            edifacttranslator.Overwrite = true;

            await edifacttranslator.Translate();
            Console.WriteLine("Translation complete.");
            break;
          case "2":
            Console.WriteLine("Converting XML File to EDIFACT File.");
            await edifacttranslator.Reset();
            edifacttranslator.InputFormat = EdifacttranslatorInputFormats.eifXML;
            edifacttranslator.OutputFormat = EdifacttranslatorOutputFormats.eofEDIFACT;
            edifacttranslator.InputFile = myArgs["f"];
            edifacttranslator.OutputFile = myArgs["o"];
            edifacttranslator.Overwrite = true;

            await edifacttranslator.Translate();
            Console.WriteLine("Translation complete.");
            break;
          default:
            Console.WriteLine("Unrecognized option.");
            break;
        }
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