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

class edifacttranslatorDemo
{
  private static EDIFACTTranslator edifacttranslator = new nsoftware.IPWorksEDITranslator.EDIFACTTranslator();

  static void Main(string[] args)
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
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        switch (myArgs["tm"])
        {
          case "1":
            Console.WriteLine("Converting EDIFACT File to XML File.");
            edifacttranslator.Reset();
            edifacttranslator.LoadSchema("../../../RSSBus_D97A_INVOIC.json");
            edifacttranslator.InputFile = myArgs["f"];
            edifacttranslator.OutputFile = myArgs["o"];
            edifacttranslator.Overwrite = true;

            edifacttranslator.Translate();
            Console.WriteLine("Translation complete.");
            break;
          case "2":
            Console.WriteLine("Converting XML File to EDIFACT File.");
            edifacttranslator.Reset();
            edifacttranslator.InputFormat = EDIFACTTranslatorInputFormats.eifXML;
            edifacttranslator.OutputFormat = EDIFACTTranslatorOutputFormats.eofEDIFACT;
            edifacttranslator.InputFile = myArgs["f"];
            edifacttranslator.OutputFile = myArgs["o"];
            edifacttranslator.Overwrite = true;

            edifacttranslator.Translate();
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