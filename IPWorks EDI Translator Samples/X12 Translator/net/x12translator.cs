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

class x12translatorDemo
{
  private static X12Translator x12translator = new nsoftware.IPWorksEDITranslator.X12Translator();

  static void Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: x12translator /tm translationmode /f inputfile /o outputfile");
      Console.WriteLine("  translationmode  1 to translate X12 to XML or 2 to translate XML to X12");
      Console.WriteLine("  inputfile        the file to translate");
      Console.WriteLine("  outputfile       the file to which the translated data will be written\n");
      Console.WriteLine("Example: x12translator /tm 1 /f ./x12.txt /o ./810.xml");
      Console.WriteLine("Example: x12translator /tm 2 /f ./810.xml /o ./810_fromxml.txt");
    }
    else
    {
      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        switch (myArgs["tm"])
        {
          case "1":
            Console.WriteLine("Converting X12 File to XML File.");
            x12translator.Reset();
            x12translator.LoadSchema("../../../RSSBus_00401_810.json");
            x12translator.InputFile = myArgs["f"];
            x12translator.OutputFile = myArgs["o"];
            x12translator.Overwrite = true;

            x12translator.Translate();
            Console.WriteLine("Translation complete.");
            break;
          case "2":
            Console.WriteLine("Converting XML File to X12 File.");
            x12translator.Reset();
            x12translator.InputFormat = X12TranslatorInputFormats.xifXML;
            x12translator.OutputFormat = X12TranslatorOutputFormats.xofX12;
            x12translator.InputFile = myArgs["f"];
            x12translator.OutputFile = myArgs["o"];
            x12translator.Overwrite = true;

            x12translator.Translate();
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