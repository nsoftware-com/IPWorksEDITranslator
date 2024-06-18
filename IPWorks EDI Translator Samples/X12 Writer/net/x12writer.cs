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

class x12writerDemo
{
  private static X12Writer x12writer = new nsoftware.IPWorksEDITranslator.X12Writer();

  static void Main(string[] args)
  {
    try
    {
      string docType = "";

      while (docType != "810" && docType != "850" && docType != "855" && docType != "856")
      {
        Console.Write("Document Type To Generate [810, 850, 855, 856]: ");
        docType = Console.ReadLine();
      }

      x12writer.Config("Encoding=iso-8859-1");
      x12writer.LoadSchema("../../../RSSBus_00401_" + docType + ".json");
      x12writer.Suffix = X12WriterSuffixes.suffixCRLF;

      // Will print output to console.
      x12writer.SetOutputStream(Console.OpenStandardOutput());

      // To save to a file, use
      // x12writer.OutputFile = "filename.txt";

      // To save data to a string, make sure no OutputFile or OutputStream has been set.
      // x12writer.OutputFile = "";
      // To get data after it has been written, use x12writer.OutputData

      Console.WriteLine("\nGenerating EDI document and printing to console:\n");
      // Print correct document.
      switch (docType)
      {
        case "810":
          WriteFile_X12_810(x12writer);
          break;
        case "850":
          WriteFile_X12_850(x12writer);
          break;
        case "855":
          WriteFile_X12_855(x12writer);
          break;
        case "856":
          WriteFile_X12_856(x12writer);
          break;
        default:
          throw new Exception("Invalid document selection.\n");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_X12_810(X12Writer x12writer)
  {
    try
    {
      x12writer.StartInterchangeHeader("004010");
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("U");
      x12writer.WriteElementString("00401");
      x12writer.WriteElementString("000000006");
      x12writer.WriteElementString("0");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString(">");
      x12writer.EndElement();

      x12writer.StartFunctionalGroupHeader();
      x12writer.WriteElementString("IN");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("20160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("6");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString("004010");
      x12writer.EndElement();

      x12writer.StartTransactionHeader("810");
      x12writer.WriteElementString("810");
      x12writer.WriteElementString("0001");
      x12writer.EndElement();

      x12writer.StartSegment("BIG");
      x12writer.WriteElementString("20150708");
      x12writer.WriteElementString("3003014445");
      x12writer.SkipElement();
      x12writer.WriteElementString("0476553272");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("DR");
      x12writer.EndElement();

      x12writer.StartSegment("CUR");
      x12writer.WriteElementString("SE");
      x12writer.WriteElementString("USD");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("8M");
      x12writer.WriteElementString("0056");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("BY");
      x12writer.WriteElementString("Company");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("544380");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N3");
      x12writer.WriteElementString("Address");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N4");
      x12writer.WriteElementString("City");
      x12writer.WriteElementString("CA");
      x12writer.WriteElementString("Postal Code");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("ST");
      x12writer.WriteElementString("Name");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("0607047800010");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N3");
      x12writer.WriteElementString("Address");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N4");
      x12writer.WriteElementString("City");
      x12writer.SkipElement();
      x12writer.WriteElementString("200131");
      x12writer.WriteElementString("Country");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("RE");
      x12writer.WriteElementString("Name");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("5095956");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N3");
      x12writer.WriteElementString("Address");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N4");
      x12writer.WriteElementString("City");
      x12writer.WriteElementString("IL");
      x12writer.WriteElementString("Postal Code");
      x12writer.EndElement();

      x12writer.StartSegment("IT1Loop1/IT1");
      x12writer.WriteElementString("20");
      x12writer.WriteElementString("2500");
      x12writer.WriteElementString("EA");
      x12writer.WriteElementString("36.96");
      x12writer.SkipElement();
      x12writer.WriteElementString("BP");
      x12writer.WriteElementString("335S0594");
      x12writer.EndElement();

      x12writer.StartSegment("IT1Loop1/REF_3");
      x12writer.WriteElementString("KK");
      x12writer.WriteElementString("0099778154");
      x12writer.EndElement();

      x12writer.StartSegment("IT1Loop1/REF_3");
      x12writer.WriteElementString("PO");
      x12writer.WriteElementString("0476553272");
      x12writer.WriteElementString("20");
      x12writer.EndElement();

      x12writer.StartSegment("TDS");
      x12writer.WriteElementString("9240000");
      x12writer.EndElement();

      x12writer.StartSegment("CTT");
      x12writer.WriteElementString("1");
      x12writer.EndElement();

      x12writer.CreateTransactionFooter();

      x12writer.CreateFunctionalGroupFooter();

      x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_X12_850(X12Writer x12writer)
  {
    try
    {
      x12writer.StartInterchangeHeader("004010");
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("U");
      x12writer.WriteElementString("00401");
      x12writer.WriteElementString("000000007");
      x12writer.WriteElementString("0");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString(">");
      x12writer.EndElement();

      x12writer.StartFunctionalGroupHeader();
      x12writer.WriteElementString("PO");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("20160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("7");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString("004010");
      x12writer.EndElement();

      x12writer.StartTransactionHeader("850");
      x12writer.WriteElementString("850");
      x12writer.WriteElementString("0001");
      x12writer.EndElement();

      x12writer.StartSegment("BEG");
      x12writer.WriteElementString("00");
      x12writer.WriteElementString("DS");
      x12writer.WriteElementString("0476696888");
      x12writer.SkipElement();
      x12writer.WriteElementString("20150708");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("SB");
      x12writer.WriteElementString("ZZ11");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("6P");
      x12writer.WriteElementString("ZZ");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("8M");
      x12writer.WriteElementString("0056");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("CR");
      x12writer.WriteElementString("1070335099");
      x12writer.EndElement();

      x12writer.StartSegment("REF");
      x12writer.WriteElementString("CO");
      x12writer.WriteElementString("7109790082");
      x12writer.EndElement();

      x12writer.StartSegment("PER");
      x12writer.WriteElementString("CN");
      x12writer.WriteElementString("name");
      x12writer.WriteElementString("TE");
      x12writer.WriteElementString("Number");

      x12writer.StartSegment("CSH");
      x12writer.WriteElementString("BK");
      x12writer.EndElement();

      x12writer.StartSegment("SACLoop1/SAC");
      x12writer.WriteElementString("C");
      x12writer.WriteElementString("ZZZZ");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("06");
      x12writer.EndElement();

      x12writer.StartSegment("TD5");
      x12writer.WriteElementString("Z");
      x12writer.WriteElementString("2");
      x12writer.WriteElementString("Code");
      x12writer.EndElement();

      x12writer.StartSegment("N9Loop1/N9");
      x12writer.WriteElementString("PD");
      x12writer.WriteElementString("ZCOF");
      x12writer.EndElement();

      x12writer.StartSegment("N9Loop1/MSG");
      x12writer.WriteElementString("Thanks!");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("BY");
      x12writer.WriteElementString("Name");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("5601");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("EN");
      x12writer.WriteElementString("Name");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N1");
      x12writer.WriteElementString("ST");
      x12writer.WriteElementString("OEM NAME");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("0000505462");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N2");
      x12writer.WriteElementString("additional name");
      x12writer.WriteElementString(""); // Not skipped because last element
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N3");
      x12writer.WriteElementString("Address");
      x12writer.WriteElementString("Address");
      x12writer.EndElement();

      x12writer.StartSegment("N1Loop1/N4");
      x12writer.WriteElementString("City");
      x12writer.SkipElement();
      x12writer.WriteElementString("201613");
      x12writer.WriteElementString("CN");
      x12writer.WriteElementString("SP");
      x12writer.WriteElementString("020");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/PO1");
      x12writer.WriteElementString("00010");
      x12writer.WriteElementString("500000");
      x12writer.WriteElementString("EA");
      x12writer.WriteElementString("495");
      x12writer.SkipElement();
      x12writer.WriteElementString("BP");
      x12writer.WriteElementString("337S3744");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/PIDLoop1/PID_2");
      x12writer.WriteElementString("F");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("Thanks!");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("EN");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/REF_7");
      x12writer.WriteElementString("CO");
      x12writer.WriteElementString("7109790082");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/REF_7");
      x12writer.WriteElementString("LI");
      x12writer.WriteElementString("000010");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/SCHLoop1/SCH");
      x12writer.WriteElementString("500000");
      x12writer.WriteElementString("EA");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("002");
      x12writer.WriteElementString("20180708");
      x12writer.EndElement();

      x12writer.StartSegment("CTTLoop1/CTT");
      x12writer.WriteElementString("1");
      x12writer.WriteElementString("500000");
      x12writer.EndElement();

      x12writer.CreateTransactionFooter();

      x12writer.CreateFunctionalGroupFooter();

      x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_X12_855(X12Writer x12writer)
  {
    try
    {
      x12writer.StartInterchangeHeader("004010");
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("U");
      x12writer.WriteElementString("00401");
      x12writer.WriteElementString("000000008");
      x12writer.WriteElementString("0");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString(">");
      x12writer.EndElement();

      x12writer.StartFunctionalGroupHeader();
      x12writer.WriteElementString("PR");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("20160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("8");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString("004010");
      x12writer.EndElement();

      x12writer.StartTransactionHeader("855");
      x12writer.WriteElementString("855");
      x12writer.WriteElementString("0013");
      x12writer.EndElement();

      x12writer.StartSegment("BAK");
      x12writer.WriteElementString("00");
      x12writer.WriteElementString("AT");
      x12writer.WriteElementString("0476553696");
      x12writer.WriteElementString("20150708");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("4900043704");
      x12writer.WriteElementString("20150708");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/PO1");
      x12writer.WriteElementString("000010");
      x12writer.WriteElementString("1100");
      x12writer.WriteElementString("EA");
      x12writer.WriteElementString("14.00");
      x12writer.SkipElement();
      x12writer.WriteElementString("BP");
      x12writer.WriteElementString("335S0548");
      x12writer.WriteElementString("VP");
      x12writer.WriteElementString("Product");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/REF");
      x12writer.WriteElementString("PO");
      x12writer.WriteElementString("0476553696");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/REF");
      x12writer.WriteElementString("VN");
      x12writer.WriteElementString("0025009879");
      x12writer.EndElement();

      x12writer.StartSegment("PO1Loop1/ACKLoop1/ACK");
      x12writer.WriteElementString("IA");
      x12writer.WriteElementString("1100");
      x12writer.WriteElementString("EA");
      x12writer.WriteElementString("067");
      x12writer.WriteElementString("20150709");
      x12writer.EndElement();

      x12writer.StartSegment("CTTLoop1/CTT");
      x12writer.WriteElementString("1");
      x12writer.WriteElementString("1100");
      x12writer.EndElement();

      x12writer.CreateTransactionFooter();

      x12writer.CreateFunctionalGroupFooter();

      x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_X12_856(X12Writer x12writer)
  {
    try
    {
      x12writer.StartInterchangeHeader("004010");
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("00");
      x12writer.SkipElement();
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("ZZ");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("U");
      x12writer.WriteElementString("00401");
      x12writer.WriteElementString("000000009");
      x12writer.WriteElementString("0");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString(">");
      x12writer.EndElement();

      x12writer.StartFunctionalGroupHeader();
      x12writer.WriteElementString("SH");
      x12writer.WriteElementString("ACME");
      x12writer.WriteElementString("WAYNE_TECH");
      x12writer.WriteElementString("20160707");
      x12writer.WriteElementString("1544");
      x12writer.WriteElementString("9");
      x12writer.WriteElementString("T");
      x12writer.WriteElementString("004010");
      x12writer.EndElement();

      x12writer.StartTransactionHeader("856");
      x12writer.WriteElementString("856");
      x12writer.WriteElementString("0029");
      x12writer.EndElement();

      x12writer.StartSegment("BSN");
      x12writer.WriteElementString("00");
      x12writer.WriteElementString("0403734501");
      x12writer.WriteElementString("20150708");
      x12writer.WriteElementString("162859");
      x12writer.EndElement();

      x12writer.StartSegment("DTM");
      x12writer.WriteElementString("011");
      x12writer.WriteElementString("20150708");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/HL");
      x12writer.WriteElementString("1");
      x12writer.SkipElement();
      x12writer.WriteElementString("S");
      x12writer.WriteElementString("1");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/PRF");
      x12writer.WriteElementString("0476553696");
      x12writer.SkipElement();
      x12writer.SkipElement();
      x12writer.WriteElementString("20150708");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/TD1");
      x12writer.WriteElementString("CNT90");
      x12writer.WriteElementString("0");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/TD5");
      x12writer.WriteElementString("O");
      x12writer.WriteElementString("2");
      x12writer.WriteElementString("FEDX");
      x12writer.WriteElementString("A");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/REF");
      x12writer.WriteElementString("BM");
      x12writer.WriteElementString("EDITEST403734501");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/REF");
      x12writer.WriteElementString("CR");
      x12writer.WriteElementString("4900043704");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/HL");
      x12writer.WriteElementString("2");
      x12writer.WriteElementString("1");
      x12writer.WriteElementString("O");
      x12writer.WriteElementString("1");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/N1Loop1/N1");
      x12writer.WriteElementString("ST");
      x12writer.WriteElementString("Name");
      x12writer.WriteElementString("92");
      x12writer.WriteElementString("0042001808");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/N1Loop1/N1");
      x12writer.WriteElementString("SF");
      x12writer.WriteElementString("NameT");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/N1Loop1/N3");
      x12writer.WriteElementString("Address");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/N1Loop1/N4");
      x12writer.WriteElementString("City");
      x12writer.WriteElementString("SG");
      x12writer.WriteElementString("339942");
      x12writer.WriteElementString("SG");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/HL");
      x12writer.WriteElementString("3");
      x12writer.WriteElementString("2");
      x12writer.WriteElementString("I");
      x12writer.WriteElementString("0");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/LIN");
      x12writer.WriteElementString("10");
      x12writer.WriteElementString("BP");
      x12writer.WriteElementString("335S0548");
      x12writer.WriteElementString("VP");
      x12writer.WriteElementString("Product");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/SN1");
      x12writer.WriteElementString("10");
      x12writer.WriteElementString("1100");
      x12writer.WriteElementString("EA");
      x12writer.EndElement();

      x12writer.StartSegment("HLLoop1/MAN");
      x12writer.WriteElementString("CP");
      x12writer.WriteElementString("Marks");
      x12writer.EndElement();

      x12writer.StartSegment("CTT");
      x12writer.WriteElementString("1");
      x12writer.EndElement();

      x12writer.CreateTransactionFooter();

      x12writer.CreateFunctionalGroupFooter();

      x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
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