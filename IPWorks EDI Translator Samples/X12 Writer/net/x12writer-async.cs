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

class x12writerDemo
{
  private static X12writer x12writer = new nsoftware.async.IPWorksEDITranslator.X12writer();

  static async Task Main(string[] args)
  {
    try
    {
      string docType = "";

      while (docType != "810" && docType != "850" && docType != "855" && docType != "856")
      {
        Console.Write("Document Type To Generate [810, 850, 855, 856]: ");
        docType = Console.ReadLine();
      }

      await x12writer.Config("Encoding=iso-8859-1");
      await x12writer.LoadSchema("../../../RSSBus_00401_" + docType + ".json");
      x12writer.Suffix = X12writerSuffixes.suffixCRLF;

      // Will print output to console.
      await x12writer.SetOutputStream(Console.OpenStandardOutput());

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
          await WriteFile_X12_810(x12writer);
          break;
        case "850":
          await WriteFile_X12_850(x12writer);
          break;
        case "855":
          await WriteFile_X12_855(x12writer);
          break;
        case "856":
          await WriteFile_X12_856(x12writer);
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

  private static async Task WriteFile_X12_810(X12writer x12writer)
  {
    try
    {
      await x12writer.StartInterchangeHeader("004010");
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("U");
      await x12writer.WriteElementString("00401");
      await x12writer.WriteElementString("000000006");
      await x12writer.WriteElementString("0");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString(">");
      await x12writer.EndElement();

      await x12writer.StartFunctionalGroupHeader();
      await x12writer.WriteElementString("IN");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("20160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("6");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString("004010");
      await x12writer.EndElement();

      await x12writer.StartTransactionHeader("810");
      await x12writer.WriteElementString("810");
      await x12writer.WriteElementString("0001");
      await x12writer.EndElement();

      await x12writer.StartSegment("BIG");
      await x12writer.WriteElementString("20150708");
      await x12writer.WriteElementString("3003014445");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("0476553272");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("DR");
      await x12writer.EndElement();

      await x12writer.StartSegment("CUR");
      await x12writer.WriteElementString("SE");
      await x12writer.WriteElementString("USD");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("8M");
      await x12writer.WriteElementString("0056");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("BY");
      await x12writer.WriteElementString("Company");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("544380");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N3");
      await x12writer.WriteElementString("Address");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N4");
      await x12writer.WriteElementString("City");
      await x12writer.WriteElementString("CA");
      await x12writer.WriteElementString("Postal Code");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("ST");
      await x12writer.WriteElementString("Name");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("0607047800010");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N3");
      await x12writer.WriteElementString("Address");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N4");
      await x12writer.WriteElementString("City");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("200131");
      await x12writer.WriteElementString("Country");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("RE");
      await x12writer.WriteElementString("Name");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("5095956");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N3");
      await x12writer.WriteElementString("Address");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N4");
      await x12writer.WriteElementString("City");
      await x12writer.WriteElementString("IL");
      await x12writer.WriteElementString("Postal Code");
      await x12writer.EndElement();

      await x12writer.StartSegment("IT1Loop1/IT1");
      await x12writer.WriteElementString("20");
      await x12writer.WriteElementString("2500");
      await x12writer.WriteElementString("EA");
      await x12writer.WriteElementString("36.96");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("BP");
      await x12writer.WriteElementString("335S0594");
      await x12writer.EndElement();

      await x12writer.StartSegment("IT1Loop1/REF_3");
      await x12writer.WriteElementString("KK");
      await x12writer.WriteElementString("0099778154");
      await x12writer.EndElement();

      await x12writer.StartSegment("IT1Loop1/REF_3");
      await x12writer.WriteElementString("PO");
      await x12writer.WriteElementString("0476553272");
      await x12writer.WriteElementString("20");
      await x12writer.EndElement();

      await x12writer.StartSegment("TDS");
      await x12writer.WriteElementString("9240000");
      await x12writer.EndElement();

      await x12writer.StartSegment("CTT");
      await x12writer.WriteElementString("1");
      await x12writer.EndElement();

      await x12writer.CreateTransactionFooter();

      await x12writer.CreateFunctionalGroupFooter();

      await x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_X12_850(X12writer x12writer)
  {
    try
    {
      await x12writer.StartInterchangeHeader("004010");
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("U");
      await x12writer.WriteElementString("00401");
      await x12writer.WriteElementString("000000007");
      await x12writer.WriteElementString("0");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString(">");
      await x12writer.EndElement();

      await x12writer.StartFunctionalGroupHeader();
      await x12writer.WriteElementString("PO");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("20160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("7");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString("004010");
      await x12writer.EndElement();

      await x12writer.StartTransactionHeader("850");
      await x12writer.WriteElementString("850");
      await x12writer.WriteElementString("0001");
      await x12writer.EndElement();

      await x12writer.StartSegment("BEG");
      await x12writer.WriteElementString("00");
      await x12writer.WriteElementString("DS");
      await x12writer.WriteElementString("0476696888");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("20150708");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("SB");
      await x12writer.WriteElementString("ZZ11");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("6P");
      await x12writer.WriteElementString("ZZ");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("8M");
      await x12writer.WriteElementString("0056");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("CR");
      await x12writer.WriteElementString("1070335099");
      await x12writer.EndElement();

      await x12writer.StartSegment("REF");
      await x12writer.WriteElementString("CO");
      await x12writer.WriteElementString("7109790082");
      await x12writer.EndElement();

      await x12writer.StartSegment("PER");
      await x12writer.WriteElementString("CN");
      await x12writer.WriteElementString("name");
      await x12writer.WriteElementString("TE");
      await x12writer.WriteElementString("Number");

      await x12writer.StartSegment("CSH");
      await x12writer.WriteElementString("BK");
      await x12writer.EndElement();

      await x12writer.StartSegment("SACLoop1/SAC");
      await x12writer.WriteElementString("C");
      await x12writer.WriteElementString("ZZZZ");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("06");
      await x12writer.EndElement();

      await x12writer.StartSegment("TD5");
      await x12writer.WriteElementString("Z");
      await x12writer.WriteElementString("2");
      await x12writer.WriteElementString("Code");
      await x12writer.EndElement();

      await x12writer.StartSegment("N9Loop1/N9");
      await x12writer.WriteElementString("PD");
      await x12writer.WriteElementString("ZCOF");
      await x12writer.EndElement();

      await x12writer.StartSegment("N9Loop1/MSG");
      await x12writer.WriteElementString("Thanks!");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("BY");
      await x12writer.WriteElementString("Name");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("5601");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("EN");
      await x12writer.WriteElementString("Name");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N1");
      await x12writer.WriteElementString("ST");
      await x12writer.WriteElementString("OEM NAME");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("0000505462");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N2");
      await x12writer.WriteElementString("additional name");
      await x12writer.WriteElementString(""); // Not skipped because last element
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N3");
      await x12writer.WriteElementString("Address");
      await x12writer.WriteElementString("Address");
      await x12writer.EndElement();

      await x12writer.StartSegment("N1Loop1/N4");
      await x12writer.WriteElementString("City");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("201613");
      await x12writer.WriteElementString("CN");
      await x12writer.WriteElementString("SP");
      await x12writer.WriteElementString("020");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/PO1");
      await x12writer.WriteElementString("00010");
      await x12writer.WriteElementString("500000");
      await x12writer.WriteElementString("EA");
      await x12writer.WriteElementString("495");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("BP");
      await x12writer.WriteElementString("337S3744");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/PIDLoop1/PID_2");
      await x12writer.WriteElementString("F");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("Thanks!");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("EN");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/REF_7");
      await x12writer.WriteElementString("CO");
      await x12writer.WriteElementString("7109790082");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/REF_7");
      await x12writer.WriteElementString("LI");
      await x12writer.WriteElementString("000010");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/SCHLoop1/SCH");
      await x12writer.WriteElementString("500000");
      await x12writer.WriteElementString("EA");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("002");
      await x12writer.WriteElementString("20180708");
      await x12writer.EndElement();

      await x12writer.StartSegment("CTTLoop1/CTT");
      await x12writer.WriteElementString("1");
      await x12writer.WriteElementString("500000");
      await x12writer.EndElement();

      await x12writer.CreateTransactionFooter();

      await x12writer.CreateFunctionalGroupFooter();

      await x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_X12_855(X12writer x12writer)
  {
    try
    {
      await x12writer.StartInterchangeHeader("004010");
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("U");
      await x12writer.WriteElementString("00401");
      await x12writer.WriteElementString("000000008");
      await x12writer.WriteElementString("0");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString(">");
      await x12writer.EndElement();

      await x12writer.StartFunctionalGroupHeader();
      await x12writer.WriteElementString("PR");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("20160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("8");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString("004010");
      await x12writer.EndElement();

      await x12writer.StartTransactionHeader("855");
      await x12writer.WriteElementString("855");
      await x12writer.WriteElementString("0013");
      await x12writer.EndElement();

      await x12writer.StartSegment("BAK");
      await x12writer.WriteElementString("00");
      await x12writer.WriteElementString("AT");
      await x12writer.WriteElementString("0476553696");
      await x12writer.WriteElementString("20150708");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("4900043704");
      await x12writer.WriteElementString("20150708");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/PO1");
      await x12writer.WriteElementString("000010");
      await x12writer.WriteElementString("1100");
      await x12writer.WriteElementString("EA");
      await x12writer.WriteElementString("14.00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("BP");
      await x12writer.WriteElementString("335S0548");
      await x12writer.WriteElementString("VP");
      await x12writer.WriteElementString("Product");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/REF");
      await x12writer.WriteElementString("PO");
      await x12writer.WriteElementString("0476553696");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/REF");
      await x12writer.WriteElementString("VN");
      await x12writer.WriteElementString("0025009879");
      await x12writer.EndElement();

      await x12writer.StartSegment("PO1Loop1/ACKLoop1/ACK");
      await x12writer.WriteElementString("IA");
      await x12writer.WriteElementString("1100");
      await x12writer.WriteElementString("EA");
      await x12writer.WriteElementString("067");
      await x12writer.WriteElementString("20150709");
      await x12writer.EndElement();

      await x12writer.StartSegment("CTTLoop1/CTT");
      await x12writer.WriteElementString("1");
      await x12writer.WriteElementString("1100");
      await x12writer.EndElement();

      await x12writer.CreateTransactionFooter();

      await x12writer.CreateFunctionalGroupFooter();

      await x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_X12_856(X12writer x12writer)
  {
    try
    {
      await x12writer.StartInterchangeHeader("004010");
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("00");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("ZZ");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("U");
      await x12writer.WriteElementString("00401");
      await x12writer.WriteElementString("000000009");
      await x12writer.WriteElementString("0");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString(">");
      await x12writer.EndElement();

      await x12writer.StartFunctionalGroupHeader();
      await x12writer.WriteElementString("SH");
      await x12writer.WriteElementString("ACME");
      await x12writer.WriteElementString("WAYNE_TECH");
      await x12writer.WriteElementString("20160707");
      await x12writer.WriteElementString("1544");
      await x12writer.WriteElementString("9");
      await x12writer.WriteElementString("T");
      await x12writer.WriteElementString("004010");
      await x12writer.EndElement();

      await x12writer.StartTransactionHeader("856");
      await x12writer.WriteElementString("856");
      await x12writer.WriteElementString("0029");
      await x12writer.EndElement();

      await x12writer.StartSegment("BSN");
      await x12writer.WriteElementString("00");
      await x12writer.WriteElementString("0403734501");
      await x12writer.WriteElementString("20150708");
      await x12writer.WriteElementString("162859");
      await x12writer.EndElement();

      await x12writer.StartSegment("DTM");
      await x12writer.WriteElementString("011");
      await x12writer.WriteElementString("20150708");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/HL");
      await x12writer.WriteElementString("1");
      await x12writer.SkipElement();
      await x12writer.WriteElementString("S");
      await x12writer.WriteElementString("1");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/PRF");
      await x12writer.WriteElementString("0476553696");
      await x12writer.SkipElement();
      await x12writer.SkipElement();
      await x12writer.WriteElementString("20150708");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/TD1");
      await x12writer.WriteElementString("CNT90");
      await x12writer.WriteElementString("0");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/TD5");
      await x12writer.WriteElementString("O");
      await x12writer.WriteElementString("2");
      await x12writer.WriteElementString("FEDX");
      await x12writer.WriteElementString("A");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/REF");
      await x12writer.WriteElementString("BM");
      await x12writer.WriteElementString("EDITEST403734501");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/REF");
      await x12writer.WriteElementString("CR");
      await x12writer.WriteElementString("4900043704");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/HL");
      await x12writer.WriteElementString("2");
      await x12writer.WriteElementString("1");
      await x12writer.WriteElementString("O");
      await x12writer.WriteElementString("1");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/N1Loop1/N1");
      await x12writer.WriteElementString("ST");
      await x12writer.WriteElementString("Name");
      await x12writer.WriteElementString("92");
      await x12writer.WriteElementString("0042001808");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/N1Loop1/N1");
      await x12writer.WriteElementString("SF");
      await x12writer.WriteElementString("NameT");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/N1Loop1/N3");
      await x12writer.WriteElementString("Address");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/N1Loop1/N4");
      await x12writer.WriteElementString("City");
      await x12writer.WriteElementString("SG");
      await x12writer.WriteElementString("339942");
      await x12writer.WriteElementString("SG");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/HL");
      await x12writer.WriteElementString("3");
      await x12writer.WriteElementString("2");
      await x12writer.WriteElementString("I");
      await x12writer.WriteElementString("0");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/LIN");
      await x12writer.WriteElementString("10");
      await x12writer.WriteElementString("BP");
      await x12writer.WriteElementString("335S0548");
      await x12writer.WriteElementString("VP");
      await x12writer.WriteElementString("Product");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/SN1");
      await x12writer.WriteElementString("10");
      await x12writer.WriteElementString("1100");
      await x12writer.WriteElementString("EA");
      await x12writer.EndElement();

      await x12writer.StartSegment("HLLoop1/MAN");
      await x12writer.WriteElementString("CP");
      await x12writer.WriteElementString("Marks");
      await x12writer.EndElement();

      await x12writer.StartSegment("CTT");
      await x12writer.WriteElementString("1");
      await x12writer.EndElement();

      await x12writer.CreateTransactionFooter();

      await x12writer.CreateFunctionalGroupFooter();

      await x12writer.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
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