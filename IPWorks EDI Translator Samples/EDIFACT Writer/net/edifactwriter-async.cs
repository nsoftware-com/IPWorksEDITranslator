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

class edifactwriterDemo
{
  private static Edifactwriter edifactwriter = new nsoftware.async.IPWorksEDITranslator.Edifactwriter();

  static async Task Main(string[] args)
  {
    try
    {
      string docType = "";

      while (docType != "DESADV" && docType != "INVOIC" && docType != "ORDERS" && docType != "ORDRSP")
      {
        Console.Write("Document Type To Generate [DESADV, INVOIC, ORDERS, ORDRSP]: ");
        docType = Console.ReadLine();
      }

      await edifactwriter.Config("Encoding=iso-8859-1");
      await edifactwriter.LoadSchema("../../../RSSBus_D97A_" + docType + ".json");
      edifactwriter.Suffix = EdifactwriterSuffixes.suffixCRLF;

      // Will print output to console.
      await edifactwriter.SetOutputStream(Console.OpenStandardOutput());

      // To save to a file, use
      // edifactwriter.OutputFile = "filename.txt";

      // To save data to a string, make sure no OutputFile or OutputStream has been set.
      // edifactwriter.OutputFile = "";
      // To get data after it has been written, use edifactwriter.OutputData

      Console.WriteLine("\nGenerating EDI document and printing to console:\n");
      // Print correct document.
      switch (docType)
      {
        case "DESADV":
          await WriteFile_EDIFACT_DESADV(edifactwriter);
          break;
        case "INVOIC":
          await WriteFile_EDIFACT_INVOIC(edifactwriter);
          break;
        case "ORDERS":
          await WriteFile_EDIFACT_ORDERS(edifactwriter);
          break;
        case "ORDRSP":
          await WriteFile_EDIFACT_ORDRSP(edifactwriter);
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

  private static async Task WriteFile_EDIFACT_DESADV(Edifactwriter edifactwriter)
  {
    try
    {
      await edifactwriter.StartInterchangeHeader("D97A");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("UNOB");
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("WAYNE_TECH");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ACME");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("160707");
      await edifactwriter.WriteComponentString("1547");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("000000001");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("1234");
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartTransactionHeader("DESADV");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("DESADV");
      await edifactwriter.WriteComponentString("D");
      await edifactwriter.WriteComponentString("97A");
      await edifactwriter.WriteComponentString("UN");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("BGM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("351");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2014/10093");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("9");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("137");
      await edifactwriter.WriteComponentString("201404192036");
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("201404192036");
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MEA");
      await edifactwriter.WriteElementString("AAX");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("SQ");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("C62");
      await edifactwriter.WriteComponentString("17");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("ST");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0018");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("SU");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2019813");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TDTLoop1/TDT");
      await edifactwriter.WriteElementString("12");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("M");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("CARRIER");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("86");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("EQDLoop1/EQD");
      await edifactwriter.WriteElementString("TE");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("X");
      await edifactwriter.EndElement();
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/CPS");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/PACLoop1/PAC");
      await edifactwriter.WriteElementString("4");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("BOX-001");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/PACLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("52");
      await edifactwriter.WriteComponentString("50");
      await edifactwriter.WriteComponentString("C62");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/CPS");
      await edifactwriter.WriteElementString("2");
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/PACLoop1/PAC");
      await edifactwriter.WriteElementString("2");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("BOX-002");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/PACLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("52");
      await edifactwriter.WriteComponentString("100");
      await edifactwriter.WriteComponentString("C62");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/LINLoop1/LIN");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("9001");
      await edifactwriter.WriteComponentString("IN");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CPSLoop1/LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("12");
      await edifactwriter.WriteComponentString("400");
      await edifactwriter.WriteComponentString("C62");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("RFFLoop1/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ON");
      await edifactwriter.WriteComponentString("N55109001");
      await edifactwriter.EndElement();

      await edifactwriter.CreateTransactionFooter();

      await edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_EDIFACT_INVOIC(Edifactwriter edifactwriter)
  {
    try
    {
      await edifactwriter.StartInterchangeHeader("D97A");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("UNOB");
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("WAYNE_TECH");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ACME");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("160707");
      await edifactwriter.WriteComponentString("1547");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("000000002");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("1234");
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartTransactionHeader("INVOIC");
      await edifactwriter.WriteElementString("509010117");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INVOIC");
      await edifactwriter.WriteComponentString("D");
      await edifactwriter.WriteComponentString("97A");
      await edifactwriter.WriteComponentString("UN");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("BGM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("380");
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("TAX INVOICE");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0013550417");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("9");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("3");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("4");
      await edifactwriter.WriteComponentString("20061123");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("FTX");
      await edifactwriter.WriteElementString("AAI");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("10072.14");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CUXLoop1/CUX");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("EUR");
      await edifactwriter.WriteComponentString("4");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("0.67529");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("PATLoop1/PAT");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("PATLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("10");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("PATLoop1/PCD");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("0");
      await edifactwriter.WriteComponentString("13");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000030");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2265S13");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5029766832002");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("50.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("19150.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("383.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("19150.45");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.45");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.45");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000040");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2269F22");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5051254078241");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("20.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("21060.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("1053.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("21060.50");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.50");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.50");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000170");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2269F10");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5051254078326");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("10.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("6950.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("695.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("6950.16");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.16");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.16");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000190");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2269F26");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5051254051190");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("5.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("2375.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("475.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("2375.06");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.06");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.06");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000200");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2265S24");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5029766000685");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("3.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("957.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("319.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("957.02");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.02");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.02");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000210");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2263T95");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5029766699575");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("3.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("2085.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("695.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("2085.05");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.05");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.05");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("000250");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2269F15");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("5051254080091");
      await edifactwriter.WriteComponentString("UP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.WriteElementString("");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("47");
      await edifactwriter.WriteComponentString("3.000");
      await edifactwriter.WriteComponentString("EA");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("11");
      await edifactwriter.WriteComponentString("20070926");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("203");
      await edifactwriter.WriteComponentString("4977.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INV");
      await edifactwriter.WriteComponentString("1659.00");
      await edifactwriter.WriteComponentString("TU");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("4977.12");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0.12");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("0.12");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("UNS");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CNT");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("4");
      await edifactwriter.WriteComponentString("7");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("9");
      await edifactwriter.WriteComponentString("67627.50");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("MOALoop4/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("79");
      await edifactwriter.WriteComponentString("57554.00");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/TAX");
      await edifactwriter.WriteElementString("7");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VAT");
      await edifactwriter.EndElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("17.500");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("125");
      await edifactwriter.WriteComponentString("57555.36");
      await edifactwriter.WriteComponentString("EUR");
      await edifactwriter.WriteComponentString("3");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("124");
      await edifactwriter.WriteComponentString("10072.14");
      await edifactwriter.WriteComponentString("EUR");
      await edifactwriter.WriteComponentString("3");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("ALCLoop1/ALC");
      await edifactwriter.WriteElementString("C");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("1.36");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FC");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("TAXLoop1/MOA");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("8");
      await edifactwriter.WriteComponentString("1.36");
      await edifactwriter.EndElement();

      await edifactwriter.CreateTransactionFooter();

      await edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_EDIFACT_ORDERS(Edifactwriter edifactwriter)
  {
    try
    {
      await edifactwriter.StartInterchangeHeader("D97A");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("UNOB");
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("WAYNE_TECH");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ACME");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("160707");
      await edifactwriter.WriteComponentString("1547");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("000000003");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("1234");
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartTransactionHeader("ORDERS");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ORDERS");
      await edifactwriter.WriteComponentString("D");
      await edifactwriter.WriteComponentString("97A");
      await edifactwriter.WriteComponentString("UN");
      await edifactwriter.WriteComponentString("ED17A1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("BGM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("105");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("K12345");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("9");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("137");
      await edifactwriter.WriteComponentString("19980626");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("FTX");
      await edifactwriter.WriteElementString("GEN");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("FREE TEXT");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("RFFLoop1/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("PS");
      await edifactwriter.WriteComponentString("10501");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("RFFLoop1/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.WriteComponentString("NO");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("BY");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/RFFLoop2/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("VA");
      await edifactwriter.WriteComponentString("GB107328000");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("SE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CUXLoop1/CUX");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("GBP");
      await edifactwriter.WriteComponentString("9");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("001");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0000057G3454");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("DESCRIPTION");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INF");
      await edifactwriter.WriteComponentString("27.54");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("9829");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("JB");
      await edifactwriter.WriteComponentString("JOB NO");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("19980717");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("002");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0000057G3454");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("DESCRIPTION");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("4000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INF");
      await edifactwriter.WriteComponentString("27.54");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("9830");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("JB");
      await edifactwriter.WriteComponentString("JOB NO");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("4000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("19980724");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("003");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("0000057G3454");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/IMD");
      await edifactwriter.WriteElementString("F");
      await edifactwriter.SkipElement();
      await edifactwriter.StartElement();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("DESCRIPTION");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("3000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("INF");
      await edifactwriter.WriteComponentString("27.54");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("9831");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("JB");
      await edifactwriter.WriteComponentString("JOB NO");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("21");
      await edifactwriter.WriteComponentString("3000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("19980731");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("UNS");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.CreateTransactionFooter();

      await edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static async Task WriteFile_EDIFACT_ORDRSP(Edifactwriter edifactwriter)
  {
    try
    {
      await edifactwriter.StartInterchangeHeader("D97A");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("UNOB");
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("WAYNE_TECH");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ACME");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("160707");
      await edifactwriter.WriteComponentString("1547");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("000000004");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("1234");
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.SkipElement();
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartTransactionHeader("ORDRSP");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ORDRSP");
      await edifactwriter.WriteComponentString("D");
      await edifactwriter.WriteComponentString("97A");
      await edifactwriter.WriteComponentString("UN");
      await edifactwriter.WriteComponentString("EDOR04");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("BGM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("231");
      await edifactwriter.EndElement();
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("1109706372/3");
      await edifactwriter.EndElement();
      await edifactwriter.WriteElementString("9");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("137");
      await edifactwriter.WriteComponentString("20150708");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("RFFLoop1/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("ON");
      await edifactwriter.WriteComponentString("INCG14040002");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("SE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("NADLoop1/NAD");
      await edifactwriter.WriteElementString("BY");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("CUXLoop1/CUX");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("USD");
      await edifactwriter.WriteComponentString("4");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("800");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.9600");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("800");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20140401");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150729");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("2");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.9600");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20141020");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150729");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("3");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRFS4115PBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.9600");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("3");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20141120");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150809");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("4");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("4000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.1000");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("4");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("4000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20140605");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150810");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("5");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("12000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.1000");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("5");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("12000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20140705");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150801");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/LIN");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.WriteElementString("6");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("VP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("91");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PIA");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("IRLR8259TRPBF");
      await edifactwriter.WriteComponentString("BP");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("92");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("12000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("AAA");
      await edifactwriter.WriteComponentString("0.1000");
      await edifactwriter.WriteComponentString("CT");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("1");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("LI");
      await edifactwriter.SkipComponent();
      await edifactwriter.WriteComponentString("6");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("10000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20140805");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150805");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/SCC");
      await edifactwriter.WriteElementString("1");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("113");
      await edifactwriter.WriteComponentString("2000");
      await edifactwriter.WriteComponentString("PCE");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("2");
      await edifactwriter.WriteComponentString("20140805");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      await edifactwriter.StartElement();
      await edifactwriter.WriteComponentString("67");
      await edifactwriter.WriteComponentString("20150815");
      await edifactwriter.WriteComponentString("102");
      await edifactwriter.EndElement();

      await edifactwriter.StartSegment("UNS");
      await edifactwriter.WriteElementString("S");
      await edifactwriter.EndElement();

      await edifactwriter.CreateTransactionFooter();

      await edifactwriter.CreateInterchangeFooter();
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