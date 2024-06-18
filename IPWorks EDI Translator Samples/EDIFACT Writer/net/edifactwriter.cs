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

class edifactwriterDemo
{
  private static EDIFACTWriter edifactwriter = new nsoftware.IPWorksEDITranslator.EDIFACTWriter();

  static void Main(string[] args)
  {
    try
    {
      string docType = "";

      while (docType != "DESADV" && docType != "INVOIC" && docType != "ORDERS" && docType != "ORDRSP")
      {
        Console.Write("Document Type To Generate [DESADV, INVOIC, ORDERS, ORDRSP]: ");
        docType = Console.ReadLine();
      }

      edifactwriter.Config("Encoding=iso-8859-1");
      edifactwriter.LoadSchema("../../../RSSBus_D97A_" + docType + ".json");
      edifactwriter.Suffix = EDIFACTWriterSuffixes.suffixCRLF;

      // Will print output to console.
      edifactwriter.SetOutputStream(Console.OpenStandardOutput());

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
          WriteFile_EDIFACT_DESADV(edifactwriter);
          break;
        case "INVOIC":
          WriteFile_EDIFACT_INVOIC(edifactwriter);
          break;
        case "ORDERS":
          WriteFile_EDIFACT_ORDERS(edifactwriter);
          break;
        case "ORDRSP":
          WriteFile_EDIFACT_ORDRSP(edifactwriter);
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

  private static void WriteFile_EDIFACT_DESADV(EDIFACTWriter edifactwriter)
  {
    try
    {
      edifactwriter.StartInterchangeHeader("D97A");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("UNOB");
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("WAYNE_TECH");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ACME");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("160707");
      edifactwriter.WriteComponentString("1547");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("000000001");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("1234");
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartTransactionHeader("DESADV");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("DESADV");
      edifactwriter.WriteComponentString("D");
      edifactwriter.WriteComponentString("97A");
      edifactwriter.WriteComponentString("UN");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("BGM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("351");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2014/10093");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("9");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("137");
      edifactwriter.WriteComponentString("201404192036");
      edifactwriter.WriteComponentString("203");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("201404192036");
      edifactwriter.WriteComponentString("203");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MEA");
      edifactwriter.WriteElementString("AAX");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("SQ");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("C62");
      edifactwriter.WriteComponentString("17");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("ST");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0018");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("SU");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2019813");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TDTLoop1/TDT");
      edifactwriter.WriteElementString("12");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("M");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("CARRIER");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("86");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("EQDLoop1/EQD");
      edifactwriter.WriteElementString("TE");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("X");
      edifactwriter.EndElement();
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/CPS");
      edifactwriter.WriteElementString("1");
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/PACLoop1/PAC");
      edifactwriter.WriteElementString("4");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("BOX-001");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/PACLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("52");
      edifactwriter.WriteComponentString("50");
      edifactwriter.WriteComponentString("C62");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/CPS");
      edifactwriter.WriteElementString("2");
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/PACLoop1/PAC");
      edifactwriter.WriteElementString("2");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("BOX-002");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/PACLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("52");
      edifactwriter.WriteComponentString("100");
      edifactwriter.WriteComponentString("C62");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/LINLoop1/LIN");
      edifactwriter.WriteElementString("1");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("9001");
      edifactwriter.WriteComponentString("IN");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CPSLoop1/LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("12");
      edifactwriter.WriteComponentString("400");
      edifactwriter.WriteComponentString("C62");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("RFFLoop1/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ON");
      edifactwriter.WriteComponentString("N55109001");
      edifactwriter.EndElement();

      edifactwriter.CreateTransactionFooter();

      edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_EDIFACT_INVOIC(EDIFACTWriter edifactwriter)
  {
    try
    {
      edifactwriter.StartInterchangeHeader("D97A");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("UNOB");
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("WAYNE_TECH");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ACME");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("160707");
      edifactwriter.WriteComponentString("1547");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("000000002");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("1234");
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartTransactionHeader("INVOIC");
      edifactwriter.WriteElementString("509010117");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INVOIC");
      edifactwriter.WriteComponentString("D");
      edifactwriter.WriteComponentString("97A");
      edifactwriter.WriteComponentString("UN");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("BGM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("380");
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("TAX INVOICE");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0013550417");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("9");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("3");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("4");
      edifactwriter.WriteComponentString("20061123");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("FTX");
      edifactwriter.WriteElementString("AAI");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("10072.14");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CUXLoop1/CUX");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("EUR");
      edifactwriter.WriteComponentString("4");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("0.67529");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("PATLoop1/PAT");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("PATLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("10");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("PATLoop1/PCD");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("0");
      edifactwriter.WriteComponentString("13");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000030");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2265S13");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5029766832002");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("50.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("19150.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("383.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("19150.45");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.45");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.45");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000040");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2269F22");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5051254078241");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("20.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("21060.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("1053.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("21060.50");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.50");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.50");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000170");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2269F10");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5051254078326");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("10.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("6950.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("695.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("6950.16");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.16");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.16");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000190");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2269F26");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5051254051190");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("5.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("2375.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("475.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("2375.06");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.06");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.06");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000200");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2265S24");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5029766000685");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("3.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("957.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("319.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("957.02");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.02");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.02");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000210");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2263T95");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5029766699575");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("3.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("2085.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("695.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("2085.05");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.05");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.05");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("000250");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2269F15");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("5051254080091");
      edifactwriter.WriteComponentString("UP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.WriteElementString("");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("47");
      edifactwriter.WriteComponentString("3.000");
      edifactwriter.WriteComponentString("EA");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("11");
      edifactwriter.WriteComponentString("20070926");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("203");
      edifactwriter.WriteComponentString("4977.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INV");
      edifactwriter.WriteComponentString("1659.00");
      edifactwriter.WriteComponentString("TU");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("4977.12");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0.12");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("0.12");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("UNS");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CNT");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("4");
      edifactwriter.WriteComponentString("7");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("9");
      edifactwriter.WriteComponentString("67627.50");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("MOALoop4/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("79");
      edifactwriter.WriteComponentString("57554.00");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/TAX");
      edifactwriter.WriteElementString("7");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VAT");
      edifactwriter.EndElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("17.500");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("125");
      edifactwriter.WriteComponentString("57555.36");
      edifactwriter.WriteComponentString("EUR");
      edifactwriter.WriteComponentString("3");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("124");
      edifactwriter.WriteComponentString("10072.14");
      edifactwriter.WriteComponentString("EUR");
      edifactwriter.WriteComponentString("3");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("ALCLoop1/ALC");
      edifactwriter.WriteElementString("C");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("1.36");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FC");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("TAXLoop1/MOA");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("8");
      edifactwriter.WriteComponentString("1.36");
      edifactwriter.EndElement();

      edifactwriter.CreateTransactionFooter();

      edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_EDIFACT_ORDERS(EDIFACTWriter edifactwriter)
  {
    try
    {
      edifactwriter.StartInterchangeHeader("D97A");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("UNOB");
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("WAYNE_TECH");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ACME");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("160707");
      edifactwriter.WriteComponentString("1547");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("000000003");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("1234");
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartTransactionHeader("ORDERS");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ORDERS");
      edifactwriter.WriteComponentString("D");
      edifactwriter.WriteComponentString("97A");
      edifactwriter.WriteComponentString("UN");
      edifactwriter.WriteComponentString("ED17A1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("BGM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("105");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("K12345");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("9");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("137");
      edifactwriter.WriteComponentString("19980626");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("FTX");
      edifactwriter.WriteElementString("GEN");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("FREE TEXT");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("RFFLoop1/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("PS");
      edifactwriter.WriteComponentString("10501");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("RFFLoop1/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("CT");
      edifactwriter.WriteComponentString("NO");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("BY");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/RFFLoop2/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("VA");
      edifactwriter.WriteComponentString("GB107328000");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("SE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CUXLoop1/CUX");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("GBP");
      edifactwriter.WriteComponentString("9");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("001");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0000057G3454");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("DESCRIPTION");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INF");
      edifactwriter.WriteComponentString("27.54");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("9829");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("JB");
      edifactwriter.WriteComponentString("JOB NO");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("19980717");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("002");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0000057G3454");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("DESCRIPTION");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("4000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INF");
      edifactwriter.WriteComponentString("27.54");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("9830");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("JB");
      edifactwriter.WriteComponentString("JOB NO");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("4000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("19980724");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("003");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("0000057G3454");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/IMD");
      edifactwriter.WriteElementString("F");
      edifactwriter.SkipElement();
      edifactwriter.StartElement();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("DESCRIPTION");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("3000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("INF");
      edifactwriter.WriteComponentString("27.54");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("9831");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("JB");
      edifactwriter.WriteComponentString("JOB NO");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("21");
      edifactwriter.WriteComponentString("3000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("19980731");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("UNS");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.CreateTransactionFooter();

      edifactwriter.CreateInterchangeFooter();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void WriteFile_EDIFACT_ORDRSP(EDIFACTWriter edifactwriter)
  {
    try
    {
      edifactwriter.StartInterchangeHeader("D97A");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("UNOB");
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("WAYNE_TECH");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ACME");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("160707");
      edifactwriter.WriteComponentString("1547");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("000000004");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("1234");
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.SkipElement();
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartTransactionHeader("ORDRSP");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ORDRSP");
      edifactwriter.WriteComponentString("D");
      edifactwriter.WriteComponentString("97A");
      edifactwriter.WriteComponentString("UN");
      edifactwriter.WriteComponentString("EDOR04");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("BGM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("231");
      edifactwriter.EndElement();
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("1109706372/3");
      edifactwriter.EndElement();
      edifactwriter.WriteElementString("9");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("137");
      edifactwriter.WriteComponentString("20150708");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("RFFLoop1/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("ON");
      edifactwriter.WriteComponentString("INCG14040002");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("SE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("NADLoop1/NAD");
      edifactwriter.WriteElementString("BY");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("CUXLoop1/CUX");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("USD");
      edifactwriter.WriteComponentString("4");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("1");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("800");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.9600");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("800");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20140401");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150729");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("2");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.9600");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("2");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20141020");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150729");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("3");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRFS4115PBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.9600");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("3");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20141120");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150809");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("4");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("4000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.1000");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("4");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("4000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20140605");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150810");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("5");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("12000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.1000");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("5");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("12000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20140705");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150801");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/LIN");
      edifactwriter.WriteElementString("6");
      edifactwriter.WriteElementString("6");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("VP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("91");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PIA");
      edifactwriter.WriteElementString("1");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("IRLR8259TRPBF");
      edifactwriter.WriteComponentString("BP");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("92");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("12000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/PRILoop1/PRI");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("AAA");
      edifactwriter.WriteComponentString("0.1000");
      edifactwriter.WriteComponentString("CT");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("1");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("LINLoop1/RFFLoop3/RFF");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("LI");
      edifactwriter.SkipComponent();
      edifactwriter.WriteComponentString("6");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("10000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20140805");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150805");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/SCC");
      edifactwriter.WriteElementString("1");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("113");
      edifactwriter.WriteComponentString("2000");
      edifactwriter.WriteComponentString("PCE");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("2");
      edifactwriter.WriteComponentString("20140805");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
      edifactwriter.StartElement();
      edifactwriter.WriteComponentString("67");
      edifactwriter.WriteComponentString("20150815");
      edifactwriter.WriteComponentString("102");
      edifactwriter.EndElement();

      edifactwriter.StartSegment("UNS");
      edifactwriter.WriteElementString("S");
      edifactwriter.EndElement();

      edifactwriter.CreateTransactionFooter();

      edifactwriter.CreateInterchangeFooter();
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