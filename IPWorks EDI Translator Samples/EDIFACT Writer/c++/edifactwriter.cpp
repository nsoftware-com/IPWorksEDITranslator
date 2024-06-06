/*
 * IPWorks EDI Translator 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <cstring>
#include "../../include/ipworkseditranslator.h"

void showCorrectCmds()
{
	printf("Usage: edifactwriter <Document Type ['DESADV','INVOIC','ORDERS','ORDRSP']> \n");
	printf("Examples: edifactwriter DESADV\n");
	printf("          edifactwriter INVOIC\n");
	printf("Press any key to continue...");
	getchar();
	exit(0);
}

void writeFile_EDIFACT_DESADV(EDIFACTWriter &edifactWriter)
{
	edifactWriter.StartInterchangeHeader("D97A");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("UNOB");
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("WAYNE_TECH");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ACME");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("160707");
	edifactWriter.WriteComponentString("1547");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("000000001");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("1234");
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartTransactionHeader("DESADV");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("DESADV");
	edifactWriter.WriteComponentString("D");
	edifactWriter.WriteComponentString("97A");
	edifactWriter.WriteComponentString("UN");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("BGM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("351");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2014/10093");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("9");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("137");
	edifactWriter.WriteComponentString("201404192036");
	edifactWriter.WriteComponentString("203");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("201404192036");
	edifactWriter.WriteComponentString("203");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MEA");
	edifactWriter.WriteElementString("AAX");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("SQ");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("C62");
	edifactWriter.WriteComponentString("17");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("ST");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0018");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("SU");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2019813");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TDTLoop1/TDT");
	edifactWriter.WriteElementString("12");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("M");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("CARRIER");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("86");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("EQDLoop1/EQD");
	edifactWriter.WriteElementString("TE");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("X");
	edifactWriter.EndElement();
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/CPS");
	edifactWriter.WriteElementString("1");
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/PACLoop1/PAC");
	edifactWriter.WriteElementString("4");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("BOX-001");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/PACLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("52");
	edifactWriter.WriteComponentString("50");
	edifactWriter.WriteComponentString("C62");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/CPS");
	edifactWriter.WriteElementString("2");
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/PACLoop1/PAC");
	edifactWriter.WriteElementString("2");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("BOX-002");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/PACLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("52");
	edifactWriter.WriteComponentString("100");
	edifactWriter.WriteComponentString("C62");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/LINLoop1/LIN");
	edifactWriter.WriteElementString("1");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("9001");
	edifactWriter.WriteComponentString("IN");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CPSLoop1/LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("12");
	edifactWriter.WriteComponentString("400");
	edifactWriter.WriteComponentString("C62");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("RFFLoop1/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ON");
	edifactWriter.WriteComponentString("N55109001");
	edifactWriter.EndElement();

	edifactWriter.CreateTransactionFooter();
	edifactWriter.CreateInterchangeFooter();

}

void writeFile_EDIFACT_INVOIC(EDIFACTWriter &edifactWriter)
{
	edifactWriter.StartInterchangeHeader("D97A");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("UNOB");
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("WAYNE_TECH");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ACME");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("160707");
	edifactWriter.WriteComponentString("1547");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("000000002");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("1234");
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartTransactionHeader("INVOIC");
	edifactWriter.WriteElementString("509010117");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INVOIC");
	edifactWriter.WriteComponentString("D");
	edifactWriter.WriteComponentString("97A");
	edifactWriter.WriteComponentString("UN");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("BGM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("380");
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("TAX INVOICE");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0013550417");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("9");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("3");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("4");
	edifactWriter.WriteComponentString("20061123");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("FTX");
	edifactWriter.WriteElementString("AAI");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("10072.14");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CUXLoop1/CUX");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("EUR");
	edifactWriter.WriteComponentString("4");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("0.67529");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("PATLoop1/PAT");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("PATLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("10");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("PATLoop1/PCD");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("0");
	edifactWriter.WriteComponentString("13");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000030");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2265S13");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5029766832002");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("50.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("19150.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("383.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("19150.45");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.45");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.45");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000040");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2269F22");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5051254078241");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("20.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("21060.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("1053.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("21060.50");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.50");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.50");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000170");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2269F10");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5051254078326");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("10.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("6950.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("695.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("6950.16");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.16");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.16");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000190");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2269F26");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5051254051190");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("5.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("2375.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("475.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("2375.06");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.06");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.06");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000200");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2265S24");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5029766000685");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("3.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("957.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("319.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("957.02");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.02");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.02");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000210");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2263T95");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5029766699575");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("3.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("2085.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("695.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("2085.05");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.05");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.05");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("000250");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2269F15");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("5051254080091");
	edifactWriter.WriteComponentString("UP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.WriteElementString("");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("47");
	edifactWriter.WriteComponentString("3.000");
	edifactWriter.WriteComponentString("EA");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("11");
	edifactWriter.WriteComponentString("20070926");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("203");
	edifactWriter.WriteComponentString("4977.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INV");
	edifactWriter.WriteComponentString("1659.00");
	edifactWriter.WriteComponentString("TU");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("4977.12");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0.12");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("0.12");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("UNS");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CNT");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("4");
	edifactWriter.WriteComponentString("7");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("9");
	edifactWriter.WriteComponentString("67627.50");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("MOALoop4/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("79");
	edifactWriter.WriteComponentString("57554.00");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/TAX");
	edifactWriter.WriteElementString("7");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VAT");
	edifactWriter.EndElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("17.500");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("125");
	edifactWriter.WriteComponentString("57555.36");
	edifactWriter.WriteComponentString("EUR");
	edifactWriter.WriteComponentString("3");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("124");
	edifactWriter.WriteComponentString("10072.14");
	edifactWriter.WriteComponentString("EUR");
	edifactWriter.WriteComponentString("3");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("ALCLoop1/ALC");
	edifactWriter.WriteElementString("C");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("1.36");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FC");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("TAXLoop1/MOA");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("8");
	edifactWriter.WriteComponentString("1.36");
	edifactWriter.EndElement();

	edifactWriter.CreateTransactionFooter();
	edifactWriter.CreateInterchangeFooter();

}

void writeFile_EDIFACT_ORDERS(EDIFACTWriter &edifactWriter)
{
	edifactWriter.StartInterchangeHeader("D97A");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("UNOB");
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("WAYNE_TECH");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ACME");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("160707");
	edifactWriter.WriteComponentString("1547");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("000000003");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("1234");
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartTransactionHeader("ORDERS");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ORDERS");
	edifactWriter.WriteComponentString("D");
	edifactWriter.WriteComponentString("97A");
	edifactWriter.WriteComponentString("UN");
	edifactWriter.WriteComponentString("ED17A1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("BGM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("105");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("K12345");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("9");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("137");
	edifactWriter.WriteComponentString("19980626");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("FTX");
	edifactWriter.WriteElementString("GEN");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("FREE TEXT");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("RFFLoop1/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("PS");
	edifactWriter.WriteComponentString("10501");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("RFFLoop1/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("CT");
	edifactWriter.WriteComponentString("NO");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("BY");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/RFFLoop2/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("VA");
	edifactWriter.WriteComponentString("GB107328000");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("SE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CUXLoop1/CUX");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("GBP");
	edifactWriter.WriteComponentString("9");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("001");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0000057G3454");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("DESCRIPTION");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INF");
	edifactWriter.WriteComponentString("27.54");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("9829");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("JB");
	edifactWriter.WriteComponentString("JOB NO");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("19980717");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("002");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0000057G3454");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("DESCRIPTION");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("4000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INF");
	edifactWriter.WriteComponentString("27.54");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("9830");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("JB");
	edifactWriter.WriteComponentString("JOB NO");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("4000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("19980724");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("003");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("0000057G3454");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/IMD");
	edifactWriter.WriteElementString("F");
	edifactWriter.SkipElement();
	edifactWriter.StartElement();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("DESCRIPTION");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("3000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("INF");
	edifactWriter.WriteComponentString("27.54");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("9831");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("JB");
	edifactWriter.WriteComponentString("JOB NO");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("21");
	edifactWriter.WriteComponentString("3000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("19980731");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("UNS");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.CreateTransactionFooter();
	edifactWriter.CreateInterchangeFooter();

}

void writeFile_EDIFACT_ORDRSP(EDIFACTWriter &edifactWriter)
{
	edifactWriter.StartInterchangeHeader("D97A");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("UNOB");
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("WAYNE_TECH");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ACME");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("160707");
	edifactWriter.WriteComponentString("1547");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("000000004");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("1234");
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.SkipElement();
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartTransactionHeader("ORDRSP");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ORDRSP");
	edifactWriter.WriteComponentString("D");
	edifactWriter.WriteComponentString("97A");
	edifactWriter.WriteComponentString("UN");
	edifactWriter.WriteComponentString("EDOR04");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("BGM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("231");
	edifactWriter.EndElement();
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("1109706372/3");
	edifactWriter.EndElement();
	edifactWriter.WriteElementString("9");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("137");
	edifactWriter.WriteComponentString("20150708");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("RFFLoop1/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("ON");
	edifactWriter.WriteComponentString("INCG14040002");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("SE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("NADLoop1/NAD");
	edifactWriter.WriteElementString("BY");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("CUXLoop1/CUX");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("USD");
	edifactWriter.WriteComponentString("4");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("1");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("800");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.9600");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("800");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20140401");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150729");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("2");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.9600");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("2");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20141020");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150729");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("3");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRFS4115PBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.9600");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("3");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20141120");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150809");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("4");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("4000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.1000");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("4");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("4000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20140605");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150810");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("5");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("12000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.1000");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("5");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("12000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20140705");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150801");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/LIN");
	edifactWriter.WriteElementString("6");
	edifactWriter.WriteElementString("6");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("VP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("91");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PIA");
	edifactWriter.WriteElementString("1");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("IRLR8259TRPBF");
	edifactWriter.WriteComponentString("BP");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("92");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("12000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/PRILoop1/PRI");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("AAA");
	edifactWriter.WriteComponentString("0.1000");
	edifactWriter.WriteComponentString("CT");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("1");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("LINLoop1/RFFLoop3/RFF");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("LI");
	edifactWriter.SkipComponent();
	edifactWriter.WriteComponentString("6");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("10000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20140805");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150805");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/SCC");
	edifactWriter.WriteElementString("1");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/QTY");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("113");
	edifactWriter.WriteComponentString("2000");
	edifactWriter.WriteComponentString("PCE");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("2");
	edifactWriter.WriteComponentString("20140805");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("SCCLoop1/QTYLoop1/DTM");
	edifactWriter.StartElement();
	edifactWriter.WriteComponentString("67");
	edifactWriter.WriteComponentString("20150815");
	edifactWriter.WriteComponentString("102");
	edifactWriter.EndElement();

	edifactWriter.StartSegment("UNS");
	edifactWriter.WriteElementString("S");
	edifactWriter.EndElement();

	edifactWriter.CreateTransactionFooter();
	edifactWriter.CreateInterchangeFooter();

}

int main(int argc, char *argv[])
{
	EDIFACTWriter edifactWriter;
	
	if (argc == 2)
	{
		if (strcmp(argv[1], "DESADV") == 0 || strcmp(argv[1], "INVOIC") == 0 || strcmp(argv[1], "ORDERS") == 0 || strcmp(argv[1], "ORDRSP") == 0) //Entered valid document type for EDIFACT standard
		{
			//Load proper EDIFACT schema for document type and set EDIFACT standard

			char schema[50];
			strcpy(schema, "./RSSBus_D97A_");
			strcat(schema, argv[1]);
			strcat(schema, ".json");
			int ret_code = edifactWriter.LoadSchema(schema);
			if (ret_code) {
				printf("ERROR: %i:%s\n", ret_code, edifactWriter.GetLastError());
				printf("Press any key to continue...");
	getchar();
			}
		}
		else //Entered invalid document type for EDIFACT standard
		{
			showCorrectCmds();
		}
	}
	else { //Entered invalid number of parameters
		showCorrectCmds();
	}

	edifactWriter.Config("Encoding=iso-8859-1");
	edifactWriter.SetSuffix(SUFFIX_CRLF);

	//Write generated EDI data to OutputData
	//To write to file instead, use
	//edifactWriter.SetOutputFile("filename.txt");
	printf("Generating EDI document and printing to console:\n\n");
	if (strcmp(argv[1], "DESADV") == 0) {
		writeFile_EDIFACT_DESADV(edifactWriter);
	}
	else if (strcmp(argv[1], "INVOIC") == 0) {
		writeFile_EDIFACT_INVOIC(edifactWriter);
	}
	else if (strcmp(argv[1], "ORDERS") == 0) {
		writeFile_EDIFACT_ORDERS(edifactWriter);
	}
	else if (strcmp(argv[1], "ORDRSP") == 0) {
		writeFile_EDIFACT_ORDRSP(edifactWriter);
	}

	//Get output data and print
	char* outputData;
	int outputDataLen;
	edifactWriter.GetOutputData(outputData, outputDataLen);
	printf(outputData);
	printf("\n\n");

	printf("Press any key to continue...");
	getchar();
	return 0;
}














