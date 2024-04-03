/*
 * IPWorks EDI Translator 2022 C++ Edition - Sample Project
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
#include "../../include/ipworksEDITranslator.h"

void showCorrectCmds()
{
	printf("Usage: x12writer <EDI Standard ['X12']> <Document Type- X12: ['810','850','855','856'] \n");
	printf("examples: x12writer X12 810\n");
	printf("          x12writer X12 850\n");
	printf("Press any key to continue...");
	getchar();
	exit(0);
}

void writeFile_X12_810(X12Writer& x12Writer)
{

	x12Writer.StartInterchangeHeader("004010");
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("U");
	x12Writer.WriteElementString("00401");
	x12Writer.WriteElementString("000000006");
	x12Writer.WriteElementString("0");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString(">");
	x12Writer.EndElement();

	x12Writer.StartFunctionalGroupHeader();
	x12Writer.WriteElementString("IN");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("20160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("6");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString("004010");
	x12Writer.EndElement();

	x12Writer.StartTransactionHeader("810");
	x12Writer.WriteElementString("810");
	x12Writer.WriteElementString("0001");
	x12Writer.EndElement();

	x12Writer.StartSegment("BIG");
	x12Writer.WriteElementString("20150708");
	x12Writer.WriteElementString("3003014445");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("0476553272");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("DR");
	x12Writer.EndElement();

	x12Writer.StartSegment("CUR");
	x12Writer.WriteElementString("SE");
	x12Writer.WriteElementString("USD");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("8M");
	x12Writer.WriteElementString("0056");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("BY");
	x12Writer.WriteElementString("Company");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("544380");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N3");
	x12Writer.WriteElementString("Address");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N4");
	x12Writer.WriteElementString("City");
	x12Writer.WriteElementString("CA");
	x12Writer.WriteElementString("Postal Code");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("ST");
	x12Writer.WriteElementString("Name");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("0607047800010");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N3");
	x12Writer.WriteElementString("Address");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N4");
	x12Writer.WriteElementString("City");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("200131");
	x12Writer.WriteElementString("Country");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("RE");
	x12Writer.WriteElementString("Name");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("5095956");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N3");
	x12Writer.WriteElementString("Address");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N4");
	x12Writer.WriteElementString("City");
	x12Writer.WriteElementString("IL");
	x12Writer.WriteElementString("Postal Code");
	x12Writer.EndElement();

	x12Writer.StartSegment("IT1Loop1/IT1");
	x12Writer.WriteElementString("20");
	x12Writer.WriteElementString("2500");
	x12Writer.WriteElementString("EA");
	x12Writer.WriteElementString("36.96");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("BP");
	x12Writer.WriteElementString("335S0594");
	x12Writer.EndElement();

	x12Writer.StartSegment("IT1Loop1/REF_3");
	x12Writer.WriteElementString("KK");
	x12Writer.WriteElementString("0099778154");
	x12Writer.EndElement();

	x12Writer.StartSegment("IT1Loop1/REF_3");
	x12Writer.WriteElementString("PO");
	x12Writer.WriteElementString("0476553272");
	x12Writer.WriteElementString("20");
	x12Writer.EndElement();

	x12Writer.StartSegment("TDS");
	x12Writer.WriteElementString("9240000");
	x12Writer.EndElement();

	x12Writer.StartSegment("CTT");
	x12Writer.WriteElementString("1");
	x12Writer.EndElement();

	x12Writer.CreateTransactionFooter();
	x12Writer.CreateFunctionalGroupFooter();
	x12Writer.CreateInterchangeFooter();
}

void writeFile_X12_850(X12Writer& x12Writer)
{
	x12Writer.StartInterchangeHeader("004010");
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("U");
	x12Writer.WriteElementString("00401");
	x12Writer.WriteElementString("000000007");
	x12Writer.WriteElementString("0");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString(">");
	x12Writer.EndElement();

	x12Writer.StartFunctionalGroupHeader();
	x12Writer.WriteElementString("PO");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("20160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("7");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString("004010");
	x12Writer.EndElement();

	x12Writer.StartTransactionHeader("850");
	x12Writer.WriteElementString("850");
	x12Writer.WriteElementString("0001");
	x12Writer.EndElement();

	x12Writer.StartSegment("BEG");
	x12Writer.WriteElementString("00");
	x12Writer.WriteElementString("DS");
	x12Writer.WriteElementString("0476696888");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("20150708");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("SB");
	x12Writer.WriteElementString("ZZ11");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("6P");
	x12Writer.WriteElementString("ZZ");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("8M");
	x12Writer.WriteElementString("0056");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("CR");
	x12Writer.WriteElementString("1070335099");
	x12Writer.EndElement();

	x12Writer.StartSegment("REF");
	x12Writer.WriteElementString("CO");
	x12Writer.WriteElementString("7109790082");
	x12Writer.EndElement();

	x12Writer.StartSegment("PER");
	x12Writer.WriteElementString("CN");
	x12Writer.WriteElementString("name");
	x12Writer.WriteElementString("TE");
	x12Writer.WriteElementString("Number");

	x12Writer.StartSegment("CSH");
	x12Writer.WriteElementString("BK");
	x12Writer.EndElement();

	x12Writer.StartSegment("SACLoop1/SAC");
	x12Writer.WriteElementString("C");
	x12Writer.WriteElementString("ZZZZ");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("06");
	x12Writer.EndElement();

	x12Writer.StartSegment("TD5");
	x12Writer.WriteElementString("Z");
	x12Writer.WriteElementString("2");
	x12Writer.WriteElementString("Code");
	x12Writer.EndElement();

	x12Writer.StartSegment("N9Loop1/N9");
	x12Writer.WriteElementString("PD");
	x12Writer.WriteElementString("ZCOF");
	x12Writer.EndElement();

	x12Writer.StartSegment("N9Loop1/MSG");
	x12Writer.WriteElementString("Thanks!");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("BY");
	x12Writer.WriteElementString("Name");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("5601");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("EN");
	x12Writer.WriteElementString("Name");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N1");
	x12Writer.WriteElementString("ST");
	x12Writer.WriteElementString("OEM NAME");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("0000505462");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N2");
	x12Writer.WriteElementString("additional name");
	x12Writer.WriteElementString(""); // not skipped because last element
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N3");
	x12Writer.WriteElementString("Address");
	x12Writer.WriteElementString("Address");
	x12Writer.EndElement();

	x12Writer.StartSegment("N1Loop1/N4");
	x12Writer.WriteElementString("City");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("201613");
	x12Writer.WriteElementString("CN");
	x12Writer.WriteElementString("SP");
	x12Writer.WriteElementString("020");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/PO1");
	x12Writer.WriteElementString("00010");
	x12Writer.WriteElementString("500000");
	x12Writer.WriteElementString("EA");
	x12Writer.WriteElementString("495");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("BP");
	x12Writer.WriteElementString("337S3744");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/PIDLoop1/PID_2");
	x12Writer.WriteElementString("F");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("Thanks!");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("EN");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/REF_7");
	x12Writer.WriteElementString("CO");
	x12Writer.WriteElementString("7109790082");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/REF_7");
	x12Writer.WriteElementString("LI");
	x12Writer.WriteElementString("000010");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/SCHLoop1/SCH");
	x12Writer.WriteElementString("500000");
	x12Writer.WriteElementString("EA");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("002");
	x12Writer.WriteElementString("20180708");
	x12Writer.EndElement();

	x12Writer.StartSegment("CTTLoop1/CTT");
	x12Writer.WriteElementString("1");
	x12Writer.WriteElementString("500000");
	x12Writer.EndElement();

	x12Writer.CreateTransactionFooter();
	x12Writer.CreateFunctionalGroupFooter();
	x12Writer.CreateInterchangeFooter();
}

void writeFile_X12_855(X12Writer& x12Writer)
{
	x12Writer.StartInterchangeHeader("004010");
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("U");
	x12Writer.WriteElementString("00401");
	x12Writer.WriteElementString("000000008");
	x12Writer.WriteElementString("0");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString(">");
	x12Writer.EndElement();

	x12Writer.StartFunctionalGroupHeader();
	x12Writer.WriteElementString("PR");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("20160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("8");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString("004010");
	x12Writer.EndElement();

	x12Writer.StartTransactionHeader("855");
	x12Writer.WriteElementString("855");
	x12Writer.WriteElementString("0013");
	x12Writer.EndElement();

	x12Writer.StartSegment("BAK");
	x12Writer.WriteElementString("00");
	x12Writer.WriteElementString("AT");
	x12Writer.WriteElementString("0476553696");
	x12Writer.WriteElementString("20150708");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("4900043704");
	x12Writer.WriteElementString("20150708");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/PO1");
	x12Writer.WriteElementString("000010");
	x12Writer.WriteElementString("1100");
	x12Writer.WriteElementString("EA");
	x12Writer.WriteElementString("14.00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("BP");
	x12Writer.WriteElementString("335S0548");
	x12Writer.WriteElementString("VP");
	x12Writer.WriteElementString("Product");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/REF");
	x12Writer.WriteElementString("PO");
	x12Writer.WriteElementString("0476553696");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/REF");
	x12Writer.WriteElementString("VN");
	x12Writer.WriteElementString("0025009879");
	x12Writer.EndElement();

	x12Writer.StartSegment("PO1Loop1/ACKLoop1/ACK");
	x12Writer.WriteElementString("IA");
	x12Writer.WriteElementString("1100");
	x12Writer.WriteElementString("EA");
	x12Writer.WriteElementString("067");
	x12Writer.WriteElementString("20150709");
	x12Writer.EndElement();

	x12Writer.StartSegment("CTTLoop1/CTT");
	x12Writer.WriteElementString("1");
	x12Writer.WriteElementString("1100");
	x12Writer.EndElement();

	x12Writer.CreateTransactionFooter();
	x12Writer.CreateFunctionalGroupFooter();
	x12Writer.CreateInterchangeFooter();
}

void writeFile_X12_856(X12Writer& x12Writer) {
	x12Writer.StartInterchangeHeader("004010");
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("00");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("ZZ");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("U");
	x12Writer.WriteElementString("00401");
	x12Writer.WriteElementString("000000009");
	x12Writer.WriteElementString("0");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString(">");
	x12Writer.EndElement();

	x12Writer.StartFunctionalGroupHeader();
	x12Writer.WriteElementString("SH");
	x12Writer.WriteElementString("ACME");
	x12Writer.WriteElementString("WAYNE_TECH");
	x12Writer.WriteElementString("20160707");
	x12Writer.WriteElementString("1544");
	x12Writer.WriteElementString("9");
	x12Writer.WriteElementString("T");
	x12Writer.WriteElementString("004010");
	x12Writer.EndElement();

	x12Writer.StartTransactionHeader("856");
	x12Writer.WriteElementString("856");
	x12Writer.WriteElementString("0029");
	x12Writer.EndElement();

	x12Writer.StartSegment("BSN");
	x12Writer.WriteElementString("00");
	x12Writer.WriteElementString("0403734501");
	x12Writer.WriteElementString("20150708");
	x12Writer.WriteElementString("162859");
	x12Writer.EndElement();

	x12Writer.StartSegment("DTM");
	x12Writer.WriteElementString("011");
	x12Writer.WriteElementString("20150708");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/HL");
	x12Writer.WriteElementString("1");
	x12Writer.SkipElement();
	x12Writer.WriteElementString("S");
	x12Writer.WriteElementString("1");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/PRF");
	x12Writer.WriteElementString("0476553696");
	x12Writer.SkipElement();
	x12Writer.SkipElement();
	x12Writer.WriteElementString("20150708");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/TD1");
	x12Writer.WriteElementString("CNT90");
	x12Writer.WriteElementString("0");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/TD5");
	x12Writer.WriteElementString("O");
	x12Writer.WriteElementString("2");
	x12Writer.WriteElementString("FEDX");
	x12Writer.WriteElementString("A");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/REF");
	x12Writer.WriteElementString("BM");
	x12Writer.WriteElementString("EDITEST403734501");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/REF");
	x12Writer.WriteElementString("CR");
	x12Writer.WriteElementString("4900043704");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/HL");
	x12Writer.WriteElementString("2");
	x12Writer.WriteElementString("1");
	x12Writer.WriteElementString("O");
	x12Writer.WriteElementString("1");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/N1Loop1/N1");
	x12Writer.WriteElementString("ST");
	x12Writer.WriteElementString("Name");
	x12Writer.WriteElementString("92");
	x12Writer.WriteElementString("0042001808");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/N1Loop1/N1");
	x12Writer.WriteElementString("SF");
	x12Writer.WriteElementString("NameT");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/N1Loop1/N3");
	x12Writer.WriteElementString("Address");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/N1Loop1/N4");
	x12Writer.WriteElementString("City");
	x12Writer.WriteElementString("SG");
	x12Writer.WriteElementString("339942");
	x12Writer.WriteElementString("SG");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/HL");
	x12Writer.WriteElementString("3");
	x12Writer.WriteElementString("2");
	x12Writer.WriteElementString("I");
	x12Writer.WriteElementString("0");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/LIN");
	x12Writer.WriteElementString("10");
	x12Writer.WriteElementString("BP");
	x12Writer.WriteElementString("335S0548");
	x12Writer.WriteElementString("VP");
	x12Writer.WriteElementString("Product");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/SN1");
	x12Writer.WriteElementString("10");
	x12Writer.WriteElementString("1100");
	x12Writer.WriteElementString("EA");
	x12Writer.EndElement();

	x12Writer.StartSegment("HLLoop1/MAN");
	x12Writer.WriteElementString("CP");
	x12Writer.WriteElementString("Marks");
	x12Writer.EndElement();

	x12Writer.StartSegment("CTT");
	x12Writer.WriteElementString("1");
	x12Writer.EndElement();

	x12Writer.CreateTransactionFooter();
	x12Writer.CreateFunctionalGroupFooter();
	x12Writer.CreateInterchangeFooter();
}

int main(int argc, char* argv[])
{
	X12Writer x12Writer;

	if (argc == 3)
	{
		if (strcmp(argv[2], "810") == 0 || strcmp(argv[2], "850") == 0 || strcmp(argv[2], "855") == 0 || strcmp(argv[2], "856") == 0) //Entered valid document type for X12 standard
		{
			//Load proper X12 schema for document type and set X12 standard

			char schema[50];
			strcpy(schema, "./RSSBus_00401_");
			strcat(schema, argv[2]);
			strcat(schema, ".json");
			int ret_code = x12Writer.LoadSchema(schema);
			if (ret_code) {
				printf("ERROR: %i:%s\n", ret_code, x12Writer.GetLastError());
				printf("Press any key to continue...");
				getchar();
			}

		}
		else //Entered invalid document type for X12 standard
		{
			showCorrectCmds();
		}
	}
	else { //Entered invalid number of parameters
		showCorrectCmds();
	}

	x12Writer.Config("Encoding=iso-8859-1");
	x12Writer.SetSuffix(SUFFIX_CRLF);

	//Write generated EDI data to OutputData
	//To write to file instead, use
	//x12Writer.SetOutputFile("filename.txt");
	printf("Generating EDI document and printing to console:\n\n");
	if (strcmp(argv[2], "810") == 0) {
		writeFile_X12_810(x12Writer);
	}
	else if (strcmp(argv[2], "850") == 0) {
		writeFile_X12_850(x12Writer);
	}
	else if (strcmp(argv[2], "855") == 0) {
		writeFile_X12_855(x12Writer);
	}
	else if (strcmp(argv[2], "856") == 0) {
		writeFile_X12_856(x12Writer);
	}
	else {
		printf("Unknown schema type %s.", argv[2]);
	}

	//Get output data and print
	char* outputData;
	int outputDataLen;
	x12Writer.GetOutputData(outputData, outputDataLen);
	printf(outputData);
	printf("\n\n");

	printf("Press any key to continue...");
	getchar();
	return 0;
}



