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
#include "../../include/ipworkseditranslator.h"
#define LINE_LEN 100

class MyEDIFACTParser : public EDIFACTReader
{

	virtual int FireEndFunctionalGroup(EDIFACTReaderEndFunctionalGroupEventParams *e) {
		printf("EndFunctionalGroup: %s\n", e->Tag);
		return 0;
	}
	virtual int FireEndInterchange(EDIFACTReaderEndInterchangeEventParams *e) {
		printf("FireEndInterchange: %s\n", e->Tag);
		return 0;
	}
	virtual int FireEndLoop(EDIFACTReaderEndLoopEventParams *e) {
		printf("EndLoop\n");
		return 0;
	}
	virtual int FireEndTransaction(EDIFACTReaderEndTransactionEventParams *e) {
		printf("FireEndTransaction: %s\n", e->Tag);
		return 0;
	}
	virtual int FireError(EDIFACTReaderErrorEventParams *e) {
		printf("ERROR: %i:%sn", e->ErrorCode, e->Description);
		return 0;
	}
	virtual int FireResolveSchema(EDIFACTReaderResolveSchemaEventParams *e) {
		printf("ResolveSchema: %s\n", e->TransactionCode);
		return 0;
	}
	virtual int FireSegment(EDIFACTReaderSegmentEventParams *e) {
		printf("Segment: %s\n", e->Name);
		return 0;
	}
	virtual int FireStartFunctionalGroup(EDIFACTReaderStartFunctionalGroupEventParams *e) {
		printf("FireStartFunctionalGroup: %s\n", e->Tag);
		return 0;
	}
	virtual int FireStartInterchange(EDIFACTReaderStartFunctionalGroupEventParams *e) {
		printf("FireStartInterchange: %s\n", e->Tag);
		return 0;
	}
	virtual int FireStartLoop(EDIFACTReaderStartLoopEventParams *e) {
		printf("StartLoop: %s\n", e->Name);
		return 0;
	}
	virtual int FireStartTransaction(EDIFACTReaderStartTransactionEventParams *e) {
		printf("FireStartTransaction: %s\n", e->Tag);
		return 0;
	}
	virtual int FireWarning(EDIFACTReaderWarningEventParams *e) {
		printf("WARNING: %i:%s\n", e->WarnCode, e->Message);
		return 0;
	}
};

int main()
{
	printf("This demo uses a test Invoice document installed with the demo. To parse your own files modify the demo code to specify the document and schema.\n");
	printf("\nPress any key to continue.");
  getchar();

	MyEDIFACTParser edifactParser;
	
	edifactParser.Config("Encoding=iso-8859-1");

	int ret_code = edifactParser.SetSchemaFormat(SCHEMA_JSON);
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, edifactParser.GetLastError());

	ret_code = edifactParser.LoadSchema("./RSSBus_D97A_INVOIC.json");
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, edifactParser.GetLastError());
	
	edifactParser.SetInputFile("INVOIC.edi");
	ret_code = edifactParser.Parse();
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, edifactParser.GetLastError());

	//example XPath statements
	printf("\n\n\nExample XPath statements output:\n\n");
	edifactParser.SetXPath("/IX");
	if (edifactParser.GetElementCount() > 0) {
		//first element of root info
		printf("First element of root: Name='%s'; Value='%s'\n", edifactParser.GetElementName(0), edifactParser.GetElementValue(0));
	}
	if (edifactParser.GetXChildren() > 0) {
		edifactParser.SetXPath(strcat(edifactParser.GetXPath(), "/[1]"));
		//first child of root info
		printf("First child of root: Segment='%s'; Tag='%s'; Value='%s'\n", edifactParser.GetXPath(), edifactParser.GetXSegment(), edifactParser.GetXTag());
	}

	printf("\nPress any key to continue.");
  getchar();
	return 0;
}




