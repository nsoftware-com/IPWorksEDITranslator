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
#define LINE_LEN 100

class MyX12Reader : public X12Reader
{

	virtual int FireEndFunctionalGroup(X12ReaderEndFunctionalGroupEventParams* e) {
		printf("EndFunctionalGroup: %s\n", e->Tag);
		return 0;
	}
	virtual int FireEndInterchange(X12ReaderEndInterchangeEventParams* e) {
		printf("FireEndInterchange: %s\n", e->Tag);
		return 0;
	}
	virtual int FireEndLoop(X12ReaderEndLoopEventParams* e) {
		printf("EndLoop\n");
		return 0;
	}
	virtual int FireEndTransaction(X12ReaderEndTransactionEventParams* e) {
		printf("FireEndTransaction: %s\n", e->Tag);
		return 0;
	}
	virtual int FireError(X12ReaderErrorEventParams* e) {
		printf("ERROR: %i:%sn", e->ErrorCode, e->Description);
		return 0;
	}
	virtual int FireResolveSchema(X12ReaderResolveSchemaEventParams* e) {
		printf("ResolveSchema: %s\n", e->TransactionCode);
		return 0;
	}
	virtual int FireSegment(X12ReaderSegmentEventParams* e) {
		printf("Segment: %s\n", e->Name);
		return 0;
	}
	virtual int FireStartFunctionalGroup(X12ReaderStartFunctionalGroupEventParams* e) {
		printf("FireStartFunctionalGroup: %s\n", e->Tag);
		return 0;
	}
	virtual int FireStartInterchange(X12ReaderStartFunctionalGroupEventParams* e) {
		printf("FireStartInterchange: %s\n", e->Tag);
		return 0;
	}
	virtual int FireStartLoop(X12ReaderStartLoopEventParams* e) {
		printf("StartLoop: %s\n", e->Name);
		return 0;
	}
	virtual int FireStartTransaction(X12ReaderStartTransactionEventParams* e) {
		printf("FireStartTransaction: %s\n", e->Tag);
		return 0;
	}
	virtual int FireWarning(X12ReaderWarningEventParams* e) {
		printf("WARNING: %i:%s\n", e->WarnCode, e->Message);
		return 0;
	}


};

int main()
{
	printf("This demo uses a test Invoice document installed with the demo. To parse your own files modify the demo code to specify the document and schema.\n");
	printf("\nPress any key to continue.");
	getchar();

	MyX12Reader x12Reader;

	x12Reader.Config("Encoding=iso-8859-1");

	int ret_code = x12Reader.SetSchemaFormat(SCHEMA_JSON);
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, x12Reader.GetLastError());

	ret_code = x12Reader.LoadSchema("./RSSBus_00401_810.json");
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, x12Reader.GetLastError());

	x12Reader.SetInputFile("x12.txt");
	ret_code = x12Reader.Parse();
	if (ret_code)
		printf("ERROR: %i:%s\n", ret_code, x12Reader.GetLastError());

	//example XPath statements
	printf("\n\n\nExample XPath statements output:\n\n");
	x12Reader.SetXPath("/IX");
	if (x12Reader.GetElementCount() > 0) {
		//first element of root info
		printf("First element of root: Name='%s'; Value='%s'\n", x12Reader.GetElementName(0), x12Reader.GetElementValue(0));
	}
	if (x12Reader.GetXChildren() > 0) {
		x12Reader.SetXPath(strcat(x12Reader.GetXPath(), "/[1]"));
		//first child of root info
		printf("First child of root: Segment='%s'; Tag='%s'; Value='%s'\n", x12Reader.GetXPath(), x12Reader.GetXSegment(), x12Reader.GetXTag());
	}

	printf("\nPress any key to continue.");
	getchar();
	return 0;
}




