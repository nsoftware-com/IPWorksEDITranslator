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

char input[LINE_LEN + 1];
const char* prompt(const char* prompt, const char* defaultVal) {
  printf("%s [%s]: ", prompt, defaultVal);
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  if (strlen(input) == 0) {
    strncpy(input, defaultVal, LINE_LEN);
    input[LINE_LEN] = '\0';
  }
  return input;
}
const bool yesno(const char* prompt, const bool defaultVal) {
  char def = defaultVal ? 'y' : 'n';
  printf("%s (y/n) [%c]: ", prompt, def);
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  if (strlen(input) == 0) return defaultVal;
  char answer = input[0];
  if (answer == 'y' || answer == 'Y') return true;
  else if (answer == 'n' || answer == 'N') return false;
  else return defaultVal;
}

int main(int argc, char *argv[])
{
	EDIFACTTranslator ediTranslator;

	printf("****************************************************************\n");
	printf("* This demo shows how to use the EDIFACTTranslator component to    *\n");
	printf("* convert an EDI document to XML, or vice versa. A sample EDI  *\n");
	printf("* document and corresponding schema is provided.               *\n");
	printf("****************************************************************\n");

    switch (atoi(prompt("Possible input format values are:\n0 - XML\n1 - JSON\n2 - EDIFACT\nPlease select a value", "2"))) {
		case 0:
			ediTranslator.SetInputFormat(EIF_XML);
			break;
		case 1:
			ediTranslator.SetInputFormat(EIF_JSON);
			break;
		case 2:
			ediTranslator.SetInputFormat(EIF_EDIFACT);
			break;
	}
	
	if (ediTranslator.GetInputFormat() == 0) {
		// InputFormat is XML, so OutputFormat must be an EDI format.
		switch (atoi(prompt("Possible output format values are:\n0 - XML\n1 - JSON\n2 - EDIFACT\nPlease select a value", ""))) {
			case 0:
				ediTranslator.SetOutputFormat(EOF_XML);
				break;
			case 1:
				ediTranslator.SetOutputFormat(EOF_JSON);
				break;
			case 2:
				ediTranslator.SetOutputFormat(EOF_EDIFACT);
				break;
		}
	} else {
		// InputFormat is EDI, so OutputFormat must be XML.
		printf("Output format set to XML.\n");
		ediTranslator.SetOutputFormat(EOF_XML);
	}

	if (ediTranslator.GetInputFormat() > 1) {
		if (yesno("Do you want to load a schema file?", true)) {
			ediTranslator.LoadSchema(prompt("Specify schema file path", (ediTranslator.GetInputFormat() == 2 ? "./RSSBus_D97A_INVOIC.json" : "")));
		}
	}

	ediTranslator.SetInputFile(prompt("Specify input file to translate", (ediTranslator.GetInputFormat() == 2 ? "./INVOIC.edi" : "")));

	ediTranslator.SetOutputFile(prompt("Specify output file", (ediTranslator.GetOutputFormat() > 1 ? "./output.edi" : "./output.xml")));
	
	ediTranslator.SetOverwrite(true);
	int ret_code = ediTranslator.Translate();

	if (ret_code)
		printf("ERROR:%i:%s\n", ret_code, ediTranslator.GetLastError());
	else
		printf("Translation Complete\n");

	printf("\nPress any key to continue.");
    getchar();
	return 0;
}








