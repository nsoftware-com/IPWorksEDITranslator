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
#include "../../include/ipworksEDITranslator.h"
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
	X12Translator x12Translator;

	printf("****************************************************************\n");
	printf("* This demo shows how to use the X12Translator component to    *\n");
	printf("* convert an X12 document to XML, or vice versa. A sample X12  *\n");
	printf("* document and corresponding schema is provided.               *\n");
	printf("****************************************************************\n");

    switch (atoi(prompt("Possible input format values are:\n0 - XML\n1 - JSON\n2 - X12\nPlease select a value", "2"))) {
		case 0:
			x12Translator.SetInputFormat(XIF_XML);
			break;
		case 1:
			x12Translator.SetInputFormat(XIF_JSON);
			break;
		case 2:
			x12Translator.SetInputFormat(XIF_X12);
			break;
	}
	
	if (x12Translator.GetInputFormat() == 0) {
		// InputFormat is XML, so OutputFormat must be an X12 format.
		switch (atoi(prompt("Possible output format values are:\n0 - XML\n1 - JSON\n3 - X12\nPlease select a value", ""))) {
			case 0:
				x12Translator.SetOutputFormat(XOF_XML);
				break;
			case 1:
				x12Translator.SetOutputFormat(XOF_JSON);
				break;
			case 3:
				x12Translator.SetOutputFormat(XOF_X12);
				break;
		}
	} else {
		// InputFormat is X12, so OutputFormat must be XML.
		printf("Output format set to XML.\n");
		x12Translator.SetOutputFormat(XOF_XML);
	}

	if (x12Translator.GetInputFormat() > 1) {
		if (yesno("Do you want to load a schema file?", true)) {
			x12Translator.LoadSchema(prompt("Specify schema file path", (x12Translator.GetInputFormat() == 2 ? "./RSSBus_00401_810.json" : "")));
		}
	}

	x12Translator.SetInputFile(prompt("Specify input file to translate", (x12Translator.GetInputFormat() == 2 ? "./x12.txt" : "")));

	x12Translator.SetOutputFile(prompt("Specify output file", (x12Translator.GetOutputFormat() > 1 ? "./output.txt" : "./output.xml")));
	
	x12Translator.SetOverwrite(true);
	int ret_code = x12Translator.Translate();

	if (ret_code)
		printf("ERROR:%i:%s\n", ret_code, x12Translator.GetLastError());
	else
		printf("Translation Complete\n");

	printf("\nPress any key to continue.");
    getchar();
	return 0;
}








