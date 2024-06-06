/*
 * IPWorks EDI Translator 2024 Java Edition - Sample Project
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

import java.io.*;


import ipworkseditranslator.*;

public class edifacttranslator extends ConsoleDemo {

	public static void main(String[] args) {


		if (args.length != 3) {

			System.out.println("usage: edifacttranslator translationMode fileToRead fileToWrite");
			System.out.println("");
			System.out.println("  translationMode 1 to translate EDI to XML or 2 to translate XML to EDI");
			System.out.println("  fileToRead      the file to tranlsate");
			System.out.println("  fileToWrite     the file to which the translated data will be written");
			System.out.println("\r\nExample: edifacttranslator 1 ./INVOIC.edi ./INVOICE.xml");
			System.out.println("\r\nExample: edifacttranslator 2 ./INVOICE.xml ./INVOIC_fromxml.edi");

		} else {
			EDIFACTTranslator editranslator1 = new EDIFACTTranslator();

			System.out.println("This demo demonstrates the translation of EDIFACT files to XML and vice-versa through the use of the EDIFACTranslator component.");


			try {

				switch(args[0]) { //
					case "1": System.out.println("Converting EDIFACT File to XML File.");
						editranslator1.reset();
						editranslator1.loadSchema("./RSSBus_D97A_INVOIC.json");
						editranslator1.setInputFile(args[1]);
						editranslator1.setOutputFile(args[2]);
						editranslator1.setOverwrite(true);
						editranslator1.translate();
						System.out.println("Translation complete.");
						break;
					case "2": System.out.println("Converting XML File to EDIFACT File.");
						editranslator1.reset();
						editranslator1.setInputFormat(EDIFACTTranslator.eifXML);
						editranslator1.setOutputFormat(EDIFACTTranslator.eofEDIFACT);
						editranslator1.setInputFile(args[1]);
						editranslator1.setOutputFile(args[2]);
						editranslator1.setOverwrite(true);
						editranslator1.translate();
						System.out.println("Translation complete.");
						break;
					default:  System.out.println("Unrecognized option.");
						break;
				}

			} catch(Exception ex) {
				displayError(ex);
			}
		}
	}
}


class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksEDITranslatorException) {
      System.out.print(" (" + ((IPWorksEDITranslatorException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



