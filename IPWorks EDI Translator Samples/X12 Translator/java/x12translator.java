/*
 * IPWorks EDI Translator 2022 Java Edition - Sample Project
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

public class x12translator extends ConsoleDemo {

	public static void main(String[] args) {


		if (args.length != 3) {

			System.out.println("usage: x12translator translationMode fileToRead fileToWrite");
			System.out.println("");
			System.out.println("  translationMode 1 to translate X12 to XML or 2 to translate XML to X12");
			System.out.println("  fileToRead      the file to tranlsate");
			System.out.println("  fileToWrite     the file to which the translated data will be written");
			System.out.println("\r\nExample: x12translator 1 ./x12.txt ./810.xml");
			System.out.println("\r\nExample: x12translator 2 ./810.xml ./810_fromxml.txt");

		} else {
			X12translator x12translator1 = new X12translator();

			System.out.println("This demo demonstrates the translation of X12 files to XML and vice-versa through the use of the X12ranslator component.");


			try {

				switch(args[0]) { //
					case "1": System.out.println("Converting X12 File to XML File.");
						x12translator1.reset();
						x12translator1.loadSchema("./RSSBus_00401_810.json");
						x12translator1.setInputFile(args[1]);
						x12translator1.setOutputFile(args[2]);
						x12translator1.setOverwrite(true);
						x12translator1.translate();
						System.out.println("Translation complete.");
						break;
					case "2": System.out.println("Converting XML File to X12 File.");
						x12translator1.reset();
						x12translator1.setInputFormat(X12translator.xifXML);
						x12translator1.setOutputFormat(X12translator.xofX12);
						x12translator1.setInputFile(args[1]);
						x12translator1.setOutputFile(args[2]);
						x12translator1.setOverwrite(true);
						x12translator1.translate();
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

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
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



