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

public class x12writer extends ConsoleDemo {
	public static void main(String[] args) {
		X12writer x12writer = new X12writer();

		try {

			System.out.println("***************************************************************************");
			System.out.println("* This demo shows how to use x12writer to create an x12 document.         *");
			System.out.println("* Several example X12 documents can be created with this demo.*");
			System.out.println("* Please see the code for details.                                        *");
			System.out.println("***************************************************************************");

			String docType = "";

			while (!docType.equals("810") && !docType.equals("850") && !docType.equals("855")
					&& !docType.equals("856")) {
				docType = prompt("Document Type To Generate [810, 850, 855, 856]");
				x12writer.loadSchema(".\\RSSBus_00401_" + docType + ".json");
			}

			x12writer.config("Encoding=iso-8859-1");

			x12writer.setSuffix(X12writer.suffixCRLF);

			// will print output to console
			x12writer.setOutputStream(System.out);

			// to save to a file, use
			// x12writer.setOutputFile("filename.txt");

			// to save data to a string, make sure no OutputFile or OutputStream
			// has been set
			// x12writer.setOutputFile("");
			// to get data after it has been written, use
			// x12writer.getOutputData();

			System.out.println("\nGenerating EDI document and printing to console:\n");
			// print correct document
			if (docType.equals("810"))
				writeFile_X12_810(x12writer);
			else if (docType.equals("850"))
				writeFile_X12_850(x12writer);
			else if (docType.equals("855"))
				writeFile_X12_855(x12writer);
			else if (docType.equals("856"))
				writeFile_X12_856(x12writer);

		}  catch (Exception ex) {
			displayError(ex);
		}
	}

	private static void writeFile_X12_810(X12writer x12writer) {
		try {
			x12writer.startInterchangeHeader("004010");
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("U");
			x12writer.writeElementString("00401");
			x12writer.writeElementString("000000006");
			x12writer.writeElementString("0");
			x12writer.writeElementString("T");
			x12writer.writeElementString(">");
			x12writer.endElement();

			x12writer.startFunctionalGroupHeader();
			x12writer.writeElementString("IN");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("20160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("6");
			x12writer.writeElementString("T");
			x12writer.writeElementString("004010");
			x12writer.endElement();

			x12writer.startTransactionHeader("810");
			x12writer.writeElementString("810");
			x12writer.writeElementString("0001");
			x12writer.endElement();

			x12writer.startSegment("BIG");
			x12writer.writeElementString("20150708");
			x12writer.writeElementString("3003014445");
			x12writer.skipElement();
			x12writer.writeElementString("0476553272");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("DR");
			x12writer.endElement();

			x12writer.startSegment("CUR");
			x12writer.writeElementString("SE");
			x12writer.writeElementString("USD");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("8M");
			x12writer.writeElementString("0056");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("BY");
			x12writer.writeElementString("Company");
			x12writer.writeElementString("92");
			x12writer.writeElementString("544380");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N3");
			x12writer.writeElementString("Address");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N4");
			x12writer.writeElementString("City");
			x12writer.writeElementString("CA");
			x12writer.writeElementString("Postal Code");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("ST");
			x12writer.writeElementString("Name");
			x12writer.writeElementString("92");
			x12writer.writeElementString("0607047800010");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N3");
			x12writer.writeElementString("Address");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N4");
			x12writer.writeElementString("City");
			x12writer.skipElement();
			x12writer.writeElementString("200131");
			x12writer.writeElementString("Country");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("RE");
			x12writer.writeElementString("Name");
			x12writer.writeElementString("92");
			x12writer.writeElementString("5095956");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N3");
			x12writer.writeElementString("Address");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N4");
			x12writer.writeElementString("City");
			x12writer.writeElementString("IL");
			x12writer.writeElementString("Postal Code");
			x12writer.endElement();

			x12writer.startSegment("IT1Loop1/IT1");
			x12writer.writeElementString("20");
			x12writer.writeElementString("2500");
			x12writer.writeElementString("EA");
			x12writer.writeElementString("36.96");
			x12writer.skipElement();
			x12writer.writeElementString("BP");
			x12writer.writeElementString("335S0594");
			x12writer.endElement();

			x12writer.startSegment("IT1Loop1/REF_3");
			x12writer.writeElementString("KK");
			x12writer.writeElementString("0099778154");
			x12writer.endElement();

			x12writer.startSegment("IT1Loop1/REF_3");
			x12writer.writeElementString("PO");
			x12writer.writeElementString("0476553272");
			x12writer.writeElementString("20");
			x12writer.endElement();

			x12writer.startSegment("TDS");
			x12writer.writeElementString("9240000");
			x12writer.endElement();

			x12writer.startSegment("CTT");
			x12writer.writeElementString("1");
			x12writer.endElement();

			x12writer.createTransactionFooter();

			x12writer.createFunctionalGroupFooter();

			x12writer.createInterchangeFooter();
		} catch (Exception ex) {
			displayError(ex);
		}
	}

	private static void writeFile_X12_850(X12writer x12writer) {
		try {
			x12writer.startInterchangeHeader("004010");
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("U");
			x12writer.writeElementString("00401");
			x12writer.writeElementString("000000007");
			x12writer.writeElementString("0");
			x12writer.writeElementString("T");
			x12writer.writeElementString(">");
			x12writer.endElement();

			x12writer.startFunctionalGroupHeader();
			x12writer.writeElementString("PO");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("20160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("7");
			x12writer.writeElementString("T");
			x12writer.writeElementString("004010");
			x12writer.endElement();

			x12writer.startTransactionHeader("850");
			x12writer.writeElementString("850");
			x12writer.writeElementString("0001");
			x12writer.endElement();

			x12writer.startSegment("BEG");
			x12writer.writeElementString("00");
			x12writer.writeElementString("DS");
			x12writer.writeElementString("0476696888");
			x12writer.skipElement();
			x12writer.writeElementString("20150708");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("SB");
			x12writer.writeElementString("ZZ11");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("6P");
			x12writer.writeElementString("ZZ");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("8M");
			x12writer.writeElementString("0056");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("CR");
			x12writer.writeElementString("1070335099");
			x12writer.endElement();

			x12writer.startSegment("REF");
			x12writer.writeElementString("CO");
			x12writer.writeElementString("7109790082");
			x12writer.endElement();

			x12writer.startSegment("PER");
			x12writer.writeElementString("CN");
			x12writer.writeElementString("name");
			x12writer.writeElementString("TE");
			x12writer.writeElementString("Number");

			x12writer.startSegment("CSH");
			x12writer.writeElementString("BK");
			x12writer.endElement();

			x12writer.startSegment("SACLoop1/SAC");
			x12writer.writeElementString("C");
			x12writer.writeElementString("ZZZZ");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("06");
			x12writer.endElement();

			x12writer.startSegment("TD5");
			x12writer.writeElementString("Z");
			x12writer.writeElementString("2");
			x12writer.writeElementString("Code");
			x12writer.endElement();

			x12writer.startSegment("N9Loop1/N9");
			x12writer.writeElementString("PD");
			x12writer.writeElementString("ZCOF");
			x12writer.endElement();

			x12writer.startSegment("N9Loop1/MSG");
			x12writer.writeElementString("Thanks!");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("BY");
			x12writer.writeElementString("Name");
			x12writer.writeElementString("92");
			x12writer.writeElementString("5601");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("EN");
			x12writer.writeElementString("Name");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N1");
			x12writer.writeElementString("ST");
			x12writer.writeElementString("OEM NAME");
			x12writer.writeElementString("92");
			x12writer.writeElementString("0000505462");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N2");
			x12writer.writeElementString("additional name");
			x12writer.writeElementString(""); // not skipped because last
			// element
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N3");
			x12writer.writeElementString("Address");
			x12writer.writeElementString("Address");
			x12writer.endElement();

			x12writer.startSegment("N1Loop1/N4");
			x12writer.writeElementString("City");
			x12writer.skipElement();
			x12writer.writeElementString("201613");
			x12writer.writeElementString("CN");
			x12writer.writeElementString("SP");
			x12writer.writeElementString("020");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/PO1");
			x12writer.writeElementString("00010");
			x12writer.writeElementString("500000");
			x12writer.writeElementString("EA");
			x12writer.writeElementString("495");
			x12writer.skipElement();
			x12writer.writeElementString("BP");
			x12writer.writeElementString("337S3744");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/PIDLoop1/PID_2");
			x12writer.writeElementString("F");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("Thanks!");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("EN");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/REF_7");
			x12writer.writeElementString("CO");
			x12writer.writeElementString("7109790082");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/REF_7");
			x12writer.writeElementString("LI");
			x12writer.writeElementString("000010");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/SCHLoop1/SCH");
			x12writer.writeElementString("500000");
			x12writer.writeElementString("EA");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("002");
			x12writer.writeElementString("20180708");
			x12writer.endElement();

			x12writer.startSegment("CTTLoop1/CTT");
			x12writer.writeElementString("1");
			x12writer.writeElementString("500000");
			x12writer.endElement();

			x12writer.createTransactionFooter();

			x12writer.createFunctionalGroupFooter();

			x12writer.createInterchangeFooter();
		} catch (Exception ex) {
			displayError(ex);
		}
	}

	private static void writeFile_X12_855(X12writer x12writer) {
		try {
			x12writer.startInterchangeHeader("004010");
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("U");
			x12writer.writeElementString("00401");
			x12writer.writeElementString("000000008");
			x12writer.writeElementString("0");
			x12writer.writeElementString("T");
			x12writer.writeElementString(">");
			x12writer.endElement();

			x12writer.startFunctionalGroupHeader();
			x12writer.writeElementString("PR");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("20160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("8");
			x12writer.writeElementString("T");
			x12writer.writeElementString("004010");
			x12writer.endElement();

			x12writer.startTransactionHeader("855");
			x12writer.writeElementString("855");
			x12writer.writeElementString("0013");
			x12writer.endElement();

			x12writer.startSegment("BAK");
			x12writer.writeElementString("00");
			x12writer.writeElementString("AT");
			x12writer.writeElementString("0476553696");
			x12writer.writeElementString("20150708");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("4900043704");
			x12writer.writeElementString("20150708");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/PO1");
			x12writer.writeElementString("000010");
			x12writer.writeElementString("1100");
			x12writer.writeElementString("EA");
			x12writer.writeElementString("14.00");
			x12writer.skipElement();
			x12writer.writeElementString("BP");
			x12writer.writeElementString("335S0548");
			x12writer.writeElementString("VP");
			x12writer.writeElementString("Product");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/REF");
			x12writer.writeElementString("PO");
			x12writer.writeElementString("0476553696");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/REF");
			x12writer.writeElementString("VN");
			x12writer.writeElementString("0025009879");
			x12writer.endElement();

			x12writer.startSegment("PO1Loop1/ACKLoop1/ACK");
			x12writer.writeElementString("IA");
			x12writer.writeElementString("1100");
			x12writer.writeElementString("EA");
			x12writer.writeElementString("067");
			x12writer.writeElementString("20150709");
			x12writer.endElement();

			x12writer.startSegment("CTTLoop1/CTT");
			x12writer.writeElementString("1");
			x12writer.writeElementString("1100");
			x12writer.endElement();

			x12writer.createTransactionFooter();

			x12writer.createFunctionalGroupFooter();

			x12writer.createInterchangeFooter();
		} catch (Exception ex) {
			displayError(ex);
		}
	}

	private static void writeFile_X12_856(X12writer x12writer) {
		try {
			x12writer.startInterchangeHeader("004010");
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("00");
			x12writer.skipElement();
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("ZZ");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("U");
			x12writer.writeElementString("00401");
			x12writer.writeElementString("000000009");
			x12writer.writeElementString("0");
			x12writer.writeElementString("T");
			x12writer.writeElementString(">");
			x12writer.endElement();

			x12writer.startFunctionalGroupHeader();
			x12writer.writeElementString("SH");
			x12writer.writeElementString("ACME");
			x12writer.writeElementString("WAYNE_TECH");
			x12writer.writeElementString("20160707");
			x12writer.writeElementString("1544");
			x12writer.writeElementString("9");
			x12writer.writeElementString("T");
			x12writer.writeElementString("004010");
			x12writer.endElement();

			x12writer.startTransactionHeader("856");
			x12writer.writeElementString("856");
			x12writer.writeElementString("0029");
			x12writer.endElement();

			x12writer.startSegment("BSN");
			x12writer.writeElementString("00");
			x12writer.writeElementString("0403734501");
			x12writer.writeElementString("20150708");
			x12writer.writeElementString("162859");
			x12writer.endElement();

			x12writer.startSegment("DTM");
			x12writer.writeElementString("011");
			x12writer.writeElementString("20150708");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/HL");
			x12writer.writeElementString("1");
			x12writer.skipElement();
			x12writer.writeElementString("S");
			x12writer.writeElementString("1");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/PRF");
			x12writer.writeElementString("0476553696");
			x12writer.skipElement();
			x12writer.skipElement();
			x12writer.writeElementString("20150708");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/TD1");
			x12writer.writeElementString("CNT90");
			x12writer.writeElementString("0");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/TD5");
			x12writer.writeElementString("O");
			x12writer.writeElementString("2");
			x12writer.writeElementString("FEDX");
			x12writer.writeElementString("A");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/REF");
			x12writer.writeElementString("BM");
			x12writer.writeElementString("EDITEST403734501");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/REF");
			x12writer.writeElementString("CR");
			x12writer.writeElementString("4900043704");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/HL");
			x12writer.writeElementString("2");
			x12writer.writeElementString("1");
			x12writer.writeElementString("O");
			x12writer.writeElementString("1");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/N1Loop1/N1");
			x12writer.writeElementString("ST");
			x12writer.writeElementString("Name");
			x12writer.writeElementString("92");
			x12writer.writeElementString("0042001808");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/N1Loop1/N1");
			x12writer.writeElementString("SF");
			x12writer.writeElementString("NameT");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/N1Loop1/N3");
			x12writer.writeElementString("Address");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/N1Loop1/N4");
			x12writer.writeElementString("City");
			x12writer.writeElementString("SG");
			x12writer.writeElementString("339942");
			x12writer.writeElementString("SG");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/HL");
			x12writer.writeElementString("3");
			x12writer.writeElementString("2");
			x12writer.writeElementString("I");
			x12writer.writeElementString("0");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/LIN");
			x12writer.writeElementString("10");
			x12writer.writeElementString("BP");
			x12writer.writeElementString("335S0548");
			x12writer.writeElementString("VP");
			x12writer.writeElementString("Product");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/SN1");
			x12writer.writeElementString("10");
			x12writer.writeElementString("1100");
			x12writer.writeElementString("EA");
			x12writer.endElement();

			x12writer.startSegment("HLLoop1/MAN");
			x12writer.writeElementString("CP");
			x12writer.writeElementString("Marks");
			x12writer.endElement();

			x12writer.startSegment("CTT");
			x12writer.writeElementString("1");
			x12writer.endElement();

			x12writer.createTransactionFooter();

			x12writer.createFunctionalGroupFooter();

			x12writer.createInterchangeFooter();
		} catch (Exception ex) {
			displayError(ex);
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



