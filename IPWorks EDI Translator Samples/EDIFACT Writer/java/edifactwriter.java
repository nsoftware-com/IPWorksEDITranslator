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

public class edifactwriter extends ConsoleDemo {
    public static void main(String[] args) {
        Edifactwriter edifactWriter = new Edifactwriter();

        try {

            System.out.println("***********************************************************************");
            System.out.println("* This demo shows how to use EDIFACTWriter to create an EDI document. *");
            System.out.println("* Several example EDIFACT documents can be created with this demo.    *");
            System.out.println("* Please see the code for details.                                    *");
            System.out.println("***********************************************************************");

            String docType = "";
            while (!docType.equals("DESADV") && !docType.equals("INVOIC") && !docType.equals("ORDERS")
                    && !docType.equals("ORDRSP")) {
                docType = prompt("Document Type To Generate [DESADV, INVOIC, ORDERS, ORDRSP]");
            }

            edifactWriter.config("Encoding=iso-8859-1");
            edifactWriter.loadSchema(".\\RSSBus_D97A_" + docType + ".json");
            edifactWriter.setSuffix(Edifactwriter.suffixCRLF);

            //will print output to console
            edifactWriter.setOutputStream(System.out);

            //to save to a file, use
            //edifactWriter.setOutputFile("filename.txt");

            //to save data to a string, make sure no OutputFile or OutputStream has been set
            //edifactWriter.setOutputFile("");
            //to get data after it has been written to string, use
            //edifactWriter.getOutputData();

            System.out.println("\nGenerating EDI document and printing to console:\n");
            // print correct document
            if (docType.equals("DESADV"))
                writeFile_EDIFACT_DESADV(edifactWriter);
            else if (docType.equals("INVOIC"))
                writeFile_EDIFACT_INVOIC(edifactWriter);
            else if (docType.equals("ORDERS"))
                writeFile_EDIFACT_ORDERS(edifactWriter);
            else if (docType.equals("ORDRSP"))
                writeFile_EDIFACT_ORDRSP(edifactWriter);

        } catch (IPWorksEDITranslatorException ex) {
            System.out.println("EDIFACT exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        }
    }

    private static void writeFile_EDIFACT_DESADV(Edifactwriter edifactWriter) {
        try {
            edifactWriter.startInterchangeHeader("D97A");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("UNOB");
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("WAYNE_TECH");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ACME");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("160707");
            edifactWriter.writeComponentString("1547");
            edifactWriter.endElement();
            edifactWriter.writeElementString("000000001");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("");
            edifactWriter.endElement();
            edifactWriter.writeElementString("1234");
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startTransactionHeader("DESADV");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("DESADV");
            edifactWriter.writeComponentString("D");
            edifactWriter.writeComponentString("97A");
            edifactWriter.writeComponentString("UN");
            edifactWriter.endElement();

            edifactWriter.startSegment("BGM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("351");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2014/10093");
            edifactWriter.endElement();
            edifactWriter.writeElementString("9");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("137");
            edifactWriter.writeComponentString("201404192036");
            edifactWriter.writeComponentString("203");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("201404192036");
            edifactWriter.writeComponentString("203");
            edifactWriter.endElement();

            edifactWriter.startSegment("MEA");
            edifactWriter.writeElementString("AAX");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("SQ");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("C62");
            edifactWriter.writeComponentString("17");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("ST");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0018");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("SU");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2019813");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("TDTLoop1/TDT");
            edifactWriter.writeElementString("12");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("M");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("CARRIER");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("86");
            edifactWriter.endElement();

            edifactWriter.startSegment("EQDLoop1/EQD");
            edifactWriter.writeElementString("TE");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("X");
            edifactWriter.endElement();
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/CPS");
            edifactWriter.writeElementString("1");
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/PACLoop1/PAC");
            edifactWriter.writeElementString("4");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("BOX-001");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/PACLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("52");
            edifactWriter.writeComponentString("50");
            edifactWriter.writeComponentString("C62");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/CPS");
            edifactWriter.writeElementString("2");
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/PACLoop1/PAC");
            edifactWriter.writeElementString("2");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("BOX-002");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/PACLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("52");
            edifactWriter.writeComponentString("100");
            edifactWriter.writeComponentString("C62");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/LINLoop1/LIN");
            edifactWriter.writeElementString("1");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("9001");
            edifactWriter.writeComponentString("IN");
            edifactWriter.endElement();

            edifactWriter.startSegment("CPSLoop1/LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("12");
            edifactWriter.writeComponentString("400");
            edifactWriter.writeComponentString("C62");
            edifactWriter.endElement();

            edifactWriter.startSegment("RFFLoop1/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ON");
            edifactWriter.writeComponentString("N55109001");
            edifactWriter.endElement();

            edifactWriter.createTransactionFooter();

            edifactWriter.createInterchangeFooter();
        } catch (IPWorksEDITranslatorException ex) {
            System.out.println("EDIFACT exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
        }
    }

    private static void writeFile_EDIFACT_INVOIC(Edifactwriter edifactWriter) {
        try {
            edifactWriter.startInterchangeHeader("D97A");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("UNOB");
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("WAYNE_TECH");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ACME");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("160707");
            edifactWriter.writeComponentString("1547");
            edifactWriter.endElement();
            edifactWriter.writeElementString("000000002");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("");
            edifactWriter.endElement();
            edifactWriter.writeElementString("1234");
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startTransactionHeader("INVOIC");
            edifactWriter.writeElementString("509010117");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INVOIC");
            edifactWriter.writeComponentString("D");
            edifactWriter.writeComponentString("97A");
            edifactWriter.writeComponentString("UN");
            edifactWriter.endElement();

            edifactWriter.startSegment("BGM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("380");
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("TAX INVOICE");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0013550417");
            edifactWriter.endElement();
            edifactWriter.writeElementString("9");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("3");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("4");
            edifactWriter.writeComponentString("20061123");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("FTX");
            edifactWriter.writeElementString("AAI");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("10072.14");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("CUXLoop1/CUX");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("EUR");
            edifactWriter.writeComponentString("4");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.writeElementString("0.67529");
            edifactWriter.endElement();

            edifactWriter.startSegment("PATLoop1/PAT");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("PATLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("10");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("PATLoop1/PCD");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("0");
            edifactWriter.writeComponentString("13");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000030");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2265S13");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5029766832002");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("50.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("19150.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("383.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("19150.45");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.45");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.45");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000040");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2269F22");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5051254078241");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("20.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("21060.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("1053.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("21060.50");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.50");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.50");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000170");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2269F10");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5051254078326");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("10.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("6950.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("695.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("6950.16");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.16");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.16");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000190");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2269F26");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5051254051190");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("5.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("2375.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("475.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("2375.06");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.06");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.06");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000200");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2265S24");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5029766000685");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("3.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("957.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("319.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("957.02");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.02");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.02");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000210");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2263T95");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5029766699575");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("3.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("2085.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("695.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("2085.05");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.05");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.05");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("000250");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2269F15");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("5051254080091");
            edifactWriter.writeComponentString("UP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.writeElementString("");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("47");
            edifactWriter.writeComponentString("3.000");
            edifactWriter.writeComponentString("EA");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("11");
            edifactWriter.writeComponentString("20070926");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("203");
            edifactWriter.writeComponentString("4977.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INV");
            edifactWriter.writeComponentString("1659.00");
            edifactWriter.writeComponentString("TU");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("4977.12");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0.12");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("0.12");
            edifactWriter.endElement();

            edifactWriter.startSegment("UNS");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("CNT");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("4");
            edifactWriter.writeComponentString("7");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("9");
            edifactWriter.writeComponentString("67627.50");
            edifactWriter.endElement();

            edifactWriter.startSegment("MOALoop4/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("79");
            edifactWriter.writeComponentString("57554.00");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/TAX");
            edifactWriter.writeElementString("7");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VAT");
            edifactWriter.endElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("17.500");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("125");
            edifactWriter.writeComponentString("57555.36");
            edifactWriter.writeComponentString("EUR");
            edifactWriter.writeComponentString("3");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("124");
            edifactWriter.writeComponentString("10072.14");
            edifactWriter.writeComponentString("EUR");
            edifactWriter.writeComponentString("3");
            edifactWriter.endElement();

            edifactWriter.startSegment("ALCLoop1/ALC");
            edifactWriter.writeElementString("C");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("1.36");
            edifactWriter.endElement();
            edifactWriter.writeElementString("");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FC");
            edifactWriter.endElement();

            edifactWriter.startSegment("TAXLoop1/MOA");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("8");
            edifactWriter.writeComponentString("1.36");
            edifactWriter.endElement();

            edifactWriter.createTransactionFooter();

            edifactWriter.createInterchangeFooter();
        } catch (IPWorksEDITranslatorException ex) {
            System.out.println("EDIFACT exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
        }
    }

    private static void writeFile_EDIFACT_ORDERS(Edifactwriter edifactWriter) {
        try {
            edifactWriter.startInterchangeHeader("D97A");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("UNOB");
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("WAYNE_TECH");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ACME");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("160707");
            edifactWriter.writeComponentString("1547");
            edifactWriter.endElement();
            edifactWriter.writeElementString("000000003");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("");
            edifactWriter.endElement();
            edifactWriter.writeElementString("1234");
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startTransactionHeader("ORDERS");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ORDERS");
            edifactWriter.writeComponentString("D");
            edifactWriter.writeComponentString("97A");
            edifactWriter.writeComponentString("UN");
            edifactWriter.writeComponentString("ED17A1");
            edifactWriter.endElement();

            edifactWriter.startSegment("BGM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("105");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("K12345");
            edifactWriter.endElement();
            edifactWriter.writeElementString("9");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("137");
            edifactWriter.writeComponentString("19980626");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("FTX");
            edifactWriter.writeElementString("GEN");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("FREE TEXT");
            edifactWriter.endElement();

            edifactWriter.startSegment("RFFLoop1/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("PS");
            edifactWriter.writeComponentString("10501");
            edifactWriter.endElement();

            edifactWriter.startSegment("RFFLoop1/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("CT");
            edifactWriter.writeComponentString("NO");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("BY");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/RFFLoop2/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("VA");
            edifactWriter.writeComponentString("GB107328000");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("SE");
            edifactWriter.endElement();

            edifactWriter.startSegment("CUXLoop1/CUX");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("GBP");
            edifactWriter.writeComponentString("9");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("001");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0000057G3454");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("DESCRIPTION");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INF");
            edifactWriter.writeComponentString("27.54");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("9829");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("JB");
            edifactWriter.writeComponentString("JOB NO");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("19980717");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("002");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0000057G3454");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("DESCRIPTION");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("4000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INF");
            edifactWriter.writeComponentString("27.54");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("9830");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("JB");
            edifactWriter.writeComponentString("JOB NO");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("4000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("19980724");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("003");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("0000057G3454");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/IMD");
            edifactWriter.writeElementString("F");
            edifactWriter.skipElement();
            edifactWriter.startElement();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("DESCRIPTION");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("3000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("INF");
            edifactWriter.writeComponentString("27.54");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("9831");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("JB");
            edifactWriter.writeComponentString("JOB NO");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("21");
            edifactWriter.writeComponentString("3000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("19980731");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("UNS");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.createTransactionFooter();

            edifactWriter.createInterchangeFooter();
        } catch (IPWorksEDITranslatorException ex) {
            System.out.println("EDIFACT exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
        }
    }

    private static void writeFile_EDIFACT_ORDRSP(Edifactwriter edifactWriter) {
        try {
            edifactWriter.startInterchangeHeader("D97A");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("UNOB");
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("WAYNE_TECH");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ACME");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("160707");
            edifactWriter.writeComponentString("1547");
            edifactWriter.endElement();
            edifactWriter.writeElementString("000000004");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("");
            edifactWriter.endElement();
            edifactWriter.writeElementString("1234");
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.skipElement();
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startTransactionHeader("ORDRSP");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ORDRSP");
            edifactWriter.writeComponentString("D");
            edifactWriter.writeComponentString("97A");
            edifactWriter.writeComponentString("UN");
            edifactWriter.writeComponentString("EDOR04");
            edifactWriter.endElement();

            edifactWriter.startSegment("BGM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("231");
            edifactWriter.endElement();
            edifactWriter.startElement();
            edifactWriter.writeComponentString("1109706372/3");
            edifactWriter.endElement();
            edifactWriter.writeElementString("9");
            edifactWriter.endElement();

            edifactWriter.startSegment("DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("137");
            edifactWriter.writeComponentString("20150708");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("RFFLoop1/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("ON");
            edifactWriter.writeComponentString("INCG14040002");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("SE");
            edifactWriter.endElement();

            edifactWriter.startSegment("NADLoop1/NAD");
            edifactWriter.writeElementString("BY");
            edifactWriter.endElement();

            edifactWriter.startSegment("CUXLoop1/CUX");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("USD");
            edifactWriter.writeComponentString("4");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("1");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("800");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.9600");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("800");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20140401");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150729");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("2");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.9600");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("2");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20141020");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150729");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("3");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRFS4115PBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.9600");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("3");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20141120");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150809");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("4");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("4000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.1000");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("4");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("4000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20140605");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150810");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("5");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("12000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.1000");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("5");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("12000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20140705");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150801");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/LIN");
            edifactWriter.writeElementString("6");
            edifactWriter.writeElementString("6");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("VP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("91");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PIA");
            edifactWriter.writeElementString("1");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("IRLR8259TRPBF");
            edifactWriter.writeComponentString("BP");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("92");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("12000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/PRILoop1/PRI");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("AAA");
            edifactWriter.writeComponentString("0.1000");
            edifactWriter.writeComponentString("CT");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("1");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("LINLoop1/RFFLoop3/RFF");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("LI");
            edifactWriter.skipComponent();
            edifactWriter.writeComponentString("6");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("10000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20140805");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150805");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/SCC");
            edifactWriter.writeElementString("1");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/QTY");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("113");
            edifactWriter.writeComponentString("2000");
            edifactWriter.writeComponentString("PCE");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("2");
            edifactWriter.writeComponentString("20140805");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("SCCLoop1/QTYLoop1/DTM");
            edifactWriter.startElement();
            edifactWriter.writeComponentString("67");
            edifactWriter.writeComponentString("20150815");
            edifactWriter.writeComponentString("102");
            edifactWriter.endElement();

            edifactWriter.startSegment("UNS");
            edifactWriter.writeElementString("S");
            edifactWriter.endElement();

            edifactWriter.createTransactionFooter();

            edifactWriter.createInterchangeFooter();
        } catch (IPWorksEDITranslatorException ex) {
            System.out.println("EDIFACT exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
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



