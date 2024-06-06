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
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class x12parser extends ConsoleDemo {
  public static void main(String[] args) {
      
      if (args.length != 1) {		
            
            System.out.println("usage: x12parser file");
            System.out.println("");
            System.out.println("  file the x12 file to parse");
            System.out.println("\r\nExample: x12parser ./x12.txt");
            
        } else {
            X12Reader x12reader1 = new X12Reader();

            try {
                
              System.out.println("****************************************************************");
              System.out.println("* This demo shows how to use the X12Reader component to parse  *");
              System.out.println("* an X12 document. A sample X12 document is provided as well.  *");
              System.out.println("****************************************************************");
                
              x12reader1.addX12ReaderEventListener(new ipworkseditranslator.DefaultX12ReaderEventListener(){
                  public void endFunctionalGroup(X12ReaderEndFunctionalGroupEvent e){
                      System.out.println("EndFunctionalGroup: " + e.tag);
                  }
                  public void endInterchange(X12ReaderEndInterchangeEvent e){
                      System.out.println("EndInterchange: " + e.tag);
                  }
                  public void endLoop(X12ReaderEndLoopEvent e){
                      System.out.println("EndLoop");
                  }
                  public void endTransaction(X12ReaderEndTransactionEvent e){
                      System.out.println("EndTransaction: " + e.tag);
                  }
                  public void error(X12ReaderErrorEvent e){
                      System.out.println("ERROR: " + e.errorCode + ":" + e.description);
                  }
                  public void resolveSchema(X12ReaderResolveSchemaEvent e){
                      System.out.println("ResolveSchema: " + e.transactionCode);
                  }
                  public void segment(X12ReaderSegmentEvent e){
                      System.out.println("Segment: " + e.name);
                  }
                  public void startFunctionalGroup(X12ReaderStartFunctionalGroupEvent e){
                      System.out.println("StartFunctionalGroup: " + e.tag);
                  }
                  public void startInterchange(X12ReaderStartInterchangeEvent e){
                      System.out.println("StartInterchange: " + e.tag);
                  }
                  public void startLoop(X12ReaderStartLoopEvent e){
                      System.out.println("StartLoop: " + e.name);
                  }
                  public void startTransaction(X12ReaderStartTransactionEvent e){
                      System.out.println("StartTransaction: " + e.tag);
                  }
                  public void warning(X12ReaderWarningEvent e){
                      System.out.println("WARNING: " + e.warnCode + ": " + e.message);
                  }
              });
              
              x12reader1.config("Encoding=iso-8859-1");
              x12reader1.setSchemaFormat(X12Reader.schemaJSON);
              x12reader1.loadSchema("./RSSBus_00401_810.json");
             
             //This demo provides information about the parsed document through the events.
             //To navigate the document using the XPath property first set:
             //x12reader1.config("ResolveXPathOnSet=true");

              x12reader1.setInputFile(args[0]);
              x12reader1.parse();
              
            } catch (Exception ex) {
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



