/*
 * IPWorks EDI Translator 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworkseditranslator = require("@nsoftware/ipworkseditranslator");

if(!ipworkseditranslator) {
  console.error("Cannot find ipworkseditranslator.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

let edireader1;
main();

async function main() {
  const argv = process.argv;
  if(argv.length != 3) {
    console.log("Usage: node edifactparser.js  file");
    console.log("");
    console.log("  file the file to parse");
    console.log("\r\nExample: node edifactparser.js ./INVOIC.edi");
    process.exit();
  }

  var edireader1 = new ipworkseditranslator.edifactreader();

  edireader1.on("EndFunctionalGroup", function(e){
    console.log("EndFunctionalGroup: " + e.tag);
  });
  
  edireader1.on("EndInterchange", function(e){
    console.log("EndInterchange: " + e.tag);
  });
  
  edireader1.on("EndLoop", function(e){
    console.log("EndLoop");
  });
  
  edireader1.on("EndTransaction", function(e){
    console.log("EndTransaction: " + e.tag);
  });
  
  edireader1.on("Error", function(e){
    console.log("ERROR: " + e.errorCode + ":" + e.description);
  });
  
  edireader1.on("ResolveSchema", function(e){
    console.log("ResolveSchema: " + e.transactionCode);
  });
  
  edireader1.on("Segment", function(e){
    console.log("Segment: " + e.name);
  });
  
  edireader1.on("StartFunctionalGroup", function(e){
    console.log("StartFunctionalGroup: " + e.tag);
  });
  
  edireader1.on("StartInterchange", function(e){
    console.log("StartInterchange: " + e.tag);
  });
  
  edireader1.on("StartLoop", function(e){
    console.log("StartLoop: " + e.name);
  });
  
  edireader1.on("StartTransaction", function(e){
    console.log("StartTransaction: " + e.tag);
  });
  
  edireader1.on("Warning", function(e){
    console.log("WARNING: " + e.warnCode + ": " + e.message);
  });
  
  edireader1.config("Encoding=ISO-8859-1");
  edireader1.setSchemaFormat(6);
  await edireader1.loadSchema("./RSSBus_D97A_INVOIC.json");
       
  edireader1.setInputFile(argv[2]);
  
  try {
    await edireader1.parse();
  } catch (err) {
    console.log(err);
    process.exit();
  }
  console.log("Parsing complete.");
  process.exit();
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
