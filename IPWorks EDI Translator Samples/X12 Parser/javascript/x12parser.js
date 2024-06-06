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

let x12reader1;
main();

async function main() {
  const argv = process.argv;
  if (argv.length !== 3)
  {
    console.log("Usage: node x12parser.js  file");
    console.log("");
    console.log("  file the file to parse");
    console.log("\r\nExample: node x12parser.js ./x12.txt");
    process.exit();
  }

  x12reader1 = new ipworkseditranslator.x12reader();
  x12reader1.on("EndFunctionalGroup", function(e){
    console.log("EndFunctionalGroup: " + e.tag);
  })
  .on("EndInterchange", function(e){
    console.log("EndInterchange: " + e.tag);
  })
  .on("EndLoop", function(e){
    console.log("EndLoop");
  })
  .on("EndTransaction", function(e){
    console.log("EndTransaction: " + e.tag);
  })
  .on("Error", function(e){
    console.log("ERROR: " + e.errorCode + ":" + e.description);
  })
  .on("ResolveSchema", function(e){
    console.log("ResolveSchema: " + e.transactionCode);
  })
  .on("Segment", function(e){
    console.log("Segment: " + e.name);
  })
  .on("StartFunctionalGroup", function(e){
    console.log("StartFunctionalGroup: " + e.tag);
  })
  .on("StartInterchange", function(e){
    console.log("StartInterchange: " + e.tag);
  })
  .on("StartLoop", function(e){
    console.log("StartLoop: " + e.name);
  })
  .on("StartTransaction", function(e){
    console.log("StartTransaction: " + e.tag);
  })
  .on("Warning", function(e){
    console.log("WARNING: " + e.warnCode + ": " + e.message);
  });

  x12reader1.config("Encoding=ISO-8859-1");

  x12reader1.setSchemaFormat(6); // schema JSON  = 6
  x12reader1.loadSchema('./RSSBus_00401_810.json');
  x12reader1.setInputFile(argv[2]);	
  
  await x12reader1.parse().catch((err) => {
    console.log("Error: "+err.message);
    process.exit();
  });
  
  process.stdout.write("\r\nDone parsing")
  process.exit();
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
