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

main();

async function main() {
	const argv = process.argv;

	if (argv.length !== 5) {
		console.log("usage: node edifacttranslator.js translationMode fileToRead fileToWrite");
		console.log("");
		console.log("  translationMode  1 to translate EDI to XML or 2 to translate XML to EDI");
		console.log("  fileToRead       the file to tranlsate");
		console.log("  fileToWrite      the file to which the translated data will be written");
		console.log("\r\nExample: node edifacttranslator.js 1 ./invoic.edi ./INVOICE.xml");
		console.log("\r\nExample: node edifacttranslator.js 2 ./INVOICE.xml INVOIC_fromxml.txt");
		process.exit();
	} else {
		let edifacttranslator1 = new ipworkseditranslator.edifacttranslator();
		console.log("This demo demonstrates the translation of EDIFACT files to XML and vice-versa through the use of the EDIFACTranslator component.\n");
		try {
			switch (argv[2]) {
				case "1":
					console.log("Converting EDIFACT File to XML File.");
					await edifacttranslator1.reset();
					await edifacttranslator1.loadSchema("./RSSBus_D97A_INVOIC.json");
					edifacttranslator1.setInputFile(argv[3]);
					edifacttranslator1.setOutputFile(argv[4]);
					edifacttranslator1.setOverwrite(true);
					await edifacttranslator1.translate();
					console.log("Translation complete.");
					process.exit();
					break;
				case "2":
					console.log("Converting XML File to EDIFACT File.");
					await edifacttranslator1.reset();
					edifacttranslator1.setInputFormat(0);
					edifacttranslator1.setOutputFormat(2);
					edifacttranslator1.setInputFile(argv[3]);
					edifacttranslator1.setOutputFile(argv[4]);
					edifacttranslator1.setOverwrite(true);
					await edifacttranslator1.translate();
					console.log("Translation complete.");
					process.exit();
					break;
				default:
					console.log("Unrecognized option.");
					break;
			}
		} catch (ex) {
			console.error(ex.message);
			process.exit();
		}
	}
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
