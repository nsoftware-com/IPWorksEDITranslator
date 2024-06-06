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

let edifactwriter1;
main();

async function main() {
	const argv = process.argv;
	if (argv.length !== 3 ||
			(argv[2] !== "DESADV" && argv[2] !== "INVOIC" && argv[2] !== "ORDERS" && argv[2] !== "ORDRSP")
	) {
			console.log("Usage: node edifactwriter.js doctype");
			console.log("");
			console.log("  doctype the document type to generate: [DESADV, INVOIC, ORDERS, ORDRSP]");
			console.log("\r\nExamples: node edifactwriter.js DESADV");
			console.log("          node edifactwriter.js INVOIC");
			process.exit();
	}

	edifactwriter1 = new ipworkseditranslator.edifactwriter();

	edifactwriter1.config("Encoding=ISO-8859-1");
	edifactwriter1.setSchemaFormat(6);

	edifactwriter1.setSuffix(3); // Suffix CRLF
	edifactwriter1.loadSchema('./RSSBus_D97A_' + argv[2] + '.json');

  //edifactwriter1.setOutputFile("../" + argv[2] + ".edi");

	//write chosen file type
	if (argv[2] === "DESADV") {
			await writeFile_EDIFACT_DESADV();
	} else if (argv[2] === "INVOIC") {
			await writeFile_EDIFACT_INVOIC();
	} else if (argv[2] === "ORDERS") {
			await writeFile_EDIFACT_ORDERS();
	} else if (argv[2] === "ORDRSP") {
			await writeFile_EDIFACT_ORDRSP();
	}

	//print generated data to console
	console.log(edifactwriter1.getOutputData().toString());

	//to print generated data to file, before calling the appropriate writeFile method, set
	//edifactwriter1.setOutputFile("filename.txt");
	process.exit();
}

async function writeFile_EDIFACT_DESADV() {
    await edifactwriter1.startInterchangeHeader("D97A");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("UNOB");
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("WAYNE_TECH");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ACME");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("160707");
    await edifactwriter1.writeComponentString("1547");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("000000001");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("1234");
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startTransactionHeader("DESADV");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("DESADV");
    await edifactwriter1.writeComponentString("D");
    await edifactwriter1.writeComponentString("97A");
    await edifactwriter1.writeComponentString("UN");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("BGM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("351");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2014/10093");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("9");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("137");
    await edifactwriter1.writeComponentString("201404192036");
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("201404192036");
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MEA");
    await edifactwriter1.writeElementString("AAX");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("SQ");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("C62");
    await edifactwriter1.writeComponentString("17");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("ST");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0018");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("SU");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2019813");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TDTLoop1/TDT");
    await edifactwriter1.writeElementString("12");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("M");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("CARRIER");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("86");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("EQDLoop1/EQD");
    await edifactwriter1.writeElementString("TE");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("X");
    await edifactwriter1.endElement();
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/CPS");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/PACLoop1/PAC");
    await edifactwriter1.writeElementString("4");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("BOX-001");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/PACLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("52");
    await edifactwriter1.writeComponentString("50");
    await edifactwriter1.writeComponentString("C62");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/CPS");
    await edifactwriter1.writeElementString("2");
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/PACLoop1/PAC");
    await edifactwriter1.writeElementString("2");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("BOX-002");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/PACLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("52");
    await edifactwriter1.writeComponentString("100");
    await edifactwriter1.writeComponentString("C62");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/LINLoop1/LIN");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("9001");
    await edifactwriter1.writeComponentString("IN");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CPSLoop1/LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("12");
    await edifactwriter1.writeComponentString("400");
    await edifactwriter1.writeComponentString("C62");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("RFFLoop1/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ON");
    await edifactwriter1.writeComponentString("N55109001");
    await edifactwriter1.endElement();

    await edifactwriter1.createTransactionFooter();

    await edifactwriter1.createInterchangeFooter();
}

async function writeFile_EDIFACT_INVOIC() {
    await edifactwriter1.startInterchangeHeader("D97A");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("UNOB");
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("WAYNE_TECH");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ACME");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("160707");
    await edifactwriter1.writeComponentString("1547");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("000000002");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("1234");
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startTransactionHeader("INVOIC");
    await edifactwriter1.writeElementString("509010117");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INVOIC");
    await edifactwriter1.writeComponentString("D");
    await edifactwriter1.writeComponentString("97A");
    await edifactwriter1.writeComponentString("UN");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("BGM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("380");
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("TAX INVOICE");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0013550417");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("9");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("3");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("4");
    await edifactwriter1.writeComponentString("20061123");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("FTX");
    await edifactwriter1.writeElementString("AAI");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("10072.14");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CUXLoop1/CUX");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("EUR");
    await edifactwriter1.writeComponentString("4");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("0.67529");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("PATLoop1/PAT");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("PATLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("10");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("PATLoop1/PCD");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("0");
    await edifactwriter1.writeComponentString("13");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000030");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2265S13");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5029766832002");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("50.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("19150.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("383.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("19150.45");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.45");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.45");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000040");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2269F22");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5051254078241");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("20.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("21060.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("1053.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("21060.50");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.50");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.50");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000170");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2269F10");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5051254078326");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("10.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("6950.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("695.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("6950.16");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.16");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.16");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000190");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2269F26");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5051254051190");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("5.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("2375.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("475.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("2375.06");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.06");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.06");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000200");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2265S24");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5029766000685");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("3.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("957.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("319.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("957.02");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.02");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.02");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000210");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2263T95");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5029766699575");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("3.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("2085.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("695.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("2085.05");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.05");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.05");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("000250");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2269F15");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("5051254080091");
    await edifactwriter1.writeComponentString("UP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.writeElementString("");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("47");
    await edifactwriter1.writeComponentString("3.000");
    await edifactwriter1.writeComponentString("EA");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("11");
    await edifactwriter1.writeComponentString("20070926");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("203");
    await edifactwriter1.writeComponentString("4977.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INV");
    await edifactwriter1.writeComponentString("1659.00");
    await edifactwriter1.writeComponentString("TU");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("4977.12");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0.12");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("0.12");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("UNS");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CNT");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("4");
    await edifactwriter1.writeComponentString("7");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("9");
    await edifactwriter1.writeComponentString("67627.50");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("MOALoop4/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("79");
    await edifactwriter1.writeComponentString("57554.00");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/TAX");
    await edifactwriter1.writeElementString("7");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VAT");
    await edifactwriter1.endElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("17.500");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("125");
    await edifactwriter1.writeComponentString("57555.36");
    await edifactwriter1.writeComponentString("EUR");
    await edifactwriter1.writeComponentString("3");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("124");
    await edifactwriter1.writeComponentString("10072.14");
    await edifactwriter1.writeComponentString("EUR");
    await edifactwriter1.writeComponentString("3");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("ALCLoop1/ALC");
    await edifactwriter1.writeElementString("C");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("1.36");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FC");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("TAXLoop1/MOA");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("8");
    await edifactwriter1.writeComponentString("1.36");
    await edifactwriter1.endElement();

    await edifactwriter1.createTransactionFooter();

    await edifactwriter1.createInterchangeFooter();
}

async function writeFile_EDIFACT_ORDERS() {
    await edifactwriter1.startInterchangeHeader("D97A");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("UNOB");
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("WAYNE_TECH");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ACME");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("160707");
    await edifactwriter1.writeComponentString("1547");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("000000003");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("1234");
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startTransactionHeader("ORDERS");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ORDERS");
    await edifactwriter1.writeComponentString("D");
    await edifactwriter1.writeComponentString("97A");
    await edifactwriter1.writeComponentString("UN");
    await edifactwriter1.writeComponentString("ED17A1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("BGM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("105");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("K12345");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("9");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("137");
    await edifactwriter1.writeComponentString("19980626");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("FTX");
    await edifactwriter1.writeElementString("GEN");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("FREE TEXT");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("RFFLoop1/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("PS");
    await edifactwriter1.writeComponentString("10501");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("RFFLoop1/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.writeComponentString("NO");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("BY");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/RFFLoop2/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("VA");
    await edifactwriter1.writeComponentString("GB107328000");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("SE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CUXLoop1/CUX");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("GBP");
    await edifactwriter1.writeComponentString("9");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("001");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0000057G3454");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("DESCRIPTION");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INF");
    await edifactwriter1.writeComponentString("27.54");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("9829");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("JB");
    await edifactwriter1.writeComponentString("JOB NO");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("19980717");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("002");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0000057G3454");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("DESCRIPTION");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("4000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INF");
    await edifactwriter1.writeComponentString("27.54");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("9830");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("JB");
    await edifactwriter1.writeComponentString("JOB NO");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("4000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("19980724");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("003");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("0000057G3454");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/IMD");
    await edifactwriter1.writeElementString("F");
    await edifactwriter1.skipElement();
    await edifactwriter1.startElement();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("DESCRIPTION");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("3000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("INF");
    await edifactwriter1.writeComponentString("27.54");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("9831");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("JB");
    await edifactwriter1.writeComponentString("JOB NO");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("21");
    await edifactwriter1.writeComponentString("3000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("19980731");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("UNS");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.createTransactionFooter();

    await edifactwriter1.createInterchangeFooter();
}

async function writeFile_EDIFACT_ORDRSP() {
    await edifactwriter1.startInterchangeHeader("D97A");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("UNOB");
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("WAYNE_TECH");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ACME");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("160707");
    await edifactwriter1.writeComponentString("1547");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("000000004");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("1234");
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.skipElement();
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startTransactionHeader("ORDRSP");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ORDRSP");
    await edifactwriter1.writeComponentString("D");
    await edifactwriter1.writeComponentString("97A");
    await edifactwriter1.writeComponentString("UN");
    await edifactwriter1.writeComponentString("EDOR04");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("BGM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("231");
    await edifactwriter1.endElement();
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("1109706372/3");
    await edifactwriter1.endElement();
    await edifactwriter1.writeElementString("9");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("137");
    await edifactwriter1.writeComponentString("20150708");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("RFFLoop1/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("ON");
    await edifactwriter1.writeComponentString("INCG14040002");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("SE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("NADLoop1/NAD");
    await edifactwriter1.writeElementString("BY");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("CUXLoop1/CUX");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("USD");
    await edifactwriter1.writeComponentString("4");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("800");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.9600");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("800");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20140401");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150729");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("2");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.9600");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20141020");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150729");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("3");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRFS4115PBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.9600");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("3");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20141120");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150809");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("4");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("4000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.1000");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("4");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("4000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20140605");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150810");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("5");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("12000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.1000");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("5");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("12000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20140705");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150801");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/LIN");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.writeElementString("6");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("VP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("91");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PIA");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("IRLR8259TRPBF");
    await edifactwriter1.writeComponentString("BP");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("92");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("12000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/PRILoop1/PRI");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("AAA");
    await edifactwriter1.writeComponentString("0.1000");
    await edifactwriter1.writeComponentString("CT");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("1");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("LINLoop1/RFFLoop3/RFF");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("LI");
    await edifactwriter1.skipComponent();
    await edifactwriter1.writeComponentString("6");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("10000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20140805");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150805");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/SCC");
    await edifactwriter1.writeElementString("1");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/QTY");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("113");
    await edifactwriter1.writeComponentString("2000");
    await edifactwriter1.writeComponentString("PCE");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("2");
    await edifactwriter1.writeComponentString("20140805");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("SCCLoop1/QTYLoop1/DTM");
    await edifactwriter1.startElement();
    await edifactwriter1.writeComponentString("67");
    await edifactwriter1.writeComponentString("20150815");
    await edifactwriter1.writeComponentString("102");
    await edifactwriter1.endElement();

    await edifactwriter1.startSegment("UNS");
    await edifactwriter1.writeElementString("S");
    await edifactwriter1.endElement();

    await edifactwriter1.createTransactionFooter();

    await edifactwriter1.createInterchangeFooter();
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
