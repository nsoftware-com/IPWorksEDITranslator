/*
 * IPWorks EDI Translator 2022 JavaScript Edition - Sample Project
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

let x12writer1;
main();

async function main() {
	const argv = process.argv;
	if(argv.length !== 3 || (argv[2]!=="810" && argv[2]!=="850" && argv[2]!=="855" && argv[2]!=="856")) {
		console.log("Usage: node x12writer.js standard doctype");
		console.log("");
		console.log("  X12 document type to generate- [810, 850, 855, 856]");
		console.log("\r\nExamples: node x12writer.js 810");
		process.exit();
	}

	x12writer1 = new ipworkseditranslator.x12writer();

	x12writer1.config("Encoding=ISO-8859-1", function(err,data){
		if(err) {
			console.log(err);
			return;
		}
		console.log(data);
		return
		
	});

	x12writer1.setSchemaFormat(6);

	x12writer1.setSuffix(3);

	x12writer1.loadSchema('./RSSBus_00401_' + argv[2] + '.json');

	//write chosen file type
	if (argv[2] === "810") {
		await writeFile_X12_810();
	} else if(argv[2]==="850") {
		await writeFile_X12_850();
	} else if(argv[2]==="855") {
		await writeFile_X12_855();
	} else if (argv[2] === "856") {
		await writeFile_X12_856();
	}

	//print generated data to console
	console.log(x12writer1.getOutputData().toString());

	//to print generated data to file, before calling the appropriate writeFile method, set
	//x12writer1.setOutputFile("filename.txt");

	process.exit();
}

async function writeFile_X12_810() {
  await x12writer1.startInterchangeHeader("004010");
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("U");
  await x12writer1.writeElementString("00401");
  await x12writer1.writeElementString("000000006");
  await x12writer1.writeElementString("0");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString(">");
  await x12writer1.endElement();

  await x12writer1.startFunctionalGroupHeader();
  await x12writer1.writeElementString("IN");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("20160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("6");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString("004010");
  await x12writer1.endElement();

  await x12writer1.startTransactionHeader("810");
  await x12writer1.writeElementString("810");
  await x12writer1.writeElementString("0001");
  await x12writer1.endElement();

  await x12writer1.startSegment("BIG");
  await x12writer1.writeElementString("20150708");
  await x12writer1.writeElementString("3003014445");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("0476553272");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("DR");
  await x12writer1.endElement();

  await x12writer1.startSegment("CUR");
  await x12writer1.writeElementString("SE");
  await x12writer1.writeElementString("USD");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("8M");
  await x12writer1.writeElementString("0056");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("BY");
  await x12writer1.writeElementString("Company");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("544380");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N3");
  await x12writer1.writeElementString("Address");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N4");
  await x12writer1.writeElementString("City");
  await x12writer1.writeElementString("CA");
  await x12writer1.writeElementString("Postal Code");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("ST");
  await x12writer1.writeElementString("Name");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("0607047800010");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N3");
  await x12writer1.writeElementString("Address");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N4");
  await x12writer1.writeElementString("City");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("200131");
  await x12writer1.writeElementString("Country");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("RE");
  await x12writer1.writeElementString("Name");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("5095956");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N3");
  await x12writer1.writeElementString("Address");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N4");
  await x12writer1.writeElementString("City");
  await x12writer1.writeElementString("IL");
  await x12writer1.writeElementString("Postal Code");
  await x12writer1.endElement();

  await x12writer1.startSegment("IT1Loop1/IT1");
  await x12writer1.writeElementString("20");
  await x12writer1.writeElementString("2500");
  await x12writer1.writeElementString("EA");
  await x12writer1.writeElementString("36.96");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("BP");
  await x12writer1.writeElementString("335S0594");
  await x12writer1.endElement();

  await x12writer1.startSegment("IT1Loop1/REF_3");
  await x12writer1.writeElementString("KK");
  await x12writer1.writeElementString("0099778154");
  await x12writer1.endElement();

  await x12writer1.startSegment("IT1Loop1/REF_3");
  await x12writer1.writeElementString("PO");
  await x12writer1.writeElementString("0476553272");
  await x12writer1.writeElementString("20");
  await x12writer1.endElement();

  await x12writer1.startSegment("TDS");
  await x12writer1.writeElementString("9240000");
  await x12writer1.endElement();

  await x12writer1.startSegment("CTT");
  await x12writer1.writeElementString("1");
  await x12writer1.endElement();

  await x12writer1.createTransactionFooter();

  await x12writer1.createFunctionalGroupFooter();

  await x12writer1.createInterchangeFooter();
}

async function writeFile_X12_850() {
  await x12writer1.startInterchangeHeader("004010");
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("U");
  await x12writer1.writeElementString("00401");
  await x12writer1.writeElementString("000000007");
  await x12writer1.writeElementString("0");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString(">");
  await x12writer1.endElement();

  await x12writer1.startFunctionalGroupHeader();
  await x12writer1.writeElementString("PO");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("20160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("7");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString("004010");
  await x12writer1.endElement();

  await x12writer1.startTransactionHeader("850");
  await x12writer1.writeElementString("850");
  await x12writer1.writeElementString("0001");
  await x12writer1.endElement();

  await x12writer1.startSegment("BEG");
  await x12writer1.writeElementString("00");
  await x12writer1.writeElementString("DS");
  await x12writer1.writeElementString("0476696888");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("20150708");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("SB");
  await x12writer1.writeElementString("ZZ11");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("6P");
  await x12writer1.writeElementString("ZZ");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("8M");
  await x12writer1.writeElementString("0056");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("CR");
  await x12writer1.writeElementString("1070335099");
  await x12writer1.endElement();

  await x12writer1.startSegment("REF");
  await x12writer1.writeElementString("CO");
  await x12writer1.writeElementString("7109790082");
  await x12writer1.endElement();

  await x12writer1.startSegment("PER");
  await x12writer1.writeElementString("CN");
  await x12writer1.writeElementString("name");
  await x12writer1.writeElementString("TE");
  await x12writer1.writeElementString("Number");

  await x12writer1.startSegment("CSH");
  await x12writer1.writeElementString("BK");
  await x12writer1.endElement();

  await x12writer1.startSegment("SACLoop1/SAC");
  await x12writer1.writeElementString("C");
  await x12writer1.writeElementString("ZZZZ");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("06");
  await x12writer1.endElement();

  await x12writer1.startSegment("TD5");
  await x12writer1.writeElementString("Z");
  await x12writer1.writeElementString("2");
  await x12writer1.writeElementString("Code");
  await x12writer1.endElement();

  await x12writer1.startSegment("N9Loop1/N9");
  await x12writer1.writeElementString("PD");
  await x12writer1.writeElementString("ZCOF");
  await x12writer1.endElement();

  await x12writer1.startSegment("N9Loop1/MSG");
  await x12writer1.writeElementString("Thanks!");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("BY");
  await x12writer1.writeElementString("Name");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("5601");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("EN");
  await x12writer1.writeElementString("Name");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N1");
  await x12writer1.writeElementString("ST");
  await x12writer1.writeElementString("OEM NAME");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("0000505462");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N2");
  await x12writer1.writeElementString("additional name");
  await x12writer1.writeElementString(""); // not skipped because last
                    // element
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N3");
  await x12writer1.writeElementString("Address");
  await x12writer1.writeElementString("Address");
  await x12writer1.endElement();

  await x12writer1.startSegment("N1Loop1/N4");
  await x12writer1.writeElementString("City");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("201613");
  await x12writer1.writeElementString("CN");
  await x12writer1.writeElementString("SP");
  await x12writer1.writeElementString("020");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/PO1");
  await x12writer1.writeElementString("00010");
  await x12writer1.writeElementString("500000");
  await x12writer1.writeElementString("EA");
  await x12writer1.writeElementString("495");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("BP");
  await x12writer1.writeElementString("337S3744");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/PIDLoop1/PID_2");
  await x12writer1.writeElementString("F");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("Thanks!");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("EN");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/REF_7");
  await x12writer1.writeElementString("CO");
  await x12writer1.writeElementString("7109790082");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/REF_7");
  await x12writer1.writeElementString("LI");
  await x12writer1.writeElementString("000010");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/SCHLoop1/SCH");
  await x12writer1.writeElementString("500000");
  await x12writer1.writeElementString("EA");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("002");
  await x12writer1.writeElementString("20180708");
  await x12writer1.endElement();

  await x12writer1.startSegment("CTTLoop1/CTT");
  await x12writer1.writeElementString("1");
  await x12writer1.writeElementString("500000");
  await x12writer1.endElement();

  await x12writer1.createTransactionFooter();

  await x12writer1.createFunctionalGroupFooter();

  await x12writer1.createInterchangeFooter();
}

async function writeFile_X12_855() {
  await x12writer1.startInterchangeHeader("004010");
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("U");
  await x12writer1.writeElementString("00401");
  await x12writer1.writeElementString("000000008");
  await x12writer1.writeElementString("0");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString(">");
  await x12writer1.endElement();

  await x12writer1.startFunctionalGroupHeader();
  await x12writer1.writeElementString("PR");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("20160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("8");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString("004010");
  await x12writer1.endElement();

  await x12writer1.startTransactionHeader("855");
  await x12writer1.writeElementString("855");
  await x12writer1.writeElementString("0013");
  await x12writer1.endElement();

  await x12writer1.startSegment("BAK");
  await x12writer1.writeElementString("00");
  await x12writer1.writeElementString("AT");
  await x12writer1.writeElementString("0476553696");
  await x12writer1.writeElementString("20150708");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("4900043704");
  await x12writer1.writeElementString("20150708");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/PO1");
  await x12writer1.writeElementString("000010");
  await x12writer1.writeElementString("1100");
  await x12writer1.writeElementString("EA");
  await x12writer1.writeElementString("14.00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("BP");
  await x12writer1.writeElementString("335S0548");
  await x12writer1.writeElementString("VP");
  await x12writer1.writeElementString("Product");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/REF");
  await x12writer1.writeElementString("PO");
  await x12writer1.writeElementString("0476553696");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/REF");
  await x12writer1.writeElementString("VN");
  await x12writer1.writeElementString("0025009879");
  await x12writer1.endElement();

  await x12writer1.startSegment("PO1Loop1/ACKLoop1/ACK");
  await x12writer1.writeElementString("IA");
  await x12writer1.writeElementString("1100");
  await x12writer1.writeElementString("EA");
  await x12writer1.writeElementString("067");
  await x12writer1.writeElementString("20150709");
  await x12writer1.endElement();

  await x12writer1.startSegment("CTTLoop1/CTT");
  await x12writer1.writeElementString("1");
  await x12writer1.writeElementString("1100");
  await x12writer1.endElement();

  await x12writer1.createTransactionFooter();

  await x12writer1.createFunctionalGroupFooter();

  await x12writer1.createInterchangeFooter();
}

async function writeFile_X12_856() {
  await x12writer1.startInterchangeHeader("004010");
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("00");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("ZZ");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("U");
  await x12writer1.writeElementString("00401");
  await x12writer1.writeElementString("000000009");
  await x12writer1.writeElementString("0");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString(">");
  await x12writer1.endElement();

  await x12writer1.startFunctionalGroupHeader();
  await x12writer1.writeElementString("SH");
  await x12writer1.writeElementString("ACME");
  await x12writer1.writeElementString("WAYNE_TECH");
  await x12writer1.writeElementString("20160707");
  await x12writer1.writeElementString("1544");
  await x12writer1.writeElementString("9");
  await x12writer1.writeElementString("T");
  await x12writer1.writeElementString("004010");
  await x12writer1.endElement();

  await x12writer1.startTransactionHeader("856");
  await x12writer1.writeElementString("856");
  await x12writer1.writeElementString("0029");
  await x12writer1.endElement();

  await x12writer1.startSegment("BSN");
  await x12writer1.writeElementString("00");
  await x12writer1.writeElementString("0403734501");
  await x12writer1.writeElementString("20150708");
  await x12writer1.writeElementString("162859");
  await x12writer1.endElement();

  await x12writer1.startSegment("DTM");
  await x12writer1.writeElementString("011");
  await x12writer1.writeElementString("20150708");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/HL");
  await x12writer1.writeElementString("1");
  await x12writer1.skipElement();
  await x12writer1.writeElementString("S");
  await x12writer1.writeElementString("1");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/PRF");
  await x12writer1.writeElementString("0476553696");
  await x12writer1.skipElement();
  await x12writer1.skipElement();
  await x12writer1.writeElementString("20150708");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/TD1");
  await x12writer1.writeElementString("CNT90");
  await x12writer1.writeElementString("0");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/TD5");
  await x12writer1.writeElementString("O");
  await x12writer1.writeElementString("2");
  await x12writer1.writeElementString("FEDX");
  await x12writer1.writeElementString("A");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/REF");
  await x12writer1.writeElementString("BM");
  await x12writer1.writeElementString("EDITEST403734501");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/REF");
  await x12writer1.writeElementString("CR");
  await x12writer1.writeElementString("4900043704");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/HL");
  await x12writer1.writeElementString("2");
  await x12writer1.writeElementString("1");
  await x12writer1.writeElementString("O");
  await x12writer1.writeElementString("1");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/N1Loop1/N1");
  await x12writer1.writeElementString("ST");
  await x12writer1.writeElementString("Name");
  await x12writer1.writeElementString("92");
  await x12writer1.writeElementString("0042001808");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/N1Loop1/N1");
  await x12writer1.writeElementString("SF");
  await x12writer1.writeElementString("NameT");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/N1Loop1/N3");
  await x12writer1.writeElementString("Address");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/N1Loop1/N4");
  await x12writer1.writeElementString("City");
  await x12writer1.writeElementString("SG");
  await x12writer1.writeElementString("339942");
  await x12writer1.writeElementString("SG");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/HL");
  await x12writer1.writeElementString("3");
  await x12writer1.writeElementString("2");
  await x12writer1.writeElementString("I");
  await x12writer1.writeElementString("0");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/LIN");
  await x12writer1.writeElementString("10");
  await x12writer1.writeElementString("BP");
  await x12writer1.writeElementString("335S0548");
  await x12writer1.writeElementString("VP");
  await x12writer1.writeElementString("Product");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/SN1");
  await x12writer1.writeElementString("10");
  await x12writer1.writeElementString("1100");
  await x12writer1.writeElementString("EA");
  await x12writer1.endElement();

  await x12writer1.startSegment("HLLoop1/MAN");
  await x12writer1.writeElementString("CP");
  await x12writer1.writeElementString("Marks");
  await x12writer1.endElement();

  await x12writer1.startSegment("CTT");
  await x12writer1.writeElementString("1");
  await x12writer1.endElement();

  await x12writer1.createTransactionFooter();

  await x12writer1.createFunctionalGroupFooter();

  await x12writer1.createInterchangeFooter();
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
