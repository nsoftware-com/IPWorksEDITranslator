<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks EDI Translator 2022 Demos - X12 Writer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks EDI Translator 2022 Demos - X12 Writer">
</head>

<body>

<div id="content">
<h1>IPWorks EDI Translator - Demo Pages</h1>
<h2>X12 Writer</h2>
<p>This demo shows how to use X12Writer to create X12 documents.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworkseditranslator_x12writer.php');
require_once('../include/ipworkseditranslator_const.php');

?>

<?php
//to print generated data to file, before calling the appropriate writeFile method, set
//x12writer1->setOutputFile("filename.txt");

$x12writer1 = new IPWorksEDITranslator_X12writer();

function writeFile_X12_810($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000006");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("IN");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("810");
  $x12writer1->doWriteElementString("810");
  $x12writer1->doWriteElementString("0001");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BIG");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doWriteElementString("3003014445");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("0476553272");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("DR");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CUR");
  $x12writer1->doWriteElementString("SE");
  $x12writer1->doWriteElementString("USD");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("8M");
  $x12writer1->doWriteElementString("0056");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doWriteElementString("Company");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("544380");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("CA");
  $x12writer1->doWriteElementString("Postal Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0607047800010");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("200131");
  $x12writer1->doWriteElementString("Country");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("RE");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("5095956");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("IL");
  $x12writer1->doWriteElementString("Postal Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/IT1");
  $x12writer1->doWriteElementString("20");
  $x12writer1->doWriteElementString("2500");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("36.96");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0594");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/REF_3");
  $x12writer1->doWriteElementString("KK");
  $x12writer1->doWriteElementString("0099778154");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/REF_3");
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("0476553272");
  $x12writer1->doWriteElementString("20");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TDS");
  $x12writer1->doWriteElementString("9240000");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_850($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000007");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("850");
  $x12writer1->doWriteElementString("850");
  $x12writer1->doWriteElementString("0001");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BEG");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("DS");
  $x12writer1->doWriteElementString("0476696888");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("SB");
  $x12writer1->doWriteElementString("ZZ11");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("6P");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("8M");
  $x12writer1->doWriteElementString("0056");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("CR");
  $x12writer1->doWriteElementString("1070335099");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("CO");
  $x12writer1->doWriteElementString("7109790082");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PER");
  $x12writer1->doWriteElementString("CN");
  $x12writer1->doWriteElementString("name");
  $x12writer1->doWriteElementString("TE");
  $x12writer1->doWriteElementString("Number");

  $x12writer1->doStartSegment("CSH");
  $x12writer1->doWriteElementString("BK");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SACLoop1/SAC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doWriteElementString("ZZZZ");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("06");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TD5");
  $x12writer1->doWriteElementString("Z");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N9Loop1/N9");
  $x12writer1->doWriteElementString("PD");
  $x12writer1->doWriteElementString("ZCOF");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N9Loop1/MSG");
  $x12writer1->doWriteElementString("Thanks!");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("5601");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("EN");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("OEM NAME");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0000505462");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N2");
  $x12writer1->doWriteElementString("additional name");
  $x12writer1->doWriteElementString(""); // not skipped because last element
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("201613");
  $x12writer1->doWriteElementString("CN");
  $x12writer1->doWriteElementString("SP");
  $x12writer1->doWriteElementString("020");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PO1");
  $x12writer1->doWriteElementString("00010");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("495");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("337S3744");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PIDLoop1/PID_2");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("Thanks!");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("EN");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF_7");
  $x12writer1->doWriteElementString("CO");
  $x12writer1->doWriteElementString("7109790082");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF_7");
  $x12writer1->doWriteElementString("LI");
  $x12writer1->doWriteElementString("000010");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/SCHLoop1/SCH");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("002");
  $x12writer1->doWriteElementString("20180708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTTLoop1/CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_855($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000008");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("PR");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("8");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("855");
  $x12writer1->doWriteElementString("855");
  $x12writer1->doWriteElementString("0013");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BAK");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("AT");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("4900043704");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PO1");
  $x12writer1->doWriteElementString("000010");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("14.00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0548");
  $x12writer1->doWriteElementString("VP");
  $x12writer1->doWriteElementString("Product");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF");
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF");
  $x12writer1->doWriteElementString("VN");
  $x12writer1->doWriteElementString("0025009879");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/ACKLoop1/ACK");
  $x12writer1->doWriteElementString("IA");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("067");
  $x12writer1->doWriteElementString("20150709");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTTLoop1/CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_856($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000009");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("SH");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("9");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("856");
  $x12writer1->doWriteElementString("856");
  $x12writer1->doWriteElementString("0029");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BSN");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("0403734501");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doWriteElementString("162859");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doWriteElementString("011");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("S");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/PRF");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/TD1");
  $x12writer1->doWriteElementString("CNT90");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/TD5");
  $x12writer1->doWriteElementString("O");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("FEDX");
  $x12writer1->doWriteElementString("A");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/REF");
  $x12writer1->doWriteElementString("BM");
  $x12writer1->doWriteElementString("EDITEST403734501");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/REF");
  $x12writer1->doWriteElementString("CR");
  $x12writer1->doWriteElementString("4900043704");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("O");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0042001808");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N1");
  $x12writer1->doWriteElementString("SF");
  $x12writer1->doWriteElementString("NameT");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("SG");
  $x12writer1->doWriteElementString("339942");
  $x12writer1->doWriteElementString("SG");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("3");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("I");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/LIN");
  $x12writer1->doWriteElementString("10");
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0548");
  $x12writer1->doWriteElementString("VP");
  $x12writer1->doWriteElementString("Product");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/SN1");
  $x12writer1->doWriteElementString("10");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/MAN");
  $x12writer1->doWriteElementString("CP");
  $x12writer1->doWriteElementString("Marks");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

if ($_SERVER['REQUEST_METHOD'] == "POST") {
  try{
    $x12writer1->doConfig("Encoding=iso-8859-1");
    $x12writer1->setSuffix(3); // CRLF
    $x12writer1->doLoadSchema(getcwd() . "/RSSBus_00401_810.json");
    if($_POST["docType"] == "810"){
      writeFile_X12_810($x12writer1);
    }else if($_POST["docType"] == "850"){
      writeFile_X12_850($x12writer1);
    }else if($_POST["docType"] == "855"){
      writeFile_X12_855($x12writer1);
    }else if($_POST["docType"] == "856"){
      writeFile_X12_856($x12writer1);
    }
    echo '<p>' . $x12writer1->getOutputData() . '</p>';
  }catch(Exception $ex){
    echo '<p>There was an issue. [' . $x12writer1->lastErrorCode() . ']: ' . $x12writer1->lastError() . '</p>';
  }
}

?>

<form method="POST">
<select name="docType">
  <option value="810">810</option>
  <option value="850">850</option>
  <option value="855">855</option>
  <option value="856">856</option>
  <input type="submit" name="write" value="Write Document" />
</form>


<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks EDI Translator objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-BKPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks EDI Translator 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-BKPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
