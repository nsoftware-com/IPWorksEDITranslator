<?php
/*
 * IPWorks EDI Translator 2024 PHP Edition - Sample Project
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
require_once('../include/ipworkseditranslator_edifactreader.php');
require_once('../include/ipworkseditranslator_edifactwriter.php');
require_once('../include/ipworkseditranslator_const.php');
?>
<?php
class MyEdifactReader extends IPWorksEDITranslator_EDIFACTReader {
  function FireEndFunctionalGroup($param) {
    echo "EndFunctionalGroup: " . $param['tag'] . "\n";
  }

  function FireEndInterchange($param) {
    echo "EndInterchange: " . $param['tag'] . "\n";
  }
  
  function FireEndLoop($param) {
    echo "EndLoop\n";
  }
  
  function FireEndTransaction($param) {
    echo "EndTransaction: " . $param['tag'] . "\n";
  }
  
  function FireError($param) {
    echo "Error [" . $param['errorcode'] . "] " . $param['description'] . "\n";
  }
  
  function FireResolveSchema($param) {
    echo "ResolveSchema: " . $param['transactioncode'] . "\n";
  }
  
  function FireSegment($param) {
    echo "Segment: " . $param['name'] . "\n";
  }
  
  function FireStartFunctionalGroup($param) {
    echo "StartFunctionalGroup: " . $param['tag'] . "\n";
  }
  
  function FireStartInterchange($param) {
    echo "StartInterchange: " . $param['tag'] . "\n";
  }
  
  function FireStartLoop($param) {
    echo "StartLoop: " . $param['name'] . "\n";
  }
  function FireStartTransaction($param) {
    echo "StartTransaction: " . $param['tag'] . "\n";
  }
  
  function FireWarning($param) {
    echo "Warning: [" . $param['warncode'] . "] " . $param['message'] . "\n";
  }
};

try {
  if ($argc < 3) {
    echo "usage: php edifact_parser.php -f file [-s schema]\n\n";
    echo "  file         the EDIFACT file to parse\n";
    echo "  schema       the schema file to use when parsing the EDIFACT document (optional)\n";
    echo "\nExample: php edifact_parser.php -f INVOIC.edi -s RSSBus_D97A_INVOIC.json\n";
    return;
  }

  $edifactreader1 = new MyEdifactReader();

  for ($i = 0; $i < $argc; $i++) {
    if (str_starts_with($argv[$i], "-")) {
      if ($argv[$i] == "-f") {
        $edifactreader1->setInputFile($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") {
        $edifactreader1->doLoadSchema($argv[$i + 1]);
      }
    }
  }

  $edifactreader1->doParse();

} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>