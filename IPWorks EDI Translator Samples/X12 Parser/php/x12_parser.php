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
require_once('../include/ipworkseditranslator_x12reader.php');
require_once('../include/ipworkseditranslator_x12writer.php');
require_once('../include/ipworkseditranslator_const.php');
?>
<?php
class MyX12Reader extends IPWorksEDITranslator_X12reader {
  function FireEndFunctionalGroup($param) {
    echo "EndFunctionalGroup: " . $param['tag'] . "\n";
  }

  function FireEndInterchange($param) {
    echo "EndInterchange: " . $param['tag'] . "\n";
  }
  
  function FireEndLoop($param) {
    echo "EndLoop \n";
  }
  
  function FireEndTransaction($param) {
    echo "EndTransaction: " . $param['tag'] . "\n";
  }
  
  function FireError($param) {
    echo "Error: [" . $param['errorcode'] . "] " . $param['description'] . "\n";
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
    echo "Warning [" . $param['warncode'] . "] " + $param['message'] . "\n";
  }
};

try {
  if ($argc < 3) {
    echo "usage: php x12_parser.php -f file [-s schema]\n\n";
    echo "  file         the X12 file to parse\n";
    echo "  schema       the schema file to use when parsing the X12 document (optional)\n";
    echo "\nExample: php x12_parser.php -f x12.txt -s RSSBus_00401_810.json\n";
    return;
  }

  $x12reader1 = new MyX12Reader();

  for ($i = 0; $i < $argc; $i++) {
    if (str_starts_with($argv[$i], "-")) {
      if ($argv[$i] == "-f") {
        $x12reader1->setInputFile($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") {
        $x12reader1->doLoadSchema($argv[$i + 1]);
      }
    }
  }

  $x12reader1->doParse();

} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>