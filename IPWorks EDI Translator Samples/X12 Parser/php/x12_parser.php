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
	$output='';
	class MyX12Reader extends IPWorksEDITranslator_X12reader{
		
    	function fireEndFunctionalGroup($param){			
			global $output;
			$output.="EndFunctionalGroup: " . $param['tag'] . "\r\n";             				
    	}

		function fireEndInterchange($param){			
			global $output;
			$output.="EndInterchange: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireEndLoop($param){			
			global $output;
			$output.="EndLoop \r\n";             				
    	}
		
		function fireEndTransaction($param){			
			global $output;
			$output.="EndTransaction: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireError($param){			
			global $output;
			$output.="ERROR " . $param['errorcode'] . ":" . $param['description'] . "\r\n";             				
    	}
		
		function fireResolveSchema($param){			
			global $output;
			$output.="ResolveSchema: " . $param['transactioncode'] . "\r\n";             				
    	}
		
		function fireSegment($param){			
			global $output;
			$output.="Segment: " . $param['name'] . "\r\n";             				
    	}
		
		function fireStartFunctionalGroup($param){			
			global $output;
			$output.="StartFunctionalGroup: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireStartInterchange($param){			
			global $output;
			$output.="StartInterchange: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireStartLoop($param){			
			global $output;
			$output.="StartLoop: " . $param['name'] . "\r\n";             				
    	}
		function fireStartTransaction($param){			
			global $output;
			$output.="StartTransaction: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireWarning($param){			
			global $output;
			$output.="WARNING" . $param['warncode'] . ":" + $param['message'] . "\r\n";             				
    	}
  };
  
  $x12reader1 = new MyX12Reader();

?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

   try{
		$x12reader1->doConfig("ResolveXPathOnSet=true");
        $x12reader1->doConfig("Encoding=iso-8859-1");
        $x12reader1->doLoadSchema(getcwd() . "/RSSBus_00401_810.json");
		$x12reader1->setInputData($_POST["input"]);
        $x12reader1->doParse();
	    
	}catch (Exception $ex){
		echo 'Error message: ', $x12reader1->lastErrorCode() , ' :', $x12reader1->lastError();
	}

}
?>

<form method=POST>
<center>
<table width="90%">
<tr><td><b>EDI Data:</b></td><td><b>Parsed Result:</b></td>
<tr><td><textarea name=input cols=110 rows=22>
ISA*00*          *00*          *ZZ*ACME           *ZZ*WAYNE_TECH     *160707*1544*U*00401*000000006*0*T*>~
GS*IN*ACME*WAYNE_TECH*20160707*1544*6*T*004010~
ST*810*0001~
BIG*20150708*3003014445**0476553272***DR~
CUR*SE*USD~
REF*8M*0056~
N1*BY*Company*92*544380~
N3*Address~
N4*City*CA*Postal Code~
N1*ST*Name*92*0607047800010~
N3*Address~
N4*City**200131*US~
N1*RE*Name*92*5095956~
N3*Address~
N4*City*IL*Postal Code~
IT1*20*2500*EA*36.96**BP*335S0594~
REF*KK*0099778154~
REF*PO*0476553272*20~
TDS*9240000~
CTT*1~
SE*19*0001~
GE*1*6~
IEA*1*000000006~

            
</textarea>
<td><textarea name=output cols=110 rows=22>
<?php echo $output ?>
</textarea>

<tr><td><input type=submit value="Parse"><td>

</table>
</center>


</form>
