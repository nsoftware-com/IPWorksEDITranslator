<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks EDI Translator 2022 Demos - EDIFACT Parser</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks EDI Translator 2022 Demos - EDIFACT Parser">
</head>

<body>

<div id="content">
<h1>IPWorks EDI Translator - Demo Pages</h1>
<h2>EDIFACT Parser</h2>
<p>Shows how to parse EDIFACT documents via the Parse method.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworkseditranslator_edifactreader.php');
require_once('../include/ipworkseditranslator_edifactwriter.php');
require_once('../include/ipworkseditranslator_const.php');

?>



<?php
	$output='';
	class MyEdifactReader extends IPWorksEDITranslator_EDIFACTReader{
		
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
			$output.="WARNING" . $param['warncode'] . ":" . $param['message'] . "\r\n";             				
    	}
  };
  
  $edifactreader1 = new MyEdifactReader();

?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

   try{
        $edifactreader1->doConfig("ResolveXPathOnSet=true");
        $edifactreader1->doConfig("Encoding=iso-8859-1");
        $edifactreader1->doLoadSchema(getcwd() . "/RSSBus_D97A_INVOIC.json");
        $edifactreader1->setInputData($_POST["input"]);
        $edifactreader1->doParse();
	    
	}catch (Exception $ex){
		echo 'Error message: ', $edifactreader1->lastErrorCode() , ' :', $edifactreader1->lastError();
	}
}
?>

<form method=POST>
<center>
<table width="90%">
<tr><td><b>EDI Data:</b></td><td><b>Parsed Result:</b></td>
<tr><td><textarea name=input cols=110 rows=22>
UNA:+.?*&#39;
UNB+UNOB:1+WAYNE_TECH+ACME+160707:1547+000000002++1234++++1&#39;
UNH+509010117+INVOIC:D:97A:UN&#39;
BGM+380:::TAX INVOICE+0013550417+9&#39;
DTM+3:20070926:102&#39;
DTM+4:20061123:102&#39;
FTX+AAI+1&#39;
TAX+7+VAT+++:::10072.14+S&#39;
CUX+2:EUR:4++0.67529&#39;
PAT+1&#39;
DTM+10:20070926:102&#39;
PCD+2:0:13&#39;
LIN+000030+&#39;
PIA+1+2265S13:BP::92&#39;
PIA+1+5029766832002:UP::92&#39;
IMD+F+&#39;
QTY+47:50.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:19150.00&#39;
PRI+INV:383.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:19150.45&#39;
ALC+C+0.45+++FC&#39;
MOA+8:0.45&#39;
LIN+000040+&#39;
PIA+1+2269F22:BP::92&#39;
PIA+1+5051254078241:UP::92&#39;
IMD+F+&#39;
QTY+47:20.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:21060.00&#39;
PRI+INV:1053.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:21060.50&#39;
ALC+C+0.50+++FC&#39;
MOA+8:0.50&#39;
LIN+000170+&#39;
PIA+1+2269F10:BP::92&#39;
PIA+1+5051254078326:UP::92&#39;
IMD+F+&#39;
QTY+47:10.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:6950.00&#39;
PRI+INV:695.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:6950.16&#39;
ALC+C+0.16+++FC&#39;
MOA+8:0.16&#39;
LIN+000190+&#39;
PIA+1+2269F26:BP::92&#39;
PIA+1+5051254051190:UP::92&#39;
IMD+F+&#39;
QTY+47:5.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:2375.00&#39;
PRI+INV:475.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:2375.06&#39;
ALC+C+0.06+++FC&#39;
MOA+8:0.06&#39;
LIN+000200+&#39;
PIA+1+2265S24:BP::92&#39;
PIA+1+5029766000685:UP::92&#39;
IMD+F+&#39;
QTY+47:3.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:957.00&#39;
PRI+INV:319.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:957.02&#39;
ALC+C+0.02+++FC&#39;
MOA+8:0.02&#39;
LIN+000210+&#39;
PIA+1+2263T95:BP::92&#39;
PIA+1+5029766699575:UP::92&#39;
IMD+F+&#39;
QTY+47:3.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:2085.00&#39;
PRI+INV:695.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:2085.05&#39;
ALC+C+0.05+++FC&#39;
MOA+8:0.05&#39;
LIN+000250+&#39;
PIA+1+2269F15:BP::92&#39;
PIA+1+5051254080091:UP::92&#39;
IMD+F+&#39;
QTY+47:3.000:EA&#39;
DTM+11:20070926:102&#39;
MOA+203:4977.00&#39;
PRI+INV:1659.00:TU&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:4977.12&#39;
ALC+C+0.12+++FC&#39;
MOA+8:0.12&#39;
UNS+S&#39;
CNT+4:7&#39;
MOA+9:67627.50&#39;
MOA+79:57554.00&#39;
TAX+7+VAT+++:::17.500+S&#39;
MOA+125:57555.36:EUR:3&#39;
MOA+124:10072.14:EUR:3&#39;
ALC+C+1.36+++FC&#39;
MOA+8:1.36&#39;
UNT+104+509010117&#39;
UNZ+1+000000002&#39;
            
</textarea>
<td><textarea name=output cols=110 rows=22>
<?php echo $output ?>
</textarea>

<tr><td><input type=submit value="Parse"><td>

</table>
</center>


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
