
<?php
error_reporting(E_ALL);



$f = ":set -w \n :load /srv/www/dsi-demo/functions.hs \n";

$results = array();

$file = fopen("in.txt", "w");
fwrite($file, "". PHP_EOL);
fclose($file);
file_put_contents("in.txt", $f. PHP_EOL, FILE_APPEND | LOCK_EX);



for($i=0; $i<$_POST["num_examples"]; $i++){
	
	$salida=array();
	$return=0;

	
	if($_POST["inputs"][$i]=="") {
		$results[$i]='""';		
		file_put_contents("in.txt", $results[$i]. PHP_EOL, FILE_APPEND | LOCK_EX);
		
	}
	else if($_POST["outputs"][$i]!="") {
		$results[$i]='"'.$_POST["outputs"][$i].'"';
		file_put_contents("in.txt", $results[$i]. PHP_EOL, FILE_APPEND | LOCK_EX);
	}
	else {	
		$results[$i] = str_replace(" a ", ' "'.$_POST["inputs"][$i].'" ', $_POST["f"]);
		file_put_contents("in.txt", $results[$i]. PHP_EOL, FILE_APPEND | LOCK_EX);
	}
			
}

		$cmd="ghci < in.txt";
		exec($cmd, $salida, $return);	

		/*if($_POST["inputs"][$i]=="") $results[$i]="";
		else if($_POST["outputs"][$i]!="") $results[$i]=$_POST["outputs"][$i];
		else {	
			$salida=array();
			$return=0;
			$file = fopen("in.txt", "w");
			fwrite($file, $f.str_replace(" a ", ' "'.$_POST["inputs"][$i].'" ', $_POST["f"]). PHP_EOL);
			fclose($file);	

			$cmd="ghci < in.txt";
			exec($cmd, $salida, $return);
			$resultado=substr($salida[3], 8);
			$resultado=substr($resultado, 0, -1);
			$results[$i]=$resultado;			
		}*/

for($i=0; $i<$_POST["num_examples"]; $i++){
	$results[$i]=$salida[$i+3];
	
}

echo json_encode($results);


?>