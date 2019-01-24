
<?php
error_reporting(E_ALL);

//echo "<h2>TCP/IP Connection</h2>\n";

/* Obtener el puerto para el servicio WWW. */
if(isset($_POST["domain"])){
	switch($_POST["domain"]){
		case "dates":
			$service_port = 8001;
			break;
		case "emails":
			$service_port = 8002;
			break;
		case "names":
			$service_port = 8003;
			break;
		case "words":
			$service_port = 8004;
			break;
		default:
			/* General */
			$service_port = 8005;
			break;
	}
} else {
	/* General */
	$service_port = 8005;
}


/* Obtener la dirección IP para el host objetivo. */
$address = 'safer-tools.dsic.upv.es';

/* Crear un socket TCP/IP. */
$socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
if ($socket === false) {
    //echo "socket_create() falló: razón: " . socket_strerror(socket_last_error()) . "\n";
} else {
    //echo "OK.\n";
}

//echo "Intentando conectar a '$address' en el puerto '$service_port'...";
$result = socket_connect($socket, $address, $service_port);
if ($result === false) {
    //echo "socket_connect() falló.\nRazón: ($result) " . socket_strerror(socket_last_error($socket)) . "\n";
} else {
    //echo "OK.\n";
}

$in = $_POST["predicate"];
$out = '';

//echo "Enviando predicado: ".$in;
socket_write($socket, $in, strlen($in));
//echo "OK.\n";

//echo "Leyendo respuesta:\n\n";
while ($out = socket_read($socket, 2048)) {
    echo $out;
}

//echo "Cerrando socket...";
socket_close($socket);
//echo "OK.\n\n";
?>