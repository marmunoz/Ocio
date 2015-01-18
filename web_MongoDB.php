# Código fuente para conectar la BBDD de MongoDB, y ver su contenido desde una página simple realizada en Php.
# Este sería el contenido del fichero index.php
<html>
 <head>
  <title>Conexion MongoDB</title>
 </head>
 <body>
  <?php echo "<p>A continuacion podemos visualizar los tuits que hemos capturado</p>"; ?>

  <?php
	$m = new MongoClient( "mongodb://servidor" ); -- oculto la info del servidor
	$db = $m->selectDB('twitter'); // nombre de la base de datos
	$info = $db->selectCollection("datos_twitter"); //seleccionar la colección
	$msgs = $info->find(); //busca y filtra la colección
	$i = 0;
	foreach ($msgs as $msg) { //recorre la colección en busca de la columna 'text'
		echo $i;
		echo " - ";
		$text = $msg['text'];
//		var_dump($text[$i]);
		echo $text[$i];

		echo "<br>";
		$i = $i + 1;
	}
  ?>
 </body>
</html>
