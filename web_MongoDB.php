# Código fuente para conectar la BBDD de MongoDB, y ver su contenido desde una página simple realizada en Php.
# Este sería el contenido del fichero index.php
<?php
	 //conectamos con mongo, suponiendo que lo haces en local (localhost)
	$m = new MongoClient("mongodb://testUser:testPass@localhost");     -- pongo datos de prueba, no lo publico
	$db = $m->selectDB('twitter'); // nombre de la base de datos
	$db->authenticate("admin", " My password ");                       -- pongo datos de prueba, no lo publico
	$info = $db->selectCollection("datos_twitter"); //seleccionar la colección
	$msgs = $info->find(); //busca y filtra la colección
	foreach ($msgs as $msg) { //recorre la colección en busca de la columna 'text'
		echo "TEXTOS:".$msg['text']."\n";
	}
?>
