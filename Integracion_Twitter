#----------------------------------------------------------------------------------------------------------------
instalar_librerias<-function(){
install.packages(c("devtools", "rjson", "bit64", "httr","tm","twitteR","plyr","wordcloud","RColorBrewer","NLP","datasets","graphics","stringr"))
library(plyr)
library(devtools)
library(twitteR)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(stringr)
install_github("twitteR", username="geoffjentry")
}
#---------------------------------------------------------------------------------------------------------------
conexion_twitter <- function() {
library(plyr)
library(devtools)
library(twitteR)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(stringr)
api_key <- "Q3gXOnuWGyG93ihrFMz9bdDMQ"
api_secret <- "HIyLjYMS7TB2mnD0kUi450U0EhwRI3h3bxEFxz7aiONF5NTMA1"
access_token <- "812221945-Qt88IVtuSYRAnt9Z8GG1oUQqF5ehHcVd00AyU5Vu"
access_token_secret <- "0HVm7qDaL2XA3jVYH9Utz8nbqmc0cyuHj63xtFRfUNltf"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
}
#---------------------------------------------------------------------------------------------------------------
#--------------------------------	
#Importación librerías 
#--------------------------------		
	require(twitteR)
 	rm(list=ls())
 	library(twitteR)
 	library(stringr)
 	library(wordcloud)
 	library(RCurl)
 	library(tm)
 	library(ROAuth)
 	library(rmongodb)
	library(plyr)
#--------------------------------------------------------------------------
#API de Twitter 
#--------------------------------------------------------------------------
	#introducimos contraseñas de la aplicación de twitter
 	download.file(url="http://curl.haxx.se/ca/cacert.pem",
 	destfile="cacert.pem")
 	requestURL <- "https://api.twitter.com/oauth/request_token"
 	accessURL <- "https://api.twitter.com/oauth/access_token"
 	authURL <- "https://api.twitter.com/oauth/authorize"

 	#obtenemos Access Token i Access Token Secret de twitter
 	consumerKey <- "zP90V3ENQuRbdLc73ZsvTLZwZ"
 	consumerSecret <- "4UUPmoDqKdkeZbAfPo5NM2PIlqTc7vrBDZwBGiapgBKmokfjmg"

 	twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                            	 consumerSecret=consumerSecret,
                            	 requestURL=requestURL,
                            	 accessURL=accessURL,
                            	 authURL=authURL)

 	twitCred$handshake(cainfo="cacert.pem")

	#--------------------------------------------------------------
	#En este punto se genera una URL:
	#1)Copiarla y unirla al buscador
	#2)Permitir el acceso a la aplicación
	#3)Copiar el codido generado a R
 	#--------------------------------------------------------------

 	save(twitCred, file="twitter authentication.Rdata")
	 registerTwitterOAuth(twitCred)
 	#En este paso si todo ha ido bien saldrá=> [1] TRUE

 	#Evitar errores SSL
 	library(RCurl)
	library(plyr)
 	options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#--------------------------------------------------------------
#ARRAYS DE HASHTAGS
#--------------------------------------------------------------
hashtags <-c("turismo", "arte",	"viaje","compras","concierto","cultura","gym","spa","deporte","entradas","travel",
		"evento", "run","running","relax","teatre","teatro","tour","familia","finde","fiesta","festa","restaurante",
		"shopping","gastronomia","golf","hotel","vacaciones","marineland","moda","musica","museo","museu","ocio",
		"ofertas","paseo","palma","party","planes","walk","walking","aventura","divertido","entretenimiento",
		"cine",	"peli", "pelicula","mar", "montaña","playa","niños","regalos","salir")

pobles <-c("palma", "alaro","alcudia","port","algaida","pina","andratx","ariany","arta","banyalbufar","binissalem",
"bunyola","orient","calvia","palma","magaluf","palmanova","peguera","portals","santaponça","mallorca","campanet",
"campos","capdepera","costitx","deia","lluc","esporles","estellencs","felanitx","portocolom","fornalutx","inca",
"lloret","lloseta","llubi","llucmajor","manacor","portocristo","mancor","maria","marratxi","cabaneta","portol","montuiri",
"muro","petra","pobla","pollença","porreres","puigpunyent","galilea","salines ","consell ","picafort","santanyi",
"portopetro","sencelles","sineu","soller","valldemossa","vilafranca")

#-------------------------------------------------------------------------
#INTRODUCIR A VAR EL NOMBRE DEL ARRAY DE BÚSQUEDA (pobles o hashtags)
#-------------------------------------------------------------------------
                           var=hashtags;
#-------------------------------------------------------------------------
long=length(var)

for(r in 1:long){        #iniciO del bucle principal
	hashtag=var[r];
	TweetNumber=100;
	#Iniciamos la extracción de información, centramos la búsqueda en el centre de Mallorca geo="39.6169,2.9715 con un radio de 65Km
	tweets = searchTwitter(hashtag, TweetNumber, geo="39.6169,2.9715,65km")
	dades = do.call("rbind", lapply(tweets, as.data.frame))
	COMENTARIOS=dades$text

#------------------------------------------------------------------
#                     COLUMNA #HASHTAG
#-------------------------------------------------------------------

 HASHTAG=character(TweetNumber);
	for(y in 1:TweetNumber){
		HASHTAG[y]=var[r]
	}
#------------------------------------------------------------------------
#                          ANÀLISIS DE SENTIMIENTOS
#-------------------------------------------------------------------------

	negativo <- scan("negatiu.txt", what='character',comment.char=';');
	positivo <- scan("positiu.txt", what='character',comment.char=';');

#------------------------------------------------------------------------
#		     FUNCIÓN ANÀLISIS DE SENTIMIENTOS
#------------------------------------------------------------------------
score.sentiment = function(sentences, pos.words, neg.words, .progress='none'){

	require(plyr)
	require(stringr)

	scores = laply(sentences, function(sentence, pos.words, neg.words){
		
		#Limpieza de carácteres especiales
		sentence = gsub('[[:punct:]]','',sentence)
		sentence = gsub('[[:cntrl:]]','',sentence)
		sentence = gsub('\\d+','',sentence)
		
		#Pasamos a minúsculas
		sentence = tolower(sentence)
		
		#Separamos los comentarios en palabras
		word.list = str_split(sentence, '\\s+')

		words = unlist(word.list)
		
		#Buscamos coincidencias entre palabras de los comentarios y las listas
		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)


		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)
		
		#Generamos el sentimiento global del comentario
		score = sum(pos.matches) - sum(neg.matches)

		return(score)
		}, pos.words, neg.words, .progress=.progress)

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}

#--------------------------------------------------------------
	#Valoración positiva y negativa 
	resultado <- score.sentiment(COMENTARIOS, positivo, negativo)
	SENTIMIENTO=resultado$score;
	NumOfResults=length(SENTIMIENTO)
	aux=(NumOfResults+1);
		
	#Rellenamos con 0 las celdas vacías	
	for(i in aux:TweetNumber){
		SENTIMIENTO[i]=0;
	}
	
#--------------------------------------------------------------
	dades = do.call("rbind", lapply(tweets, as.data.frame))

	COMENTARIOS=dades$text
	CMT=as.data.frame(COMENTARIOS)
  
	USUARIOS=dades$screenName
	USU=as.data.frame(USUARIOS)

	DATES=dades$created
	DAT=as.data.frame(DATES)

	TWEETS=dades$retweetCount 
	TWE=as.data.frame(TWEETS)

#--------------------------------------------------------------
#                    EXTRACCIÓN DE URL's
#--------------------------------------------------------------
 
 	comentaris <- sapply(tweets, function(x) x$getText()) 
 	comentaris=c(comentaris)                              #Crea un vector con los comentarios
 	URL=character(TweetNumber);
 	n=1;
 
 	while(n<(TweetNumber+1)){
 		a=str_locate(comentaris[n],"http://t.co/")    #Donde comienza la URL 
 		inici=a[1];

		#En Twitter las URL's tienen 22 caracteres
 		fi= inici+22;
 		URL[n]=substr(comentaris[n],inici, fi)
 		n=n+1;
	}

 	URL[is.na(URL)] = 0 
 	U=as.data.frame(URL)

#------------------------------------------------------------------------------
#                     CREACIÓN DE TABLA DE DATOS
#------------------------------------------------------------------------------

	tabla=cbind(HASHTAG,USUARIOS,TWEETS,URL,COMENTARIOS,SENTIMIENTO)

#------------------------------------------------------------------
#                       GUARDAMOS UNA COPIA EN TXT
#------------------------------------------------------------------

# 	nombredelarchivo =hashtag;
# 	nombredelarchivo = paste(nombredelarchivo,".txt", sep="")
# 	write.table(tabla, file=nombredelarchivo)

#------------------------------------------------------------------------------
#                          GUARDAMOS EN LA BBDD (MONGODB)
#------------------------------------------------------------------------------

 	mongo = mongo.create(host = "backend.aws.ittravelservices.com:37017")
 	mongo.is.connected(mongo)

 	db <- mongo.create("backend.aws.ittravelservices.com:37017")
 	if (mongo.is.connected(mongo)) {

		for(t in 1:TweetNumber){
			if (URL[t]!=0) {
 				buf <- mongo.bson.buffer.create()
				mongo.bson.buffer.append(buf, "hashtag", HASHTAG[t])
				mongo.bson.buffer.append(buf, "usuarios", USUARIOS[t])
				mongo.bson.buffer.append(buf, "retweets", TWEETS[t])
				mongo.bson.buffer.append(buf, "url", URL[t])
				mongo.bson.buffer.append(buf, "comentarios", COMENTARIOS[t])
				mongo.bson.buffer.append(buf, "sentimiento", SENTIMIENTO[t])
				mongo.bson.buffer.append(buf, "fecha",date())
				b <- mongo.bson.from.buffer(buf)
	 			mongo.insert(mongo, "twitter.datos3", b)
 			}
 		}
	}
}#cierro bucle principal


