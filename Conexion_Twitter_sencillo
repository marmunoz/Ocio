# Funciones para extraer e insertar en MongoDB
#---------------------------------------------------------------------------------
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
library(rmongodb)
install_github("twitteR", username="geoffjentry")
}
#---------------------------------------------------------------------------------
conexion_twitter <- function() {
api_key <- "Q3gXOnuWGyG93ihrFMz9bdDMQ"
api_secret <- "HIyLjYMS7TB2mnD0kUi450U0EhwRI3h3bxEFxz7aiONF5NTMA1"
access_token <- "812221945-Qt88IVtuSYRAnt9Z8GG1oUQqF5ehHcVd00AyU5Vu"
access_token_secret <- "0HVm7qDaL2XA3jVYH9Utz8nbqmc0cyuHj63xtFRfUNltf"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
}
#---------------------------------------------------------------------------------
instalar_librerias()
conexion_twitter()

# Guardando datos de fecha y hora de captura de datos
fechahora<-paste(substr(Sys.time(),1,4),substr(Sys.time(),6,7),substr(Sys.time(),9,10),substr(Sys.time(),12,13),substr(Sys.time(),15,16),substr(Sys.time(),18,19),sep="")
# Capturando los datos de Twitter para crear el primer archivo

hashtags <-c( "arte","aventura","cine","compras","concierto","cultura","deporte","divertido","entradas","entretenimiento",
    "evento","familia","finde","fiesta","festa","gastronomia","golf","gym","hotel","mar","marineland","moda","montaña",
    "musica","museo","museu","niños","ocio","ofertas","paseo","palma","party","planes","peli", "pelicula", "playa",
    "regalos","relax","restaurante","run","running","salir","shopping","spa","teatre","teatro","tour","turismo",
    "travel","vacaciones","viaje","walk","walking")

mongo = mongo.create(host = "backend.aws.ittravelservices.com:37017")
mongo.is.connected(mongo)
db <- mongo.create("backend.aws.ittravelservices.com:37017")
num_tweets<-10

for (num in 1:length(hashtags)){
palabra_clave<-hashtags[num]
captura00 <-searchTwitter(palabra_clave, num_tweets, "es")
tweets00 = ldply(captura00, function(t) t$toDataFrame() )
#
# Guardando los tweets en archivo CSV
#
archivo00 <- paste(paste("tweets","es",palabra_clave,fechahora,sep="_"),".csv",sep="")
rutaarchivo00 <- paste("C:/BTT/archivos/",archivo00,sep="")
write.csv(tweets00, file = rutaarchivo00)

if (mongo.is.connected(mongo)) {
for(t in 1:num_tweets){
				buf <- mongo.bson.buffer.create()
				mongo.bson.buffer.append(buf, "hashtag", palabra_clave)
				mongo.bson.buffer.append(buf, "text", tweets00$text)
				mongo.bson.buffer.append(buf, "RT",tweets00$retweeted)
				mongo.bson.buffer.append(buf, "IsRT",tweets00$isRetweet)
				mongo.bson.buffer.append(buf, "NumRT",tweets00$retweetCount)
				mongo.bson.buffer.append(buf, "Sentimientos", TRUE)
				mongo.bson.buffer.append(buf, "Fecha", fechahora)
				b <- mongo.bson.from.buffer(buf)
	 			mongo.insert(mongo, "twitter.datos_twitter", b)
 			}
 		}
}



