#=====================================================================#
#    Facultad de Economía "Vasco de Quiroga" de la UMSNH
#    Seminario Análisis de datos aplicados a la Economía
#    Morelia, México, 6 de octubre de 2022
#_____________________________________________________________________#
#    Topic: Análisis de datos de texto en redes sociales
#_____________________________________________________________________#
#    1. Carga de librerias necesarias
#    2. Como conectarnos a la API de Twitter y descargar datos
#    3. Otras funciones utiles de la libreria 'rtweet'
#    4. Como realizar el proceso de limpieza y tokenizacion con datos de texto en Twitter
#    5. Como realizar un analisis exploratorio con datos de texto en Twitter
#    6. Como analizar la opinion, polaridad o sentimiento sobre un individuo con datos de texto en Twitter
#    7. Como clasificar/predecir la autoria de un texto con datos de Twitter
#=====================================================================#
## 1 Carga de librerias necesarias -----
#_____________________________________________________________________#
library(rtweet) # Para poder obtener los tweets de Twitter (seguir esta alternativa)
library(twitteR) # Para poder obtener los tweets de Twitter
library(httpuv) # Para resolver los problemas de la liberia twitteR
library(openssl) # Para resolver los problemas de la liberia twitteR
library(tm) # Para editar el texto: quitar puntuacion, etc. 
library(corpus) # Para analizar el corpus del texto
library(tidytext) # Para hacer analisis de sentimiento
library(tidyverse) # Para hacer analisis de sentimiento
library(syuzhet) # Para hacer analisis de sentimiento
library(wordcloud) # Para hacer wordclouds
library(wordcloud2) # Otra alternativa para hacer wordclouds
library(RColorBrewer) # Para cambiar la gma de colores
library(tidyr) # Para poder hacer un unnest tokens
library(dplyr) # Para gestionar objetos en R
library(plyr) # Para gestionar objetos en R
library(purrr) # Para gestionar objetos en R
library(stringr) # Para gestionar objetos en R
library(scales) # Para gestionar objetos en R
library(ggplot2) # Para realizar graficos
library(qdap) # Para conjuntos de palabras
library(maps) # Para graficar los tweets
library(readr) # Para grabar archivos csv
library(rvest) # Para hacer Web scraping
library(rebus) # Para resolver los problemas de Web scraping
library(lubridate) # Para analisis temporal de los tweets
library(igraph) # Para realizar redes de n-gramas
library(quanteda) # Para construir ma matriz tf-idf
library(e1071) # Para ajustar un modelo SVM
library(caret) # Algoritmos machine learning

## 2 Como conectarnos a la API de Twitter y descargar datos ----
#_____________________________________________________________________#
# Nuestro objetivo sera conectarnos a los datos de Twitter y descargar los tweets 
# segun las palabras claves de interes. Para esto especificaremos una palabra clave, 
# la latitud y longitud de la geolocalizacion del tweet, el radio  en kilometros desde ese punto 
# geolocalizado y el numero de tweets a descargar.

# Este es el momento de utilizar los datos de la API. Para ello tendremos que definir nuestras claves y tokens.
# Vamos a crear una serie de variables y les asignaremos los valores correspondientes.
api_key             <- "kOWOnzNkDX3IM1swgl2pdZQRo"
api_secret_key      <- "JVM33O694rOjODRUX05lfQ25Pg11B4DZLXZ9FjJFPuwimnAqeu"
access_token        <- "1388492528974974979-owxzJr9FKVpyWnAAr6w205T1VoZnPt"
access_secret       <- "L1FP4amrZ7MQJoRQXwJt7KVBNz7cGAFFLKFyEVGM0ecSV"

# Autentificacion acceso a la API
token <- create_token(
  app = "git_prueba",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret)

# Confirmamos que tenemos acceso a la API sin problemas
post_tweet(status = "Hola, les envio mi primer tweet")

# La API Estandar (gratuita) permite la obtencion de Tweets de cualquier localizacion en los ultimos 
# 7 dias. Sin embargo, solo podemos obtener 18.000 tweets cada 15 minutos. Existen 2 librerias para descargar datos de 
# twitter: rtweet y twitteR. Nosotros utilizaremos la primera de ellas. Hay 2 formas fundamentalmente de descargar
# los datos en twitter: escucha directa o indirecta a traves de keywords.

# Escucha DIRECTA. Vamos a descargar tweets de @elonmusk y @BillGates por ejemplo
# Esta opcion tiene el limite de 3200 tweets en cada busqueda.
tweets1 <- get_timeline("@elonmusk", n = 200, parse = TRUE, check = TRUE, include_rts = FALSE)
tweets2 <- get_timeline("@BillGates", n = 200, parse = TRUE, check = TRUE, include_rts = FALSE)

# Se unen todos los tweets en un unico dataframe
tweets <- bind_rows(tweets1, tweets2)

# Tambien podriamos hacerlo directamente
tweets <- get_timeline(c("@elonmusk","@BillGates"), n = 200, parse = TRUE, check = TRUE, include_rts = FALSE)

# Podemos extraer info de varios componentes: nombre de usuario, frecuencia en la que los tweets fueron publicados,
# el propio texto del tweet, los hashtags, los links, los retweets, los favoritos, la latitud/longitud...
# Como podeis observar existen 90 variables con informacion de cada tweet con lo cual dependiendo de los objetivos de 
# nuestro estudio nos centraremos en unas u otras. Ver https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/tweet
View(tweets)

# Escucha INDIRECTA. Existen 2 posibilidades de escucha indirecta: simple o compuesta. Comenzaremos con la SIMPLE.
# En este caso vamos a definir una peticion en la que rescataremos 1000 tweets (si los hay) 
# con el hashtag #rstudio en la zona de Madrid y para un area circundante de 750 kms

# Definimos nuestro hashtag de la siguiente forma:
hashtag <- #rstudio

# La localizacion se define a partir de la latitud, la longitud y el radio de busqueda con la siguiente estructura.
# Para el geocode tenemos que definir latitud-longitud: https://www.coordenadas-gps.com/
geoloc <- "40.36329,-3.69141,750km" 

# Definiremos el numero maximo de tweets con:
n_tweets <- 1000

# Ahora si podemos crear nuestra peticion de tweets con la siguiente instruccion:
tweets <- search_tweets(q = hashtag, geocode = geoloc, n = n_tweets, retryonratelimit = T)

# El parametro retryonratelimit se deberan utilizar siempre que el numero de tweets sea mayor 
# a 18.000. En ese caso, cada 15 minutos, la instruccion recuperara 18.000 nuevos tweets 
# siempre que existan en esa ubicacion y con el hashtag indicado.

# El resultado de nuestra consulta ha sido mas de 800 tweets. Los datos se han guardado en el objeto 
# tweets que es un tipo data.frame y ya podemos manipular y analizar nuestros datos a traves de R.
colnames(tweets)

# Como podeis ver existen varios parametros que podemos incluir en nuestras consultas y uno de 
# los mas importantes es el idioma.
tweetsq0a <- search_tweets(q = hashtag, n = 200, lang = "en")
tweetsq0b <- search_tweets(q = hashtag, n = 200, lang = "es")

# Existe la posibilidad de una busqueda mas especifica considerando la escucha indirecta COMPUESTA. 
# En este caso, podremos utilizar mas de 1 keyword junto al operador booleano OR.
tweetsq1 <- search_tweets2(q = '"data science"', n = 200)
tweetsq2 <- search_tweets2(q = "data science", n = 200)
tweetsq3 <- search_tweets2(q = "data OR science", n = 200)
tweetsq4 <- search_tweets2(q = c('"data science"', "rstats OR python"),n = 200)

# Recordad siempre Descargar, Guardar y Leer si no quereis perder vuestros datos. Tened tambien en cuenta 
# la siguiente cuestion en relacion al tamano de los archivos de cara a guardarlos. Os recomiendo que utiliceis 
# estas 2 alternativas para grabar los datos en un archivo csv:
rtweet::write_as_csv()
data.table::fwrite()

# estas 2 alternativas para cargar el archivo csv:
data.table::fread()
rtweet::read_twitter_csv()

# Tambien tened en cuenta que debeis de eliminar tweets por el ID cuando hagais descargas 
# de un mismo termino en momentos del tiempo distintos ya que puede existir la posibilidad 
# de que tengais tweets repetidos. Utilizar la libreria match y su operador %in% por ejemplo.

# Otra consulta sin utilizar las coordenadas, considerando solo el hashtag #rstats y 500 tweets
rstats_tweets <- search_tweets(q = "#rstats",n = 500)

# Obtenemos una lista de los usuarios que lo han publicado
unique(rstats_tweets$screen_name)

# Podemos ver las localizaciones de esos usuarios si estan autorizadas
length(unique(rstats_tweets$location))

# Graficamos un pequeno mapa de frecuencia
rstats_tweets %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Frecuencia",
       y = "Localizacion",
       title = "Usuarios de twitter - localizacion")

# Vamos a obtener un grafico mejor que el anterior considerando solo las localizaciones top (> 20 usuarios) 
rstats_tweets %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Frecuencia",
       y = "Localizacion",
       title = "Usuarios de twitter - localizacion top")

# En este otro ejemplo buscaremos los tweets que contengan la palabra clave 'Pedro Sanchez' 
politico <- "Pedro Sanchez"
geocode <- '40.417764,-3.7058457,650km'
n <- 500
tweets <- search_tweets(politico, geocode = geocode ,n=n)
View(tweets)

# Si superais la cantidad de 18 mil tweets teneis que escribir 'retryonratelimit = TRUE'
tweets <- search_tweets(politico, geocode = geocode ,n=n , retryonratelimit = TRUE)
View(tweets)

# Si lo que quereis es descargar los tweets que ha publicado el propio Pedro Sanchez.
# Para no incluir 'retweets' agregamos include_rts = FALSE.
tweetsps <- get_timeline("@sanchezcastejon", n = 200, include_rts = FALSE)

# Graficando una serie de tiempo de tweets
ts_plot(tweets, "1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequencia de Tweets que incluya a Pedro Sanchez",
    subtitle = "Tweets contados en intervalos de una hora",
    caption = "\nFuente: Datos recolectados desde la API de Twitter"
  )

# Comprobaremos cuantos tweets tienen latitud y longitud
sum(!is.na(tweets$lat))

# Grafico estatico: los tweets en Espana (puntos geolocalizacion)
par(mar = c(0, 0, 0, 0))
map(regions=sov.expand("Spain"))
with(tweets, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .8, .7, .75)))

# Grafico dinamico: los tweets en Espana (puntos geolocalizacion)
datos_map<-cbind.data.frame(tweets$lng,tweets$lat)
datos_map<-na.omit(datos_map)
leaflet(data=datos_map) %>%
  addTiles()%>%  
  addMarkers(data=datos_map,lng=~tweets$lng, lat=~tweets$lat)

## ********************************************************************


## ********************************************************************

## ********************************************************************
## 3 Otras funciones utiles del paquete 'rtweet' ----
#_____________________________________________________________________#
# Genera info sobre el ultimo tweet del usuario en el que estemos interesados 
usuario <- 'sanchezcastejon'
usr_df <- lookup_users(users = usuario)
View(usr_df)

# Genera una lista de IDs de usuarios seguidores, para un listado superior a
# 75.000 usuarios (es el limite maximo) estableceremos "retryonratelimit" en TRUE.
followers <- get_followers(user = usuario, n = 5000)
View(followers)

# Vamos a ver ahora que followers comparte con Pablo Iglesias
usuario2<-'PabloIglesias'
followers2 <- get_followers(user = usuario2, n = 5000)
View(followers2)
followers_compartidos<-sum(followers$user_id %in% followers2$user_id)
View(followers_compartidos)

# Otra funcion parecida a la anterior de las personas que sigue 
friends <- get_friends(user = usuario, n = 5000)
View(friends)

# Tendencias en twitter disponibles
trends <- trends_available()
View(trends)

# Con la tabla anterior podemos seleccionar el pais y 
# generar las tendencias de twitter de ese pais.
woeid       <-as.numeric(trends[trends$name=='Spain',"woeid"])
data_trends <- get_trends(woeid)
View(data_trends)

## ********************************************************************
## 4 Como realizar el proceso de limpieza y tokenizacion con datos de texto en Twitter ----
#_____________________________________________________________________#
# El proceso de limpieza de texto, dentro del ambito de text mining, consiste en eliminar del texto 
# todo aquello que no aporte informacion sobre su tematica, estructura o contenido. No existe una unica forma 
# de hacerlo, depende en gran medida de la finalidad del analisis y de la fuente de la que proceda el texto.

# Definir una funcion que contenga cada uno de los pasos de limpieza tiene la ventaja de poder 
# adaptarse facilmente dependiendo del tipo de texto analizado.

limpiar_tokenizar <- function(texto){
  
  # Primero se convierte todo el texto a minusculas
  nuevo_texto <- tolower(texto)
  
  # Eliminamos las paginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  
  # Eliminamos los signos de puntuacion
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminamos los numeros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminamos los espacios en blanco multiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  
  # Realizamos el proceso de tokenizacion por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  
  # Finalmente eliminamos los tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Realizamos una pequena prueba
test <- "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @elonmusk #textmining"
limpiar_tokenizar(texto = test)

# Vamos a cargar nuevos tweets de @elonmusk y @BillGates utilizando los archivos csv 
tweets <- bind_rows(elonmusk, billgates)

# De entre toda la informacion disponible, en este ejemplo unicamente emplearemos: 
# autor del tweet, fecha de publicacion, identificador del tweet y contenido.
tweets <- tweets %>% select(screen_name, created_at, status_id, text)

# Se renombran las variables con nombres mas practicos
tweets <- tweets %>% dplyr::rename(autor = screen_name, fecha = created_at, texto = text, tweet_id = status_id)

# Aplicamos la funcion de limpieza y tokenizacion a cada tweet analizado 
tweets <- tweets %>% dplyr::mutate(texto_tokenizado = purrr::map(.x = tweets$texto,
                                                                 .f = limpiar_tokenizar))

# Cada elemento de la columna texto_tokenizado es una lista con un vector de tipo 
# caracter que contiene los tokens (palabras) generados en cada tweet
tweets %>% select(texto_tokenizado) %>% head()

# Por ejemplo, vamos a ver cual es el texto tokenizado del primer tweet
tweets %>% slice(1) %>% select(texto_tokenizado) %>% pull()
tweets %>% slice(2) %>% select(texto_tokenizado) %>% pull()


## 5 Como realizar un analisis exploratorio con datos de texto en Twitter ----
#_____________________________________________________________________#
# Notad que previamente a la division del texto, los elementos de estudio eran los tweets, 
# y cada uno se encontraba en una fila, cumpliendo asi la condicion de tidy data: una observacion, 
# una fila. Al realizar la tokenizacion, el elemento de estudio ha pasado a ser cada token (palabra), 
# incumpliendo asi la condicion de tidy data.

# Para volver de nuevo a la estructura ideal se tiene que expandir cada lista de tokens, 
# duplicando el valor de las otras columnas tantas veces como sea necesario. A este proceso 
# se le conoce como expansion o unnest.
tweets_tidy <- tweets %>% select(-texto) %>% tidyr::unnest(cols = c(texto_tokenizado))
tweets_tidy <- tweets_tidy %>% dplyr::rename(token = texto_tokenizado)
head(tweets_tidy) 

## ********************************************************************
## 5.1 Comenzaremos realizando un analisis de la distribucion temporal de los tweets
## ********************************************************************
# Dado que cada usuario puede haber iniciado su actividad en twitter en diferentes momentos, 
# es interesante explorar si los tweets recuperados se solapan en el tiempo. En este caso,
# Bill Gates ha mantenido una actividad constante de en torno a 100 tweets  durante todo el 
# periodo estudiado. Elon Musk muestra una actividad inicial por debajo de la de Bill Gates 
# pero, a partir de enero de 2016 incrementa sustancialmente el numero de tweets publicados.
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicacion", y = "numero de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Otra alternativa sería la siguiente. Puede observarse un perfil de actividad distinto para cada usuario.  
tweets$fecha <- strptime(as.character(tweets$fecha), "%Y-%m-%d %H:%M:%OS")
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% dplyr::summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

## ********************************************************************
## 5.2 Fecuencia de palabras
## ********************************************************************
# A la hora de entender que caracteriza la escritura de cada usuario, es interesante estudiar que palabras emplea, 
# con que frecuencia, asi como el significado de las mismas. Nos centraremos en 7 indicadores en particular.

# 5.2.1 Total de palabras utilizadas por cada usuario
tweets_tidy %>% group_by(autor) %>% dplyr::summarise(n = n()) 
tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# 5.2.2 Palabras distintas utilizadas por cada usuario
tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  dplyr::summarise(palabras_distintas = n())
tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# Teneis que comparar el total de palabras utilizadas vs palabras diferentes

# 5.2.3 Longitud media de los tweets por usuario
tweets_tidy %>% group_by(autor, tweet_id) %>% dplyr::summarise(longitud = n()) %>%                       
  group_by(autor) %>% dplyr::summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud))
tweets_tidy %>% group_by(autor, tweet_id) %>% dplyr::summarise(longitud = n()) %>% group_by(autor) %>%
  dplyr::summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) + geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) + coord_flip() + theme_bw()

# El tipo de tweet de Bill Gates y Elon Musk es diferente en cuanto a longitud media y desviacion. 
# Elon Musk alterna mas entre tweets cortos y largos, siendo su media inferior a la de Bill Gates.

# 5.2.4 Palabras mas utilizadas por usuario
tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>% group_by(autor) %>%
  dplyr::top_n(10, n) %>% dplyr::arrange(autor, desc(n)) %>% print(n=30)

# 5.2.5 Stop words
# En la tabla anterior puede observarse que los terminos mas frecuentes en todos los usuarios se 
# corresponden con articulos, preposiciones, pronombres, en general, palabras que no aportan 
# informacion relevante sobre el texto. A estas palabras se les conoce como stopwords.

# Para cada idioma existen distintos listados de stopwords, ademas, dependiendo del contexto, puede 
# ser necesario adaptar el listado. Dentro del paquete tidytext y tokenizers pueden encontrarse varios 
# listados de stopwords para los idiomas en (ingles), da, de, es (espanol), fr, it, ru. En este caso, se 
# emplea un listado de stopwords obtenido de la libreria de Python nltk.corpus.
lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves',
                     'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him','his',
                     'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
                     'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
                     'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are',
                     'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had',
                     'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and',
                     'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at',
                     'by', 'for', 'with', 'about', 'against', 'between', 'into',
                     'through', 'during', 'before', 'after', 'above', 'below', 'to',
                     'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under',
                     'again', 'further', 'then', 'once', 'here', 'there', 'when',
                     'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more',
                     'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will',
                     'just', 'don', 'should', 'now', 'd', 'll', 'm', 'o', 're', 've',
                     'y', 'ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn', 'hasn',
                     'haven', 'isn', 'ma', 'mightn', 'mustn', 'needn', 'shan',
                     'shouldn', 'wasn', 'weren', 'won', 'wouldn','i')

# Se anade el termino amp al listado de stopwords que procede de la etiqueta html: amp
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))

# Nuevamente vemos las palabras mas utilizadas por usuario tras eliminar las stopwords
tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>% group_by(autor) %>%
  dplyr::top_n(10, n) %>% dplyr::arrange(autor, desc(n)) %>% print(n=30)

# 5.2.6 Representacion grafica de las frecuencias
tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>% group_by(autor) %>%
  dplyr::top_n(10, n) %>% dplyr::arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

# Los resultados obtenidos tienen sentido si ponemos en contexto la actividad profesional de 
# los usuarios analizados. Elon Musk dirige varias empresas tecnologicas entre las que destacan Tesla 
# y SpaceX, dedicadas a los coches y a la aeronautica. Por ultimo, Bill Gates, ademas de propietario 
# de Microsoft, dedica parte de su capital a fundaciones de ayuda, de ahi las palabras mundo, personas, ayuda

# 5.2.7 Word Clouds o Nube de Palabras
# Una nube de palabras o word cloud es una representacion visual de las palabras que conforman un texto, donde el 
# tamano es mayor para las palabras que aparecen con mayor frecuencia. Hacemos una primera funcion utilizando
# la libreria wordcloud.
wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>%
  group_by(autor) %>% dplyr::mutate(frecuencia = n / n()) %>%
  dplyr::arrange(autor, desc(frecuencia)) %>% nest()

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

# Como no termina de cargar bien y tarda utilizamos otras alternativas.
# De entre toda la informacion disponible, en este ejemplo unicamente se emplea: 
# autor del tweet (@elonmusk) , fecha de publicacion, identificador del tweet y contenido.
tweets1 <- elonmusk %>% select(screen_name, created_at, status_id, text)

# Visualizaremos la estructura de la tabla 'tweets' con las 10 primeras observaciones
View(tweets1[1:10,])

# Las columnas seleccionadas seran los tweets escritos (text), la fecha-hora de creacion 
# y la cuenta de twitter
tweets11 <- tweets1[,c("text","created_at","screen_name")]

# Reemplazaremos por un campo vacio los tweets que no tengan letras, numeros, # y @
tweets11$text2 = str_replace_all(tweets11$text, "[^0-9a-zA-Z??????????????????????????#@]", " ")

# Esta es la otra alternativa que os comentaba de stopwords predefinidas en la libreria tm
# Corpus son colecciones de documentos que contienen textos (en lenguaje natural). 
tweets_corpus = tm::Corpus(VectorSource(tweets11$text2))
tm::stopwords(kind="en")
tm::stopwords(kind="es")

# Se filtran las stopwords y los siguientes elementos que aportan ruido al analisis.
# removePunctuation: Eliminaremos los signos de puntuacion de un documento de texto
# removeNumbers: Eliminaremos los numeros
# tolower: convertiremos los textos en minusculas
tdm <- TermDocumentMatrix(tweets_corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("amp", "https",tm::stopwords(kind = "en")),
                                         removeNumbers = TRUE, tolower = TRUE))

# Lo anterior lo definiremos como una matriz, si sale algun error, debemos ampliar
# la memoria para convertirlo a matriz usando: memory.limit(size = 16000)
m <- as.matrix(tdm)

# Contaremos las palabras en orden decreciente
word_freqs <- sort(rowSums(m), decreasing=TRUE) 

# Crearemos un data-frame con palabras y frecuencias de esas palabras
dm <- data.frame(word=names(word_freqs), freq=word_freqs, stringsAsFactors = FALSE)

# Graficaremos la nube de palabras utilizando wordcloud2
wordcloud2(data=dm)

# Finalmente os dejo aqui los pasos para hacer un wordcloud desde cero.
# Ustedes tendriais que hacer una funcion propia para automatizar el proceso.
usuario <- 'sanchezcastejon'
tweets_user <- get_timelines(user = usuario, n = 2000)
tweets_PS <- tweets_user[,c("text","created_at","screen_name")]
tweets_PS$text2 = str_replace_all(tweets_PS$text, "[^0-9a-zA-Z??????????????????????????#@]", " ")
tweets_corpus_PS = Corpus(VectorSource(tweets_PS$text2))
tdm_PS = TermDocumentMatrix(tweets_corpus_PS,
                            control = list(removePunctuation = TRUE,
                                           stopwords = c("https", tm::stopwords(kind = "es")),
                                           removeNumbers = TRUE, tolower = TRUE))
m_PS = as.matrix(tdm_PS)
word_freqs_PS = sort(rowSums(m_PS), decreasing=TRUE) 
dm_PS = data.frame(word=names(word_freqs_PS), 
                   freq=word_freqs_PS, stringsAsFactors = FALSE)
dm_PS = dm_PS[(dm_PS$word!="pedro" & dm_PS$word!="sanchez"),]
dm_PS = dm_PS[dm_PS$freq>3,]
wordcloud2(dm_PS)

## ********************************************************************
## 5.3 Correlacion entre usuarios por palabras utilizadas 
## ********************************************************************
# Una forma de cuantificar la similitud entre los perfiles de dos usuarios de Twitter es 
# calculando la correlacion en el uso de palabras. La idea es que, si dos usuarios escriben 
# de forma similar, tenderan a utilizar las mismas palabras y con frecuencias similares.

# Para poder generar graficos de correlaciones se necesita disponer de cada variable en una columna. 
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

# En este caso, las variables a correlacionar son los usuarios.
cor.test(~ BillGates + elonmusk, method = "pearson", data = tweets_spread)

# Graficamente podemos ver esa correlacion del siguiente modo (localizar palabras)
ggplot(tweets_spread, aes(elonmusk, BillGates)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Para poder valorar adecuadamente el nivel de correlacion seria interesante conocer el 
# numero de palabras comunes entre cada par de autores.
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="BillGates") %>%
                                       select(token)) %>% nrow()
paste("Numero de palabras comunes entre Elon Musk y Bill Gates", palabras_comunes)

## ********************************************************************
## 5.4 Comparacion en el uso de palabras
## ********************************************************************
# En este punto estudiaremos que palabras se utilizan de forma mas diferenciada por cada 
# usuario, es decir, palabras que utiliza mucho un autor y que no utiliza el otro.
# Una forma de hacer este analisis es mediante el log of odds ratio (cociente de probabilidades) 
# de las frecuencias.

# Para realizar este calculo es necesario que, para todos los usuarios, se cuantifique 
# la frecuencia de cada una de las palabras que aparecen en el conjunto de tweets, es decir, 
# si un autor no ha utilizado una de las palabras que si ha utilizado otro, debe aparecer 
# esa palabra en su registro con frecuencia igual a cero. Hay una serie de pasos:

# Pivotaje y despivotaje
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% dplyr::count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)

# Seleccion de los autores elonmusk y BillGates
tweets_unpivot <- tweets_unpivot %>% filter(autor %in% c("elonmusk",
                                                         "BillGates"))
# Se anade el total de palabras de cada autor
tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>% group_by(autor) %>%
                                                 dplyr::summarise(N = n()), by = "autor")
# Calculo de odds y log of odds de cada palabra
tweets_logOdds <- tweets_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>% 
  spread(key = autor, value = odds)
tweets_logOdds <- tweets_logOdds %>%  mutate(log_odds = log(elonmusk/BillGates),
                                             abs_log_odds = abs(log_odds))

# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es asi porque el ratio se ha
# calculado como elonmusk/BillGates.
tweets_logOdds <- tweets_logOdds %>% mutate(autor_frecuente = if_else(log_odds > 0,
                                                                      "@elonmusk", "@BillGates"))
tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head()

# Ahora representamos, por ejemplo, las 30 palabras mas diferenciadas ya que 
# esas palabras posiblemente tendran mucho peso a la hora de clasificar los tweets
tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() + labs(x = "palabra", y = "log odds ratio (@elonmusk / @BillGates)") +
  coord_flip() + theme_bw()

## ********************************************************************
## 5.5 Relacion entre palabras
## ********************************************************************
# En todos los analisis anteriores, se han considerado a las palabras como unidades 
# individuales e independientes. Esto es una simplificacion bastante grande, ya que en 
# realidad el lenguaje se crea por combinaciones no aleatorias de palabras, es decir, 
# determinadas palabras tienden a utilizarse de forma conjunta. A continuacion se 
# muestran algunas formas de calcular, identificar y visualizar relaciones entre palabras.

# Lo que vamos a hacer es dividir el texto por n-gramas, siendo cada n-grama una secuencia 
# de n palabras consecutivas. Para conseguir los n-gramas, se tiene que eliminar la tokenizacion 
# de la funcion creada 'limpiar_tokenizar'. Luego, crearemos otra funcion en este caso.
limpiar <- function(texto){
  
  # Se convierte todo el texto a minusculas
  nuevo_texto <- tolower(texto)
  
  # Eliminacion de paginas web (palabras que empiezan por "http." seguidas de cualquier 
  # cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  
  # Eliminacion de signos de puntuacion
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminacion de numeros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminacion de espacios en blanco multiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  
  # Output
  return(nuevo_texto)
}

# Obtenemos el bigramas (concatenacion de 2 palabras)
bigramas <- tweets %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>% unnest_tokens(input = texto, output = "bigrama",
                                  token = "ngrams",n = 2, drop = TRUE)

# Frecuencia de ocurrencias de cada bigrama
bigramas %>% dplyr::count(bigrama, sort = TRUE)

# Los bigramas mas frecuentes son los formados por stopwords. Como la relacion entre 
# estas palabras no aporta informacion de interes, se procede a eliminar todos aquellos 
# bigramas que contienen alguna stopword. Separacion de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"), sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Union de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Identificamos ahora los bigramas mas frecuentes
head(bigramas %>% dplyr::count(bigrama, sort = TRUE), 50)

# Una forma mas visual e informativa de analizar las relaciones entre palabras es 
# mediante el uso de redes de n-gramas.
graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  dplyr::count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 10) %>% graph_from_data_frame(directed = FALSE)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

## ********************************************************************
## 5.6 El estadistico td-idf
## ********************************************************************
# Uno de los principales intereses en text mining es cuantificar la tematica de un texto, asi como 
# la importancia de cada termino que lo forma. Una manera sencilla de medir la importancia de un termino 
# dentro de un documento es utilizando la frecuencia con la que aparece (Term Frequency) que se denota por tf.

# Esta aproximacion, aunque simple, tiene la limitacion de atribuir mucha importancia a aquellas palabras que 
# aparecen muchas veces aunque no aporten informacion selectiva. Por ejemplo, si la palabra matematicas aparece 
# 5 veces en un documento y la palabra pagina aparece 50, la segunda tendra 10 veces mas peso a pesar de que no 
# aporte tanta informacion. 

# Para solucionar este problema se pueden ponderar los valores tf multiplicandolos por la inversa de la frecuencia 
# con la que el termino en cuestion aparece en el resto de documentos del corpus (Inverse Document Frequency) que 
# se denota por idf. De esta forma, se consigue reducir el valor de aquellos terminos que aparecen en muchos 
# documentos y que, por lo tanto, no aportan informacion selectiva.

# Por tanto, el estadistico tf-idf mide como de importante es un termino en un documento teniendo en cuenta la frecuencia 
# con la que ese termino aparece en otros documentos. Veamos como lo calculamos. Primero, calcularemos el tf, luego el idf
# y por ultimo el estadístico tf-idf.

# Numero de veces que aparece cada termino por tweet
tweets_tf <- tweets_tidy %>% group_by(tweet_id, token) %>% dplyr::summarise(n = n())

# Se anade una columna con el total de terminos por tweet
tweets_tf <- tweets_tf %>% mutate(total_n = sum(n))

# Se calcula el tf (Term Frequency) de cada palabra. Es decir, tf(término) = n(término) / longitud documento
tweets_tf <- tweets_tf %>% mutate(tf = n / total_n )
head(tweets_tf)

# Comprobamos el numero total de documentos (tweets)
total_documentos = tweets_tidy$tweet_id %>% unique() %>% length()
total_documentos

# Numero de documentos en los que aparece cada termino
tweets_idf <- tweets_tidy %>% distinct(token, tweet_id) %>% group_by(token) %>%
  dplyr::summarise(n_documentos = n())

# Se calcula el idf (Inverse Document Frequency) de cada palabras. Es decir, idf (término) = log (n(documentos)/nd(ocumentos con el término))
tweets_idf <- tweets_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(tweets_idf)

# Por ultimo, se obtiene el estadistico tf-idf
tweets_tf_idf <- left_join(x = tweets_tf, y = tweets_idf, by = "token") %>% ungroup()
tweets_tf_idf <- tweets_tf_idf %>% mutate(tf_idf = tf * idf) %>% select(-tweet_id) 
View(tweets_tf_idf)

# En este caso como podeis ver todos los terminos que aparecen una vez tienen el mismo valor de tf, 
# sin embargo, dado que no todos los terminos aparecen con la misma frecuencia en el conjunto de todos los tweets, 
# la correccion de idf es distinta para cada uno. Valores mas altos significan que tendran un mayor peso.

## 6 Como analizar la opinion, polaridad o sentimiento sobre un individuo con datos de texto en Twitter ----
#_____________________________________________________________________#
# El enfoque semantico se caracteriza por el uso de diccionarios de terminos (lexicons) con orientacion 
# semantica de polaridad u opinion. Tipicamente los sistemas preprocesan el texto y lo dividen en palabras, 
# con la apropiada eliminacion de las palabras de parada (stopwords). Luego, se comprueba la aparicion de 
# los terminos del lexicon para asignar el valor de polaridad del texto mediante la suma del valores de 
# polaridad de los terminos. 

# En este primer ejemplo vamos a analizar el sentimiento de los tweets publicados por elonmusk y billgates. 
# En este caso, vamos a estudiar la opinion o polaridad sobre estos individuos con datos de Twitter.
# La metodologia a seguir seria la siguiente (3 etapas):

## ********************************************************************
## 6.1 Sentimiento promedio de cada tweet
## ********************************************************************
# Empleamos la clasificacion positivo/negativo proporcionada por el diccionario bing (libreria tidytext)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

# Para facilitar el calculo de sentimientos globales (autor, tweet…) se recodifican los sentimientos como 
# +1 para positivo y -1 para negativo.
sentimientos <- sentimientos %>% mutate(valor = if_else(sentiment == "negative", -1, 1))

# Al disponer de los datos en formato tidy (una palabra por fila), mediante un inner join se anade a cada palabra 
# su sentimiento y se filtran automaticamente todas aquellas palabras para las que no hay informacion disponible.
tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos,
                          by = c("token" = "word"))

# Se suman los sentimientos de las palabras que forman cada tweet.
tweets_sent %>% group_by(autor, tweet_id) %>%
  dplyr::summarise(sentimiento_promedio = sum(valor)) %>%
  head()

## ********************************************************************
## 6.2 Porcentaje de tweets positivos, negativos y neutros por autor 
## ********************************************************************
# Obtenemos ese porcentaje de tweets para cada autor. Vemos como los dos autores tienen un perfil muy similar. 
# La gran mayoria de tweets son de tipo positivo. Este patron es comun en redes sociales, donde se suele participar 
# mostrando aspectos o actividades positivas. Los usuarios no tienden a mostrar las cosas malas de sus vidas.
tweets_sent %>% group_by(autor, tweet_id) %>%
  dplyr::summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  dplyr::summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
                   neutros = 100 * sum(sentimiento_promedio == 0) / n(),
                   negativos = 100 * sum(sentimiento_promedio  < 0) / n())

# Podemos visualizarlo graficamente del siguiente modo
tweets_sent %>% group_by(autor, tweet_id) %>%
  dplyr::summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  dplyr::summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
                   neutros = 100*sum(sentimiento_promedio == 0) / n(),
                   negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw()

## ********************************************************************
## 6.3 Evolucion de los sentimientos en funcion del tiempo
## ********************************************************************
# A continuacion, se estudia como varia el sentimiento promedio de los tweets agrupados por intervalos 
# de un mes para cada uno de los usuarios. Podemos ver como la distribucion del sentimiento promedio de 
# los tweets se mantiene aproximadamente constante para los 2 usuarios. Existen ciertas oscilaciones, 
# pero todas ellas dentro del rango de sentimiento POSITIVO.
tweets_sent %>% mutate(anyo = year(fecha),
                       mes = month(fecha),
                       anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
  group_by(autor, anyo_mes) %>%
  dplyr::summarise(sentimiento = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
  geom_point() + 
  geom_smooth(formula = y ~ x, method = 'loess') + 
  labs(x = "fecha de publicación") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")

# Veamos un segundo ejemplo. Consideremos el mismo politico de referencia para realizar el analisis de sentimientos
politico <- "Pedro Sanchez"
geocode <- '40.417764,-3.7058457,650km'
n <- 2000
tweetps <- search_tweets(politico, geocode = geocode ,n=n, include_rts = FALSE)
tweets2 <- tweetps[,c("text","created_at","screen_name")]
tweets2$text2 = str_replace_all(tweets2$text, "[^0-9a-zA-Z??????????????????????????#@]", " ")

# Cargaremos el siguiente fichero para obtener las palabras positivas y negativas necesarias 
source("positive_negative.R",encoding = 'UTF-8')

# Visualizaremos una muestra de las palabras positivas y negativas
positivo[1:20]
negativo[1:20]

# Se construye una funcion para generar la puntuacion por cada tweet
score_function<-function(sentence, pos.words, neg.words){
  
  # Los caracteres diferentes a este vector de valores [a-zA-Z?????????????????????????? ] 
  # se reemplazan por vacios
  sentence = str_replace_all(sentence, "[^a-zA-Z?????????????????????????? ]", "") 
  
  # Se reemplazan las palabras a minusculas
  sentence = tolower(sentence) 
  
  # Se divide una oracion en palabras
  word.list = str_split(sentence, '\\s+') 
  
  # Se convierte la lista a vector
  words = unlist(word.list) 
  
  # Se asigna como 1 la posicion del vector coincidente con las palabras positivas
  pos.matches = match(words, pos.words) 
  
  # Se asigna como -1 la posicion del vector coincidente con las palabras negativas
  neg.matches = match(words, neg.words) 
  
  # Se asigna como verdadero si no es NA y falso si es NA con las palabras positivas
  pos.matches = !is.na(pos.matches) 
  
  # Se asigna como verdadero si no es NA y falso si es NA con las palabras negativas
  neg.matches = !is.na(neg.matches) 
  
  # Se calcula la puntuacion (los verdaderos se representan como uno y los falsos como ceros)
  score = sum(pos.matches) - sum(neg.matches) 
  return(score)
}

# Se construye una segunda funcion que genere una tabla con todos los tweets puntuados
score.sentiment = function(sentence, pos.words, neg.words){
  scores = laply(sentence, score_function, pos.words, neg.words)
  scores.df = data.frame(score=scores, text=sentence)
  return(scores.df)
}

# Generamos los resultados para nuestro ejemplo
resultado <- score.sentiment(tweets2$text2, positivo, negativo) 
View(resultado)

# Por ultimo calculamos el porcentaje de tweets positivos
mean(resultado$score>0)*100

# Por ultimo calculamos el porcentaje de tweets negativos
mean(resultado$score<0)*100

# Por ultimo calculamos el porcentaje de tweets neutros
mean(resultado$score==0)*100

# En este otro ejemplo utilizaremos el diccionario nrc que clasifica cada palabra en uno o mas 
# de los siguientes sentimientos: positivo, negativo, ira, anticipacion, asco, miedo, alegria, 
# tristeza, sorpresa y confianza. 

# La clasificacion de las emociones se basa en el NRC Word-Emotion Association Lexicon (tambien
# conocido como EmoLex). La definición de NRC Emotion Lexicon extraida de http://saifmohammad.com/
# WebPages/NRC-Emotion-Lexicon.htm es el siguiente. El NRC Emotion Lexicon es una lista de palabras 
# en ingles y sus asociaciones con ocho emociones basicas (ira, miedo, expectativas-ilusión, confianza, 
# sorpresa, tristeza, alegria y asco) y dos sentimientos (negativo y positivo).

# Obtenemos un data frame en el que cada fila representa un tweet. El data frame tiene diez columnas: 
# una columna para cada una de las ocho emociones, una columna para la valencia del sentimiento positivo 
# y otra para la valencia del sentimiento negativo. Lo aplicaremos sobre los tweets de billgates.
tweetsbg <- billgates %>% select(screen_name, created_at, status_id, text)
tweetsbg <- tweetsbg %>% dplyr::rename(autor = screen_name, fecha = created_at, texto = text, tweet_id = status_id)
resultado_nrc <- syuzhet::get_nrc_sentiment(tweetsbg$texto)
head (resultado_nrc,10)

# Podemos graficar los resultados del siguiente modo para los primeros 250 tweets (frecuencia palabras)
td <- data.frame(t(resultado_nrc))
td_new <- data.frame(rowSums(td[1:250]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, xlab="Emociones", ylab="Frecuencia")+ggtitle("Analisis sentimientos tweets de Bill Gates")

# Otro grafico expresado en procentaje (valores relativos) 
barplot(
  sort(colSums(prop.table(resultado_nrc[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emociones en los tweets de Bill Gates", xlab="Porcentaje"
)





## 7 Como clasificar/predecir la autoria de un texto con datos de Twitter ----
#_____________________________________________________________________#
# Los enfoques basados en aprendizaje computacional consisten en entrenar un clasificador usando 
# un algoritmo de aprendizaje supervisado a partir de una coleccion de textos anotados. Cada texto 
# habitualmente se representa con un vector de palabras (bag of words) en combinacion con otro tipo 
# de caracteristicas semanticas que intentan modelar la estructura sintactica de las frases. 

# Los sistemas utilizan diversas tecnicas, aunque los mas populares son los clasificadores basados 
# en SVM (Support Vector Machines), Random Forest y Redes Neuronales. Recordad que aqui estas tecnicas
# no las utilizaremos para realizar REGRESIONES sino para problemas de CLASIFICACION.

# En este primer ejemplo se construira un modelo de aprendizaje estadistico basado en maquinas de vector 
# soporte (SVM) con el objetivo de predecir la autoria de los tweets. En concreto, se comparan los tweets 
# de Elon Musk y Bill Gates. La metodologia a seguir sera la siguiente (5 etapas):

## ********************************************************************
## 2.1 Separación de los datos en entrenamiento y test
## ********************************************************************
# En todo proceso de aprendizaje estadistico es recomendable repartir las observaciones en un set de entrenamiento 
# y otro de test. Esto permite evaluar la capacidad del modelo. Para este ejercicio se selecciona como test un 20% 
# aleatorio de los tweets. Es importante fijar una semilla para poder replicar los resultados.
set.seed(34)
tweets_elon_ed <- bind_rows(elonmusk, billgates)
tweets_elon_ed <- tweets_elon_ed %>% select(screen_name, created_at, status_id, text)
tweets_elon_ed <- tweets_elon_ed %>% dplyr::rename(autor = screen_name, fecha = created_at, texto = text, tweet_id = status_id)
train          <- sample(x = 1:nrow(tweets_elon_ed), size = 0.8 * nrow(tweets_elon_ed))
tweets_train   <- tweets_elon_ed[train, ]
tweets_test    <- tweets_elon_ed[-train, ]

# Es importante verificar que la proporcion de cada grupo es similar en el set de entrenamiento 
table(tweets_train$autor) / length(tweets_train$autor)

# Es importante verificar que la proporcion de cada grupo es similar en el set de test
table(tweets_test$autor) / length(tweets_test$autor)

## ********************************************************************
## 2.2 Vectorización tf-idf
## ********************************************************************
# Empleando los tweets de entrenamiento se crea un matriz tf-idf en la que cada columna es un termino, cada fila un 
# documento y el valor de interseccion el tf-idf correspondiente. Esta matriz representa el espacio n-dimensional en 
# el que se proyecta cada tweet. Las funciones dfm() y tfidf() del paquete quanteda automatizan la creacion una matriz 
# df-idf a partir de un corpus de documentos. Ademas, incorpora un tokenizador con multiples opciones de limpieza.
texto <- paste0("Esto es 1 ejemplo de l'limpieza de6 TEXTO", "https://t.co/rnHPgyhx4Z @elonmusk #textmining")
matriz_tfidf <- quanteda::dfm(x = texto, what = "word", remove_numbers = TRUE,
                              remove_punct = TRUE, remove_symbols = TRUE,
                              remove_separators = TRUE, remove_twitter = FALSE,
                              remove_hyphens = TRUE, remove_url = FALSE)
colnames(matriz_tfidf)

# Viendo los resultados de la tokenizacion realizada mediante quanteda, parece que la funcion de limpieza y tokenización 
# definida por nosotros en el script 2 funciona mejor para la limpieza de tweets. Hemos utilizado el mismo ejemplo para comprobarlo.
limpiar_tokenizar(texto = texto)

# La funcion dfm() interpreta cada elemento de un vector character como un documento distinto. Como el resultado de nuestra funcion 
# limpiar_tokenizar() trasforma cada tweet en un vector de palabras, es necesario concatenar el resultado para que formen de nuevo un 
# unico elemento si recordais.
paste(limpiar_tokenizar(texto = texto), collapse = " ")

# Tras esta breve comparativa sobre las funciones para limpiar el texto y tokenizar, lo aplicamos sobre nuestro ejemplo.
# Limpieza y tokenizacion de los documentos de entrenamiento
tweets_train$texto <- tweets_train$texto %>% purrr::map(.f = limpiar_tokenizar) %>%
  purrr::map(.f = paste, collapse = " ") %>% unlist()

# Creacion de la matriz documento-termino
matriz_tfidf_train <- quanteda::dfm(x = tweets_train$texto, remove = lista_stopwords)

# Se reduce la dimension de la matriz eliminando aquellos terminos que aparecen en menos de 5 documentos. 
# Con esto se consigue eliminar ruido.
matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 5)

# Conversion de los valores de la matriz a tf-idf
matriz_tfidf_train <- quanteda::dfm_tfidf(matriz_tfidf_train, scheme_tf = "prop",
                                          scheme_df = "inverse")
matriz_tfidf_train

# A la hora de trasformar los documentos de test, es importante proyectarlos en la misma matriz obtenida previamente con el 
# set de entrenamiento. Esto es importante ya que, si en los documentos de test hay algun termino que no aparece en los de 
# entrenamiento o viceversa, las dimensiones de cada matriz no coincidiran. Para evitar este problema, al crear la matriz tf-idf 
# de test, se pasa como argumento dictionary el nombre de las columnas de la matriz tf-idf de entrenamiento. El argumento 
# dictionary tiene que ser de tipo diccionario, la transformacion de un vector a un diccionario es un tanto compleja ya que tiene 
# que convertirse primero a lista. Veamoslo en nuestro ejemplo.

# Limpieza y tokenizacion de los documentos de test
tweets_test$texto <- tweets_test$texto %>% purrr::map(.f = limpiar_tokenizar) %>%
  purrr::map(.f = paste, collapse = " ") %>% unlist()

# Identificacion de las dimensiones de la matriz de entrenamiento
# Los objetos dm() son de clase S4, se accede a sus elementos mediante @
dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features

# Conversion de vector a diccionario pasando por lista
dimensiones_matriz_train        <- as.list(dimensiones_matriz_train)
names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
dimensiones_matriz_train        <- dictionary(dimensiones_matriz_train)

# Proyeccion de los documentos de test
matriz_tfidf_test <- quanteda::dfm(x = tweets_test$texto,
                                   dictionary = dimensiones_matriz_train)
matriz_tfidf_test <- quanteda::dfm_tfidf(matriz_tfidf_test, scheme_tf = "prop",
                                         scheme_df = "inverse")
matriz_tfidf_test

# Por utlimo se comprueba que las dimensiones de ambas matrices sean iguales.
all(colnames(matriz_tfidf_test) == colnames(matriz_tfidf_train))

## ********************************************************************
## 2.3 Estimación y ajuste del modelo SVM lineal
## ********************************************************************
# Utilizaremos un modelo SVM para clasificar nuestros tweets. Este metodo de aprendizaje estadistico suele dar buenos resultados 
# en clasificacion. Para una informacion mas detallada consultar el libro que os he subido a la intranet Support Vector Machines.
modelo_svm <- e1071::svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                         kernel = "linear", cost = 1, scale = TRUE,
                         type = "C-classification")
modelo_svm

## ********************************************************************
## 2.4 Predicciones y errores del modelo SVM lineal
## ********************************************************************
# Empleando el modelo entrenado en el paso anterior se predice la autoria de los tweets de test.
predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)

# Calculamos los errores de clasificacion
clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("Numero de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2), "%")

## ********************************************************************
## 2.5 Optimización parametro C del modelo SVM lineal
## ********************************************************************
# En este caso, el metodo de SVM lineal tiene un unico parametro C que establece la penalizacion 
# por clasificacion incorrecta, regulando asi el balance entre bias y varianza. En estos modelos
# el valor optimo del parametro C no se aprende en el proceso de entrenamiento, para estimarlo hay 
# que recurrir a validacion cruzada.
svm_cv <- e1071::tune("svm", train.x =  matriz_tfidf_train,
                      train.y = as.factor(tweets_train$autor),
                      kernel = "linear", ranges = list(cost = c(0.1, 0.5, 1, 2.5, 5)))
summary(svm_cv)

# Lo visualizamos graficamente
ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error - dispersion, ymax = error + dispersion)) +
  theme_bw()

# Acorde al error estimado por validacion cruzada, el valor optimo del parametro C es 0.5. 
# Luego, se tiene que reajustar el modelo con este valor.
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                  kernel = "linear", cost = 0.5, scale = TRUE)
predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
table(observado = tweets_test$autor, predicho = predicciones)

# Empleando un modelo de SVM lineal con parametro C = 0.5 se consigue un porcentaje de error del 7.97%. 
# Se trata de un porcentaje de error bajo. Tambien  se podria comparar con otros modelos de clasificación 
# como pueden ser SVM no lineales o Random Forest para ver si obtendriamos mejores resultados. En este caso 
# lo dejaremos para un futuro.
clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2), "%")

# Habriamos conseguido construir un modelo de aprendizaje estadistico basado en maquinas de vector 
# soporte (SVM) con el objetivo de predecir la autoria de los tweets de elonmusk y billgates con
# una tasa de error del 7.97%. De cada 100 tweets analizados nos equivocariamos en menos de 8.

# Veamos un segundo ejemplo en el que en lugar de ver el error que cometeriamos a la hora de asignar 
# un tweet a un autor nos vamos a centrar en identificar los tweets positivos y su porcentaje. Para ello, 
# tendreis que cargar el fichero con los datos de diferentes tweets del mismo politico de referencia que 
# en los casos anteriores.
source("train_twitter.R")
View(train)

# En este caso creamos una funcion para automatizar el proceso de 'aprendizaje supervisado'
aprendizaje_computacional<-function(politico,geocode,n){
  
  # Nos conectamos a la API de twitter y descargamos los tweets 
  search_tw <- search_tweets(politico, geocode = geocode ,n=n, include_rts = FALSE)
  
  # Extraemos los tweets
  text <- search_tw$text
  
  # Reemplazamos por vacio todo lo que no se encuentre dentro de los corchetes
  text <- str_replace_all(text, "[^0-9a-zA-Z???????????????????????????? ]", "") 
  
  # Creamos un data frame con dos columnas
  test <- data.frame(text,sentiment=9) 
  
  # Unimos por filas dos data frame, es decir, la tabla de entrenamiento con la de test
  tweets <- rbind(train,test) 
  
  # Definimos el conjunto de datos que vamos a analizar
  mach_text <- tweets$text
  mach_dataframe <- data.frame(doc_id=paste("doc_",c(1:length(mach_text))), 
                               text=mach_text,
                               stringsAsFactors = FALSE)
  
  # Reemplazamos las palabras a minusculas
  mach_dataframe$text <- tolower(mach_dataframe$text) 
  mach_dataframe <- DataframeSource(mach_dataframe)
  
  # Creamos un corpus a partir de los textos
  corpus_tw <- Corpus(mach_dataframe) 
  
  # Eliminamos las stopwords, excepto "no"(16) y "si"(29)
  corpus_tw <- tm_map(corpus_tw, removeWords, c("rt ",stopwords("spanish")[c(-16,-29)])) 
  
  # Realizamos un stemming en un documento de texto utilizando el algoritmo de Porter
  corpus_tw <- tm_map(corpus_tw, stemDocument, language = "spanish") 
  
  # Transformamos el corpus a una matriz de termino-documento.
  frequencies <- DocumentTermMatrix(corpus_tw) 
  
  # Encontramos terminos frecuentes en una matriz de termino-documento donde la frecuencia minima esta dada por lowfreq
  findFreqTerms(frequencies,lowfreq = 10)  
  
  # Nos quedamos con las palabras que tienen por lo menos un 0.5% de frecuencia
  sparse <- removeSparseTerms(frequencies,0.995) 
  
  # Lo convertimos a data frame
  sparse <- as.data.frame(as.matrix(sparse)) 
  
  # Asignamos el sentimiento del train y test (para el test es 9 porque no tiene asignado el sentimiento)
  sparse$sentiment <- tweets$sentiment 
  
  # Observamos la estructura de la matriz
  sparse[1:10,1:10] 
  table(sparse$sentiment)
  
  # Creamos la matriz de entrenamiento
  tweet_train <- sparse[1:nrow(train),] 
  table(tweet_train$sentiment)
  
  # Como el conjunto de palabras en el train y en el test no son iguales, creamos un vector de 
  # palabras en el train con frecuencia mayor a cero y asignamos ese conjunto de palabras en el test
  col_name <- data.frame(freq=colSums(tweet_train))
  col_name$words <- row.names(col_name)
  col_name <- col_name[col_name$freq!=0,]
  col_name <- col_name$words
  
  # Filtramos las palabras del train cuya frecuencia sea mayor que cero en toda la matriz
  sparse <- sparse[,col_name] 
  
  # Matriz de entrenamiento con un conjunto de palabras mas frecuente
  tweet_train <- sparse[1:nrow(train),] 
  table(tweet_train$sentiment)
  
  # Matriz de test con un conjunto de palabras mas frecuente
  tweet_test<-sparse[(nrow(train)+1):nrow(sparse),] 
  table(tweet_test$sentiment)
  
  tweet_train$sentiment <- as.factor(tweet_train$sentiment)
  levels(tweet_train$sentiment)
  
  # Aplicamos el clasificador Support Vector Machine (SVM) 
  svm_train <- svm(sentiment~.,data=tweet_train) 
  summary(svm_train)
  
  # Haciendo predicciones con los datos de entrenamiento
  prediction <- predict(svm_train,newdata = tweet_train) 
  
  # Generando la matriz de confusion para conocer el Accuracy
  confusionMatrix(as.factor(tweet_train$sentiment),prediction) 
  
  # Nos devuelve el porcentaje relativo de tweets positivos
  prediction      <- predict(svm_train,newdata = tweet_test)
  test$prediction <- prediction
  table(test$prediction)
  return(mean(test$prediction==1)) 
}

# Finalmente aplicamos nuestra funcion a un caso particular
politico <- "Pedro Sanchez"
geocode <- '40.417764,-3.7058457,650km'
n <- 100

# La salida nos proporciona la tasa de tweets positivos. Os recomiendo que hagais varias pruebas
# y modificaciones en la funcion para obtener otro tipo de info.
out<-aprendizaje_computacional(politico = politico,geocode = geocode, n=n)





