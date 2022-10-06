########################################
# Web scraping producción científica UCM

########################################
# https://produccioncientifica.ucm.es/

# ------------------------------------------------------------------------------
# Preliminares
# ------------------------------------------------------------------------------
# Es conveniente (NO necesario) un "Selector Gadget extension" para instalar en el chrome
# fuente: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# Recomendable un conocimiento mínimo de HTML & CSS 
# Aunque en realidad sólo necesitamos el nombre del <tag> o etiqueta que queremos guardar 

# ------------------------------------------------------------------------------
# Librerías utilizadas
# ------------------------------------------------------------------------------
library(rvest) # esta es la principal librería para leer la página
library(stringr) # para depurar el contenido del texto que nos bajamos
library(httr) # si queremos usar proxy para cambiar IP: ##content(GET("https://ifconfig.co/json", use_proxy("138.201.63.123", 31288)), "parsed")
library(xml2) # para leer xlm
library(rjson) # para leer el texto de los javascript
library(robotstxt) # trae funciones interesantes para webscraping
library(RCurl) # para gestionar objetos http
library(genero) # para saber genero en español y portugues
#library(genderizeR) # ojo que esta está lmitada a 1000 diarias
#library(gender) # para saber género countries = c("United States", "Canada", "United Kingdom", "Denmark", "Iceland","Norway", "Sweden")

library(tidyverse) # para manipular data.frames


# Generamos una función para detectar errores
is.error <- function(x) inherits(x, "try-error")


# ------------------------------------------------------------------------------
# Funciones utilizadas con rvest
# ------------------------------------------------------------------------------
# rvest::read_html() lee pagweb
# rvest::html_node(objeto, tag) sirve para leer una etiqueta particular desde una pag bajadacon read_html
# rvest::html_name() para obtner todos los nombres
# rvest::html_text() para extraer texto de un nodo extraido con html_node  
# rvest::html_attr()
# rvest::html_attrs()
# rvest::html_table() para extraer texto de un nodo extraido con html_node


# ------------------------------------------------------------------------------
# Normlamente el procedimiento es
# ------------------------------------------------------------------------------
# url <- "http...."
# pagweb <- read_html(url)
# nodoweb <- html_node(pagweb,"tag")
# textoweb <- html_text(nodoweb)
# tablaweb <- html_table(nodoweb)

# o alternativamente

# read_html(url) %>% 
#  html_node("tag") %>% 
#   html_text()


# ------------------------------------------------------------------------------
# Confirmamos que podemos hacer web scraping
# ------------------------------------------------------------------------------
# robotstxt::paths_allowed(paths = url)
url <- "https://produccioncientifica.ucm.es/investigadores"
robotstxt::paths_allowed(paths = url)
# TRUE


# ------------------------------------------------------------------------------
# 1. Lectura de links facultades: datosFacu 
# ------------------------------------------------------------------------------

# descarga de links de las facultdes
url <- "https://produccioncientifica.ucm.es/investigadores"

# Vamos a descargar número de investigadores por facultad
pagwebfacult <- read_html(url)

# Nombre facultad
nombreFacu <- pagwebfacult %>% 
  html_node("ul") %>%  
  html_children() %>% 
  html_element("a") %>% 
  html_text()

# Numero de personas por facultad
numeroPDI <- pagwebfacult%>% 
  html_node("ul") %>%  
  html_children() %>% 
  html_element("span") %>% 
  html_text()

# Link de la facultad
linkFacu <- pagwebfacult %>% 
  html_node("ul") %>%  
  html_children() %>% 
  html_element("a") %>% 
  html_attr("href")
linkFacu <- paste0("https://produccioncientifica.ucm.es",linkFacu)

# Creamos el data.frame con esta infromacion
datosFacu <- data.frame(nombreFacu,numeroPDI,linkFacu)
save(datosFacu,file="DatosFacultades.RData")


#load("DatosFacultades.RData")


# ------------------------------------------------------------------------------
# 2 Lectura de link de cada profesor: datosPDI 
# ------------------------------------------------------------------------------

# Número de Facultades
numfacult<-nrow(datosFacu)

# Número total de investigadores
Total_PDI <- datosFacu %>%
  summarise(sum(as.numeric(numeroPDI)))
Total_PDI

# Reinicio
datosPDI<-data.frame()
pb <- txtProgressBar(min = 0, max = numfacult, style = 3)

for (i in 1:numfacult){

  # Cargamos el link de la facultad (investigadores) --> datosPDI/linkFacu
  # url <- "https://produccioncientifica.ucm.es/unidades/782/investigadores"
  url <- datosFacu$linkFacu[i]
  
  # Leemos la url seleccionada
  pagwebPDI <- read_html(url)
  
  # Extraemos el nombre de la Facultad
  pagwebPDI %>% 
    html_nodes("h1")%>% 
    html_text()
  
  # Extraemos los link de cada investigador 
  link <- pagwebPDI %>% 
    html_nodes(".unidad-miembros__items")%>%  
    html_children() %>%
    html_element(".c-persona-card__detalles a") %>%  
    html_attr('href')   
  link <- paste0("https://produccioncientifica.ucm.es",link)
  
  # Extraemos el nombre de todos los investigadores por facultad
  nombre <- pagwebPDI %>% 
    html_nodes(".unidad-miembros__items")%>%  
    html_children() %>%
    html_element(".c-persona-card__nombre") %>% 
    html_text()   
  
  # Extraemos los apellidos de todos los investigadores por facultad
  apellidos <- pagwebPDI %>% 
    html_nodes(".unidad-miembros__items")%>%  
    html_children() %>%
    html_element(".c-persona-card__apellidos") %>%  
    html_text()   
  
  # Extraemos el área de todos los investigadores por facultad
  area <- pagwebPDI %>% html_nodes(".unidad-miembros__items")%>%  
    html_children() %>%
    html_element(".c-persona-card__area") %>%  
    html_text()   
  
  # Extraemos el sexo
  nombrePDI_Primero <- unlist(lapply(str_split(nombre," "), `[[`, 1))
  sexoPDI <- genero(nombrePDI_Primero,result_as = c(male="varón",female="mujer"))
  
  # Creamos data Frame con todos los links de la facultad
  datosPDI<-rbind.data.frame(datosPDI, data.frame(facultad=datosFacu$nombreFacu[i], nombre, apellidos, sexoPDI,area, linkPDI=link))
  
  # Actualización de la barra
  setTxtProgressBar(pb, i)  
}

# Creo el ID con el numero de url que tengan
datosPDI$Id_PDI <- NA
datosPDI$Id_PDI <- str_remove(datosPDI$linkPD,"https://produccioncientifica.ucm.es/investigadores/")
datosPDI$Id_PDI <- str_remove(datosPDI$Id_PDI,"/detalle")

save(datosPDI,file="DatosPDI.RData")

#load(file="DatosPDI.RData")


# ..... seguiríamos


#-------------------------------------------------------------------------------
# 3 Lectura detalle Investigador: datosPDI_detalle 
# (sexo, categoría, departamento, facultad, Grupos, email, DOCTOR, WebScholar)
#-------------------------------------------------------------------------------
