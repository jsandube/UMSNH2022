
#' # Objetivo
#' Queremos estimar la ley de OKUN con datos Municipales,
#' en este caso para explicar la Tasa de Paro Municipal en función
#' de la renta percápita Municipal. Al tener datos georeferenciados
#' nos preguntaremos si en la explicación de la tasa de paro municipal
#' existen correlaciones espaciales con los municipio vecinos, y en tal caso
#' presentaremos dos modelos para intentar estimar dichos efectos espaciales

# Carga de Librerías necesarias ----
library(rgdal)

# library(maptools) # leer los shapes

library(sp) # paquete de referencia de datos espaciales
library(spdep) # econometría espacial
library(spatialreg) # para otros métodos de regresión espacial


library(tmap)
library(tidyverse)


library(stargazer) # tablas de resultados de moedlos de regresi?n


# Leo los Shapes ----
#' # Carga de Cartografías de los Municipios Españoles
MUNIC_ESP<-rgdal::readOGR(dsn="cartografias",layer="Munic04_ESP", stringsAsFactors = FALSE)

# primer mapa municipal
plot(MUNIC_ESP)

# El SpatialPoligonDataFrame que hemos cargado incorpora una base con datos Municipales
# Podemos consultar por ejemplo los municipios mñas ricos en la base de datos

head(MUNIC_ESP@data%>%
       arrange(desc(RENTPCAP07))%>%
       select(MUN,RENTPCAP07), 10)


# Mapas por cuantiles ----
#' # Mapas Descriptivos de la renta percápita y la tasa de paro

# Renta percápita
tm_shape(MUNIC_ESP) +
        tm_fill(palette ="YlOrBr",col = "RENTPCAP07",style = "quantile")+
        tmap_options(check.and.fix = TRUE)+
        tm_layout(frame = FALSE,
                  legend.title.size = .7,
                  legend.text.size =0.6,
                  legend.position = c("right","bottom"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1,
                  legend.stack = "horizontal",
                  legend.width = 1.5,
                  legend.height = 1.5)


#Mapa de la Tasa de Paro
tm_shape(MUNIC_ESP) +
        tm_fill(palette ="PuBuGn",col = "TASA_PARO",style = "quantile")+
        tmap_options(check.and.fix = TRUE)+
        tm_layout(frame = FALSE,
                  legend.title.size = .7,
                  legend.text.size =0.6,
                  legend.position = c("right","bottom"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1,
                  legend.stack = "horizontal",
                  legend.width = 1.5,
                  legend.height = 1.5)



# Observando esos dos gráficos, ¿podría decirse que los municipios con mayor nivel de Renta son los que tienen menores tasas de paro?


##################################################
# Creo la matriz de pesos----
##################################################
#' # La matriz de pesos espaciales



#### Por contiguidad de la Reina
list.queen<-poly2nb(MUNIC_ESP, queen=TRUE)

#se puede grabar como un fichero de pesos .GAL (se puede editar como texto)
# write.nb.gal(list.queen, "list_queen.GAL")
# o se puede editar directamente la lista 
# Editar el archivo GAL con block de notas
# leer una matriz de pesos GAL de contigüidad espacial existente
# list.queen <- read.gal("list_queen.GAL", region.id=NULL, override.id=FALSE)



# puedo editar la lista  (OJO, MANTENER EL ORDEN DE MENOR A MAYOR SI NO DEJA DE FUNCIONAR)
# list.queen[[7]] <- c(8L,12L, 43L, 46L)   # La "L" es para indicar que es un entero (que es el tipo que requiere la matriz de pesos)
# list.queen[[35]] <- 38L
# list.queen[[38]] <- 35L



#matriz de pesos
# Esquema de codificación
#B: codificación binaria básica
#w: filas estandarizadas # esto para construir el operador retardo espacial 
#c: estandarización global
#U: igual a C dividida por el número de vecinos
#S: esquema de codificación propuesto para estabilización de la varianza

W<-nb2listw(list.queen, style="W", zero.policy=TRUE)


# mapa de contigüidades
par(mar=c(0,0,0,0))
plot(MUNIC_ESP, border="grey",xlim=c(-0.5,0.5), ylim=c(37,40))
plot(W,coordinates(MUNIC_ESP), add=TRUE)
par(mar=c(5.1, 4.1, 4.1, 2.1))


# y ahora obtengo WM en forma de matriz para calcular retardos

WM<-listw2mat(W)
dim(WM)
rowSums(WM) # ya está estandarizada por filas

###################################################
#creo el retardo espacial de la tasa de paro ----
#' # Retardos Espaciales

MUNIC_ESP@data$TASA_PAROW<-(WM%*%MUNIC_ESP@data$TASA_PARO)[,1]

# El retardo espacial también se puede calcular directamente

MUNIC_ESP@data$TASA_PAROW<-spdep::lag.listw(var=MUNIC_ESP@data$TASA_PARO,x=W)



tm_shape(MUNIC_ESP) +
        tm_fill(palette =c("PuBuGn"),col = c("TASA_PARO","TASA_PAROW"),style = "quantile")+
        tm_facets(sync = TRUE, ncol = 2)+
        tmap_options(check.and.fix = TRUE)+
        tm_layout(frame = FALSE,
                  legend.title.size = .7,
                  legend.text.size =0.5,
                  legend.position = c("right","bottom"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1,
                  legend.stack = "horizontal",
                  legend.width = 1,
                  legend.height = 1)




# correlación espacial----
#' # Correlación entre una variable y su retardo espacial
 
plot(MUNIC_ESP$TASA_PAROW~MUNIC_ESP$TASA_PARO)

# Coeficiente de Correlación
cor.test(MUNIC_ESP$TASA_PAROW,MUNIC_ESP$TASA_PARO) # No es válido con objetos espaciales


# Coeficiente de Correlación espacial: La I de Moran **Global**
moran.test(MUNIC_ESP$TASA_PARO,W,zero.policy = TRUE)

par(mar=c(5.1, 4.1, 4.1, 2.1))
moran.plot(MUNIC_ESP$TASA_PARO,W,zero.policy = TRUE)



# Estimación de la desviación típica de la I de Moran por Montecarlo
moran.mc(MUNIC_ESP$TASA_PARO,W,zero.policy = TRUE,nsim=1000) #Estimaci?n de la I de moran con simulaci?n

# Correlación Espacial **Local**

lmoran<-localmoran(MUNIC_ESP$TASA_PARO,W,zero.policy = TRUE)  # I de moran local (para cada poligono)
summary(lmoran) # la quinta columna proporciona el pValor 

# ojo aquí el valor de la I indica si la autocorrelación es positiva o negativa, por eso para hacer 




# correlación tasa de paro con Rentapercápita----

cor.test(MUNIC_ESP$TASA_PARO,MUNIC_ESP$RENTPCAP07)
plot(MUNIC_ESP$TASA_PARO~MUNIC_ESP$RENTPCAP07)


# modelo regresion sin efectos espaciales
modelo.lm<-lm(TASA_PARO~RENTPCAP07, MUNIC_ESP@data)
summary(modelo.lm)


# Guardo los residuos de cada Municipio
MUNIC_ESP@data$lm.res<-resid(modelo.lm)


tm_shape(MUNIC_ESP) +
        tm_fill(palette ="PuBuGn",col = "lm.res",style = "quantile",n=5)+
        tmap_options(check.and.fix = TRUE)+
        tm_layout(frame = FALSE,
                  legend.title.size = .7,
                  legend.text.size =0.6,
                  legend.position = c("right","bottom"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1,
                  legend.stack = "horizontal",
                  legend.width = 1.5,
                  legend.height = 1.5)




# Si los residuos fuesen un ruido blanco deberían distribuirse de anera uniforma en todo el teritorio
# no debería observarse relación espacial



# Indice de Moran sobre los residuos
moran.lm<-lm.morantest(modelo.lm, W, alternative="two.sided", zero.policy=TRUE)
print(moran.lm)



####################################################
#' ## Modelos de ecnometría Espacial
formula_lm<-formula(modelo.lm) # copio el modelo de la regresión lineal para añadirle la dependencia espacial


## SAR: Spatial lag Model ---------------

# por Mínimos Cuadrados en dos etapas

sar.tslm<-stsls(formula_lm, data = MUNIC_ESP@data, W, zero.policy = TRUE,
             na.action = na.fail, robust = TRUE, HC="HC1", legacy=FALSE, W2X = TRUE)
summary(sar.tslm)


# residuos del Spatial lag model
sar.res<-resid(sar.tslm)
names(sar.res)


# residuos del modelo

MUNIC_ESP@data$sar.res<-resid(sar.tslm) #residual Para poder dibujar los residuos


tm_shape(MUNIC_ESP) +
        tm_fill(palette ="PuBuGn",col = "sar.res",style = "quantile",n=5)+
        tmap_options(check.and.fix = TRUE)+
        tm_layout(frame = FALSE,
                  legend.title.size = .7,
                  legend.text.size =0.6,
                  legend.position = c("right","bottom"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1,
                  legend.stack = "horizontal",
                  legend.width = 1.5,
                  legend.height = 1.5)



# Indice de Moran sobre los residuos

moran.test(resid(sar.tslm), W, alternative="greater", zero.policy=TRUE)
moran.plot(resid(sar.tslm),W,zero.policy = TRUE)


# Impactos (OJO QUE TARDA MUCHO)
# impacts(sar.lm, listw=W,zero.policy = TRUE )
# impacts(sar.tslm, empirical = TRUE,listw=W,zero.policy = TRUE )

