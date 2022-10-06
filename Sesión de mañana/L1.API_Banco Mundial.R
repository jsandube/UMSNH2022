#Lectura de datos del Banco Mundial
# Dos librerías permiten hacerlo
#library(wbstats)
#library(WDI)

library(tidyverse)

library(wbstats)

# 1 busco el dato que quiero
#     - Tasa de crecimiento de la Renta o renta percápita percápita
#     - Tasa de Paro



# Compruebos si hay un caché de todas las variables y países
str(wb_cachelist, max.level = 1)

# para actualizar el caché
# default language is english
new_cache <- wb_cache()
# ahora se puede buscar en esta lista actualizada
nuevo_cache <- wb_cache(lang="es")


# listado de indicadores

indicadores.en <- data.frame(new_cache$indicators, stringsAsFactors = FALSE)
indicadores.es <- data.frame(nuevo_cache$indicators, stringsAsFactors = FALSE)




# Realizo la búsqueda en el nuevo caché

wb_search("GDP") # busca palabras en el caché de datos disponibles


wb_search("gdp.*capita.*US\\$", cache = new_cache)
wb_search("gdp.*growth", cache = new_cache)
wb_search("life expectancy at birth.*total", cache = new_cache)
wb_search("^mortality.*rate.*infant", cache = new_cache)
View(wb_search("unemployment. *rate", cache = new_cache))
View(wb_search(".*pobla.*", cache = nuevo_cache))



# Bajo los datos


wb_dat <- wb_data(indicator = c("NY.GDP.MKTP.KD.ZG"))
names(wb_dat)

wb_dat<-wb_dat %>% 
  filter(date=="2019") 


# Para hacer un mapa
library(rworldmap)

worldMap <- getMap()
plot(worldMap)

sPDF <- joinCountryData2Map(wb_dat, joinCode = "ISO3", nameJoinColumn="iso3c")
mapCountryData(mapToPlot=sPDF, nameColumnToPlot = "NY.GDP.MKTP.KD.ZG", mapTitle=" Tasa de crecimiento del PIB")



# Esta es la tasa de cerecimiento. Si quisiéramos saber quíen es más rico y más pobre

wb_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD"))
wb_dat<-wb_dat %>% 
  filter(date=="2019") 
sPDF <- joinCountryData2Map(wb_dat, joinCode = "ISO3", nameJoinColumn="iso3c")

library(RColorBrewer)
mapCountryData(mapToPlot=sPDF,catMethod = "quantiles", colourPalette = brewer.pal("PuBuGn"), numCats=5, nameColumnToPlot = "NY.GDP.PCAP.KD", mapTitle="PIB percápita")



# Ahora quiero que bajéis las tasas de desempleo y la tasa de crecimiento percápita

wb_dat <- wb_data(indicator = c("NY.GDP.MKTP.KD.ZG", "SL.UEM.TOTL.ZS")) 

wb_dat<-wb_dat %>% 
  filter(date=="2019") 

wb_dat <- wb_dat[complete.cases(wb_dat),]

g4<-ggplot(data=wb_dat,aes(x=NY.GDP.MKTP.KD.ZG, y=SL.UEM.TOTL.ZS, label=country)) +
  geom_point()+
  geom_smooth(method='lm')+
  geom_text(check_overlap = TRUE, vjust = 0, nudge_y = 0.0005)+
  ylab("Tasa de Paro")+
  xlab("Tasa de Crecimiento del PIB")+
  labs( title="La Ley de Okun Mundial",
        subtitle=paste("Año 2019", sep  = "\n"),
        caption = paste("Fuente: Elaboración Propia a partir de datos del Banco Mundial", sep  = "\n"))+
  theme_minimal()+
  theme(legend.position = "right",
        plot.title= element_text(colour="black", face="bold"),
        axis.line = element_line(size = 0.25, colour = "darkgrey", linetype = 1),
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))


print(g4)


# Tarea ¿harías un gráfico pero por continentes?

# Pista.....
countries <- data.frame(new_cache$countries, stringsAsFactors = FALSE)




#library(WDI)
#new_wdi_cache <- WDIcache() 
#WDIsearch("gdp.*capita.*US\\$", cache = new_wdi_cache)
#WDIsearch("life expectancy at birth.*total", cache = new_wdi_cache)
#WDIsearch("^mortality.*rate.*infant", cache = new_wdi_cache)
#wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN"), start = 1960, end = 2015, extra = TRUE) 
#names(wdi_dat)





