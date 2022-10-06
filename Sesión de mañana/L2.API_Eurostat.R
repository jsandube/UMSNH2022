#' abstract: |
#'  En esta práctica se muestra cómo utilizar la librería `eurostat`
#'  para descargar datos directamente de eurostat, pintar mapas, y gráficos.
#'  Se utilizarán datos de la unión europea para analizar qué países
#'  están haciéndolo relativamente mejor en términos de creación de empleo  

#' # Librerías necesarias

library(eurostat)
library(dplyr)
library(ggplot2)
library(stargazer)
library(sf)
library(mapproj)

#' # Búsqueda de datos en las tablas de eurostat

# Busco datos de PIB en el conjunto de tablas de Eurostat
query<-search_eurostat("GDP", type ="dataset")

query[1:10,1:2]

#' revisando las posibles tablas encontramos una titulada "GDP and main components (output, expenditure and income)" 
#' utilizaremos entonces esta tabla que se llama "nama_10_gdp"
dat.pib<-get_eurostat("nama_10_gdp",time_format = "num",type ="code", stringsAsFactors = TRUE) #,time_format = "raw",type ="code", stringsAsFactors = TRUE)


names(dat.pib)
#Para ver que significan los campos
cc<-label_eurostat_vars(dat.pib)
print(cc)


#' # Representación de la importancia de cada país en el conjunto de la EU

table(dat.pib$time)

dat.pib<-dat.pib%>%
  filter(time==2018 |time==2019)

# añadir los labels
dat.pib<-label_eurostat(dat.pib, code=names(dat.pib))

#View(dat.pib)

table(dat.pib$na_item)

dat.pib<-dat.pib%>%
  filter(na_item=="Gross domestic product at market prices")

table(dat.pib$unit)

# Voy a hacer un mapa y un gráfico para analizar el peso de cada país
# La variable peso de cada país se llama "PC_EU28_MPPS_CP"

dat.pib%>%
  distinct(unit_code)


dat.pib_mapa<-dat.pib%>%
  filter(unit_code=="CLV15_MEUR")%>%
  filter(time==2019)


g1<-ggplot() +
  geom_bar(data=dat.pib_mapa, aes(x=reorder(geo_code, values),y=values) ,fill = "#D8B70A",stat = "identity")+
  ylab("%")+
  xlab("Países")+
  labs( title="Peso en el PIB total de la Unión Europea (%)",
        subtitle="pps",
        caption = paste("Fuente: Eurostat", sep  = "\n"))+
  theme_minimal()+
  theme(legend.position = "right",
        plot.title= element_text(colour="black", face="bold"),
        axis.line = element_line(size = 0.25, colour = "darkgrey", linetype = 1),
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))

print(g1)

library(plotly)
ggplotly(g1)

