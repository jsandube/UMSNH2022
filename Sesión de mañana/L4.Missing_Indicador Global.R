# Mapas WordEconomicForum  +  Measures30  + UNDP + OECD 

library(tidyverse)


load("datos_WEF_M30.Rdata")
load("datos_OECD.Rdata")
load("datos_UNDP.Rdata")

names(datos_OECD)[3] <- "SIGI_OECD"
names(datos_UNDP)[3] <- "GII_UNDP"


# fusiono en una única tabla
aa <- full_join(datos_WEF_M30,datos_OECD,by=c("iso_a3","name_long"))

table(is.na(aa$GGGI_WEF))
table(is.na(aa$SDGGI_EM30))
table(is.na(aa$SIGI_OECD))

bb <- full_join(aa,datos_UNDP,by=c("iso_a3","name_long"))

table(is.na(bb$GII_UNDP))
table(is.na(bb$GGGI_WEF))
table(is.na(bb$SDGGI_EM30))
table(is.na(bb$SIGI_OECD))


#Rellenar missing
library(mice)
tempnomiss = mice(bb[3:6])
nomiss = complete(tempnomiss, 1)

Indicadores <- cbind(bb[,1:2],nomiss)

table(is.na(Indicadores$GII_UNDP))
table(is.na(Indicadores$GGGI_WEF))
table(is.na(Indicadores$SDGGI_EM30))
table(is.na(Indicadores$SIGI_OECD))


# Construyo el indicador Global

Indicadores$GenderIndex<-rowMeans(Indicadores[,3:6])



# Ahora el mapa

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

mapa <- ne_countries(type = 'countries', scale = 'medium')

mapa_sf <- st_as_sf(mapa)


ggplot(mapa_sf) +
  geom_sf(fill = "#DFDFDF", color = "#656565")


#quito del mapa el polo sur "ATA" antartico

mapa_sf <- mapa_sf %>% 
  filter(adm0_a3!="ATA")

ggplot(mapa_sf) +
  geom_sf(fill = "#DFDFDF", color = "#656565")


# Añado los datos al MAPA

mapa_sf <- mapa_sf %>% 
  left_join(Indicadores, by=c("name_long","iso_a3"))




library(gtools)  # quantcut() para obtener puntos de corte mirar cut_number, cut_interval o cut(x, breaks)
library(RColorBrewer)


summary(mapa_sf$GenderIndex)


ggplot(data = mapa_sf) +
  geom_sf(aes(fill=GenderIndex),na.rm = TRUE, colour=NA)+      # mirar cut_number, cut_interval o cut(x, breaks)
  scale_fill_gradientn("Gender GAP",colours=brewer.pal(11,"RdYlGn"),na.value="white")+
  guides(fill = guide_colorbar(title.position='top', title.hjust=0.5,
                               barwidth = unit(20,'lines'),
                               barheight = unit(0.5,'lines')))+
  labs(title = "Gender GAP Index",
       subtitle= NULL,
       caption= "Note: Mean Gender GAP Index from World Economic Forum-Equal Measures 2030-OECD-UNPD \n expresed as Female/Male Ratio, with a range from 0 (for very high discrimiantion) to 100  (for gender equality)")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 12, hjust=0.5, face="bold", color="black"),
    plot.subtitle =  element_text(size = 10, hjust=0.5, color="black"),
    plot.caption = element_text(size = 8, hjust=1, face="italic", color="black"),
    legend.position="bottom",
    line = element_blank(),                          # remove axis lines .. 
    axis.text=element_blank(),                       # .. tickmarks..
    axis.title=element_blank(),                      # .. axis labels..
    panel.background = element_blank())            # .. background gridlines  

ggsave(filename = "Gender-GAP Index.svg",device = "svg", units = "cm",width = 15,height = 10)
ggsave(filename = "Gender-GAP Index.jpeg",device = "jpeg",bg = "white", dpi = 600, units = "cm",width = 15,height = 10)




# Guardo La información


save(Indicadores, file = "Indicador_Global.Rdata")
write.csv(Indicadores,file = "Indicador_Global.csv")
                                       