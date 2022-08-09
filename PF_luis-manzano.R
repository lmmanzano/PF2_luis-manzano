# install.packages("openxlsx")
library(openxlsx)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

dir <- getwd()
y <- file.path(dir, "R Files/Clase Data science")


road <- read_excel("Road_Injuries_America.xls")

#Limpieza
#_________________________________________________________________
title <- road[1,]

colnames(road) <- title
length(road$Country)

road <- road[2:562,-(7:10)]

colnames(road)[colnames(road) == "ISOAlpha3"] <- "ABB"

abb <- road$ABB
abb <- abb[!is.na(abb)]
abb <- rep(abb, each = 17)

ctry <- road$Country
ctry <- ctry[!is.na(ctry)]
ctry <- rep(ctry, each = 17)

sr <- road$`Subregion name`
sr <- sr[!is.na(sr)]
sr <- rep(sr, each = 17)

cs <- road$Cause
cs <- cs[!is.na(cs)]
cs <- rep(cs, len=561)


road <- road %>% mutate(ABB = abb) %>%
  mutate(Country = ctry) %>%
  mutate(`Subregion name` = sr) %>%
  mutate(Cause = cs)

head(road)
#___________________________________________________________________________________
#Arrange 


colnames(road)

road$Year <- as.integer(road$Year)
road$Male <- as.integer(road$Male)
road$Female <- as.integer(road$Female)

regions <- as.factor(road$`Subregion name`)
road$`Subregion name` <- regions
#___________________________________________________________________________________
#Cambiamos los nombres de los paises y filtramos por los que nos interesan

road[545:561,2] <- rep("Venezuela",each=17)
road[511:527,2] <- rep("USA",each=17)
road[69:85,2] <- rep("Bolivia",each=17)

road <- road %>% 
  filter(Country %in% c("Argentina" ,"Bahamas" ,"Belize","Bolivia","Brazil",
                        "Barbados","Canada","Chile","Colombia","Costa Rica",
                        "Cuba" ,"Dominican Republic","Ecuador","Grenada",
                        "Guatemala","Guyana","Honduras","Haiti","Jamaica",
                        "Saint Lucia","Mexico","Nicaragua","Panama","Peru","Paraguay",
                        "El Salvador","Suriname","Uruguay","USA","Venezuela"))

cnames <- c("Argentina" ,"Bahamas" ,"Belize","Bolivia","Brazil",
            "Barbados","Canada","Chile","Colombia","Costa Rica",
            "Cuba" ,"Dominican Republic","Ecuador","Grenada",
            "Guatemala","Guyana","Honduras","Haiti","Jamaica",
            "Saint Lucia","Mexico","Nicaragua","Panama","Peru","Paraguay",
            "El Salvador","Suriname","Uruguay","USA","Venezuela")

#___________________________________________________________________



#___________________________________________________________________



#___________________________________________________________________
#Se crea las tablas que extras que vamos a usar para los graficos

road1 <- road

#Le cambiaremos el nombre de la columna Country a region para asi poder unirla con
#una futura tabla para un grafico del mapa

colnames(road1)[colnames(road1) == "Country"] <- "region"

#Crearemos una tabla con las medias anuales por region

road2 <- road %>% group_by(`Subregion name` , Year) %>%
  summarise(media_anual_hombres = mean(Male) , 
            media_anual_mujeres = mean(Female))

#Grafico Mundial

#Debemos crear una nueva tabla con las coordenadas de paises para hacer este grafico

map <- map_data("world")

mapdata <- left_join(map,road1,by = "region")

mapdata <- mapdata %>% filter(!is.na(mapdata$Male))

mapdata1 <- mapdata %>% filter(long <= 0)

mapdata2 <- mapdata1 %>% filter(Year == 2016)

#___________________________________________________________________
#Saving Data Frames_________________________________________________

write.csv(road, file = "road.csv", row.names = FALSE)
write.csv(map, file = "map.csv", row.names = FALSE)
write.csv(road1, file = "road1.csv", row.names = FALSE)
write.csv(road2, file = "road2.csv", row.names = FALSE)
write.csv(mapdata2, file = "mapdata2.csv", row.names = FALSE)

save(road, file = "road.RData")
save(road1, file = "road1.RData")
save(road2, file = "road2.RData")
save(map, file = "map.RData")
save(mapdata2, file = "mapdata2.RData")

#a <- load("~/R Files/Clase Data science/road2.RData")

#road1 <- read.csv("~/R Files/Clase Data science/road1.csv")

#___________________________________________________________________
#A partir de aca crearemos la nueva tabla que incluya "the fragile state index"


#Leemos el archivo

fragile <- read.xlsx("FSI.xlsx", fillMergedCells = TRUE)

#Seleccionamos las columnas y filas que deseamos y cambiamos los nombres
#a gusto

fragile <- fragile[1:1973,1:20]

title2 <- fragile[1,]

colnames(fragile) <- title2

fragile <- fragile[2:1973,-(2:3)]

colnames(fragile)[colnames(fragile) == "Alternate"] <- "Country"

#Filtramos por la mismos paises de la tabla "road"

cnames <- c("Argentina" ,"Bahamas" ,"Belize","Bolivia","Brazil",
            "Barbados","Canada","Chile","Colombia","Costa Rica",
            "Cuba" ,"Dominican Republic","Ecuador","Grenada",
            "Guatemala","Guyana","Honduras","Haiti","Jamaica",
            "Saint Lucia","Mexico","Nicaragua","Panama","Peru","Paraguay",
            "El Salvador","Suriname","Uruguay","USA","Venezuela")

fragile <- fragile %>% filter(Country %in% cnames)

#Usamos la funcion pivot_longer para pasar las columnas a filas

fragile <- fragile %>% pivot_longer(cols=2:18, names_to = "Year", 
                                    values_to = "index",
                                    values_drop_na = TRUE)

#Organizamos la tabla del indice por año y pais

fragile <- fragile %>% arrange(Year) %>% arrange(Country)

#Convertimos la columna Year e index en integral

fragile$index <- as.integer(fragile$index)
fragile$Year <- as.integer(fragile$Year)

#Filtramos por los años que tinen valores ya que hay algunos paises que no 
#presentan valor en el año 2006. Por lo tanto es necesario filtrar desde el 
#2007 al 2016 en este caso ya que sino no podremos unir ambas tablas

fragile <- fragile %>% filter(Year %in% c(2007:2016))
road3 <- road1 %>% filter(Year %in% c(2007:2016)) %>% arrange(region)

#Creamos un vector con los valores de los paises para asi eliminar aquellos 
#paises que falten para que su union sea efectiva

cnames2 <- unique(fragile$Country)
cnames3 <- unique(road3$region)

#Filtramos

road3 <- road3 %>% filter(region %in% cnames2)

#Cambiamos el nombre de una columna para unirlos

colnames(road3)[colnames(road3) == "region"] <- "Country"

#Creamos la ultima tabla a utilizar

road_fragile <- left_join(road3,fragile)

#Creamos una nueva columna la cual nos indicara la variacion porcentual
#del FSI y los Ratios de Muerte de los hombres por accidente automovilistico
#Tomando como base el año 2007

road_fragile <- road_fragile %>%
  group_by(Country) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate("%Change_Index" = (index/first(index) - 1) * 100)

road_fragile <- road_fragile %>%
  group_by(Country) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate("%Change_male" = (Male/first(Male) - 1) * 100)

#___________________________________________________________________
#___________________________________________________________________
#Grafico mundial muerte hombres 

mapa_mundial_hombres<-ggplot(mapdata2, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Male), color = "black")


#Agregamos mayor personalizacion 

mapa_mundial_hombres <- mapa_mundial_hombres +
  scale_fill_gradient2(name = "Male Deaths Ratio",
                       low="yellow", high="red", mid = "grey", 
                       midpoint = 34)+
  theme_void()+
  labs(title = "Ratio de Muerte de Hombres por Accidente de Auto en el 2016")

mapa_mundial_hombres

#Grafico muerte Mujeres

mapa_mundial_mujeres <-ggplot(mapdata2, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Female), color = "black")


#Agregamos mayor personalizacion 

mapa_mundial_mujeres <- mapa_mundial_mujeres + 
  scale_fill_gradient2(name = "Female Deaths Ratio",
                       low="yellow", high="red", mid = "grey",
                       midpoint = 7) +
  theme_void()+
  labs(title = "Ratio de Muerte de Mujeres por Accidente de Auto en el 2016")

mapa_mundial_mujeres

#_________________________________________________________________________
#Graficos


#Extra
unique(road_fragile$`Subregion name`)
colnames(road_fragile)

#____________________________________________________________________
#Male plots (region)_________________________________________________

#Plot Media Anual Hombres

plot_MediaAnualH <- road2 %>% ggplot(aes(Year, media_anual_hombres,
                                         color = `Subregion name`,
                                         fill = `Subregion name`))+
  geom_col(position = "dodge" , width = 0.8) + 
  theme_minimal() +
  facet_wrap(. ~ `Subregion name`) +
  labs( y = "Media Muerte Hombres" , title = "Media Anual Muerte Hombres por Region")

plot_MediaAnualH

#Plot Media Anual Mujeres

plot_MediaAnualM <- road2 %>% ggplot(aes(Year, media_anual_mujeres,
                                         color = `Subregion name`,
                                         fill = `Subregion name`))+
  geom_col(position = "dodge" , width = 0.8) +
  theme_minimal() +
  facet_wrap(. ~ `Subregion name`) +
  labs( y = "Media Muerte Mujeres" , title = "Media Anual Muerte Mujeres por Region")

plot_MediaAnualM

#Plot Muertes Hombres Latin caribbean, Andean area, North America

plot_1 <- road %>% filter(Country %in% c("Venezuela","USA", 
                                         "Dominican Republic")) %>%
  ggplot(aes(Year, Male, color = Country, fill = Country)) +
  geom_col(position = "dodge" , width = 0.8) +
  theme(legend.position = "bottom") +
  facet_wrap(. ~ `Subregion name`) +
  theme_minimal() +
  labs( y = "Ratio Muerte Hombres" , title = "Ratio Muerte Hombres Por Pais y Region" )

plot_1

#Plot Muertes Hombres Central America

plot_HCA <- road %>% filter(`Subregion name` %in% c("Central America")) %>%
  filter(Country %in%  c("El Salvador", "Mexico", "Guatemala")) %>%
  ggplot(aes(Year, Male, color = Country , fill = Country)) +
  geom_point() +
  geom_line() +
  facet_wrap(. ~ `Subregion name`)  +
  theme_minimal() +
  labs( y = "Ratio Muerte Hombres" , title = "Ratio Muerte Hombres Por Pais y Region" )

plot_HCA

#Plot Muertes Hombres Non latin caribbean

plot_HNLC <- road %>% filter(Country %in% c("Barbados", "Suriname", 
                                            "Saint Lucia")) %>%
  ggplot(aes(Year, Male, color = Country)) +
  geom_point() +
  geom_line() +
  facet_wrap(. ~ `Subregion name`)  +
  theme_minimal() +
  labs( y = "Ratio Muerte Hombres" ,
        title = "Ratio Muerte Hombres Por Pais y Region" )

plot_HNLC

#Plot Muertes Hombres Southern Cone

plot_HSC <- road %>% filter(Country %in% c("Paraguay", "Chile",
                                           "Argentina")) %>%
  ggplot(aes(Year, Male, color = Country)) +
  geom_point() +
  geom_line() +
  facet_wrap(. ~ `Subregion name`)  +
  theme_minimal() +
  labs( y = "Ratio Muerte Hombres" , 
        title = "Ratio Muerte Hombres Por Pais y Region" ) 

plot_HSC

#Plots Unidos


plot_HCA + plot_HNLC +  plot_1 + plot_HSC



#Southern Cone Male Plot

plotISC <- road_fragile %>% filter(`Subregion name` %in% c("Southern Cone")) %>%
  ggplot()+
  geom_line(mapping = aes(Year, `%Change_Index`), col = "blue",lwd=1)+
  geom_line(mapping = aes(Year, `%Change_male`), col = "red",lwd=1) +
  facet_wrap(. ~ Country) +
  theme_minimal() +
  labs( y = "Variacion Porcentual" , 
        title = "Relacion FSI y Muertes. Region: Southern Cone" ,
        caption = "FSI = Blue | Male Death Ratio = Red")

plotISC

#Non Latin Caribbean Male Plot

plotINLC <- road_fragile %>% filter(`Subregion name` %in% c("Non latin caribbean")) %>%
  ggplot()+
  geom_line(mapping = aes(Year, `%Change_Index`), col = "blue",lwd=1)+
  geom_line(mapping = aes(Year, `%Change_male`), col = "red",lwd=1) +
  facet_wrap(. ~ Country) +
  theme_minimal() +
  labs( y = "Variacion Porcentual" , 
        title = "Relacion FSI y Muertes. Region: Non latin caribbean" ,
        caption = "FSI = Blue | Male Death Ratio = Red")

plotINLC

#Andean Area Male Plot

plotIAA <- road_fragile %>% filter(`Subregion name` %in% c("Andean area")) %>%
  ggplot()+
  geom_line(mapping = aes(Year, `%Change_Index`), col = "blue",lwd=1)+
  geom_line(mapping = aes(Year, `%Change_male`), col = "red",lwd=1) +
  facet_wrap(. ~ Country) +
  theme_minimal() +
  labs( y = "Variacion Porcentual" , 
        title = "Relacion FSI y Muertes. Region: Andean area" ,
        caption = "FSI = Blue | Male Death Ratio = Red")

plotIAA

#Central America  Male Plot

CA_names <- c("Belize","Mexico","El Salvador","Costa Rica",
              "Panama", "Nicaragua")

plotICA <- road_fragile %>% filter(`Subregion name` %in% c("Central America")) %>%
  filter(Country %in% CA_names) %>%
  ggplot()+
  geom_line(mapping = aes(Year, `%Change_Index`), col = "blue",lwd=1)+
  geom_line(mapping = aes(Year, `%Change_male`), col = "red", lwd=1) +
  facet_wrap(. ~ Country) +
  theme_minimal() +
  labs( y = "Variacion Porcentual" ,
        title = "Relacion FSI y Muertes. Region: Central America" ,
        caption = "FSI = Blue | Male Death Ratio = Red")

plotICA




 

