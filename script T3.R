apex(data = datos,
     type = "line",
     mapping = aes(x = fecha, 
                   y = infectados, fill = pais))%>% 
  ax_title(text = "Resultados de la encuesta",
           style = list(fontSize = 20),
           align = "center")


ax <- apex(data = datos,
     type = "line",
     mapping = aes(x = fecha, 
                   y = infectados, fill = pais)) %>% 
  ax_title(text = "Cantidad de personas infectadas por coronavirus ",
           style = list(fontSize = 20),
           align = "center") %>%
  ax_labs(x = "Fecha de la observación", 
          y = "Cantidad de casos confirmados")



##############################################################################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggforce)
library(ggplot2)
world <- ne_countries(scale = "small", returnclass = "sf")
datos <- read.csv("2019-ncov-total.csv", header = T)


world_infectados <- left_join(x = world,
                              y = datos,
                              by = c("admin" = "region"))


ggplot(data = world)+
  geom_sf(color="white", fill= case_when(world_infectados$value >= 1 ~"#d90000", TRUE ~"#a6a6a6"))+
  theme_classic()+
  geom_mark_circle(data=datos,
                   mapping=aes(x=datos$Long,
                               y=datos$Lat,
                               group=datos$region,
                               label=datos$region),
                   expand = unit(2,"mm"),
                   label.fontsize = 6)



ggplot(data = world)+
  geom_sf(color="white", fill= case_when(world_infectados$value >= 1 ~"#d90000", TRUE ~"#a6a6a6"))+
  theme_classic()+
  geom_mark_circle(mapping = aes(x = world_infectados$Long, 
                                 y = world_infectados$Lat, 
                                 label = world_infectados$admin, 
                                 group = world_infectados$admin), 
                   label.fontsize = 12)


######################################################################################

## 3. Utilizando el conjunto de datos "2019-ncov-totals-coords.csv" y 
#el paquete leaflet genere un mapa interactivo que cumpla con los siguientes requisitos:

## a) Se debe colocar un marcador en cada uno de los paises que han reportado casos de 2019-nCoV.

## b) Al dar clic sobre un marcador se debe desplegar un mensaje que nos indique la ciudad y 
#la cantidad de casos reportados.

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggforce)
library(ggplot2)
library(leaflet)
library(htmltools)
library(stringr)
library(scales)
library(purrr)


world <- ne_countries(scale = "small", returnclass = "sf")
datos <- read.csv("2019-ncov-totals-coords.csv", header = T)


world_infectados_interactivo <- right_join(x = world,y = datos, by = c("admin" = "region"))

pal <- colorBin("Reds", domain = world_infectados_interactivo$pop_est)

etiquetas <- map(transpose(world_infectados_interactivo), ~{
  HTML(
    str_c("<strong> País: ", .$admin, "</strong>",
          "<br/>",
          "Cantidad de infectados: ", comma(.$value))
  )
})


leaflet(world_infectados_interactivo) %>%
  addTiles() %>%
  addPolygons(label = ~etiquetas,
              fillColor = ~pal(pop_est),
              fillOpacity = .9,
              weight = 2.0,
              color = "black",
              highlightOptions = highlightOptions(color = "#ff935c",
                                                  weight = 3,
                                                  bringToFront = TRUE,
                                                  opacity = 1))


