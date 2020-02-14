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

world_infectados$value
ggplot(data = world)+
  geom_sf(color="white", fill= case_when(world$admin == "Australia" ~"#870000", TRUE ~"#999999"))
