library(tidyverse)
library(sf)
library(glue)
library(scales)
library(htmltools)
library(leaflet)


geo <- sf::st_read("btw21/Geometrie_Wahlkreise_20DBT_VG250_geo.shp", 
                      stringsAsFactors = FALSE) %>% 
  mutate(ID = as.numeric(WKR_NR)) %>% 
  inner_join(btw21, by = "ID") %>% 
  mutate(color = case_when(Partei == "Union" ~ "#000000",
                           Partei == "GRÜNE" ~ "#0E8C1D",
                           Partei == "SPD" ~ "#C00000",
                           Partei == "AfD" ~ "#005EA4",
                           Partei == "DIE LINKE" ~ "#CC0066",
                           Partei == "FDP" ~ "#FFC000"))

map <- sf::st_transform(geo, "+proj=longlat +datum=WGS84")


popup <- glue("<strong>{map$WK_Name} (WK {map$ID})</strong><br />
                    <strong>Gewinner: {map$Name} ({map$Partei})</strong><br />
                    Erststimmen: {scales::comma(map$Anteil*100, accuracy = 2)}%<br />
                    Abstand: {scales::comma(map$Diff*100, accuracy = 2)}%P")  %>%   
  lapply(htmltools::HTML)


map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = map,
    fillColor = map$color,
    label = popup,
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = map$Alpha,
    color = "#666",
    weight = 1
  )

map

htmlwidgets::saveWidget(map, "BTW_Map.html")

