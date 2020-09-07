rm(list = ls()); gc(T)
#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonsf)
library(mapview)

## Caracteristicas disponibles
available_features()

## Etiquetas dentro de una caracteristica
available_tags("amenity")

## ------------------------------------------------------ ##
## ---- Ejercicio 1 - bancos poligono parkway Bogotá ---- ##
## ------------------------------------------------------ ##

## Defino una caja
my_box <- c(-74.075607061386, 4.6304414673187, -74.072549343109, 4.6332058140013)
bank_pol <- opq(bbox = my_box, timeout = 25*100) %>% add_osm_feature(key = "amenity", "bank") %>% osmdata_sf()

## Gráfico los bancos mapeados como nodos - puntos
mapview(subset(bank_pol$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"), map.types = "OpenStreetMap")

## Gráfico los bancos mapeados como vías - poligonos
mapview(bank_pol$osm_polygons[,c("osm_id","name","amenity","atm","operator")], map.types = "OpenStreetMap")

## Uno nodos y vías
res1 <- subset(bank_pol$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank")
res2 <- mutate(bank_pol$osm_polygons[,c("osm_id","name","amenity","atm","operator")], name = ifelse(is.na(name), operator, name)) %>% 
  select(-"operator") %>% mutate(geometry = st_centroid(geometry), id = osm_id) %>% remove_rownames %>% column_to_rownames(var = "id")

## Gráfico la totalidad de los bancos
mapview(bind_rows(res1, res2))

## ------------------------------------------------------ ##
## ---- Ejercicio 2 - Total de bancos en Bogotá      ---- ##
## ------------------------------------------------------ ##

## Extraigo la caja de Bogota
bank_bog <- opq(bbox = getbb(place_name = "Bogota", display_name_contains = "Colombia"), timeout = 25*100) %>% 
  add_osm_feature(key = "amenity", "bank") %>% 
  osmdata_sf()

## Gráfico los bancos mapeados como nodos - puntos
mapview(subset(bank_bog$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"))

## Gráfico los bancos mapeados como vías - poligonos
mapview(bank_bog$osm_polygons[,c("osm_id","name","amenity","atm","operator")])

## Gráfico los bancos mapeados como vías - poligonos
res_bank <- bind_rows(subset(bank_bog$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"),
                      mutate(bank_bog$osm_polygons[,c("osm_id","name","amenity","atm","operator")], name = ifelse(is.na(name), operator, name)) %>% 
                        select(-"operator") %>% mutate(geometry = st_centroid(geometry), id = osm_id) %>% remove_rownames %>% column_to_rownames(var = "id"))
mapview(res_bank)

as.tibble(res_bank) %>% group_by(name) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% as.data.frame()

subset(res_bank,name == "Servintrega")

## ------------------------------------------------------ ##
## ---- Ejercicio 3 - Gráfico la cartografía de Bogotá -- ##
## ------------------------------------------------------ ##

bog <- opq(bbox = "Bogota") %>% add_osm_feature(key = "boundary", "administrative") %>% osmdata_sf()
mapview(bog$osm_lines)


## ------------------------------------------------------ ##
## ---- Ejercicio 4 - Query algunas calles de Bogotá ---- ##
## ------------------------------------------------------ ##

callesMan <- opq(bbox = "Bogota") %>% 
  add_osm_feature(key = "highway", value = c("residential", "living_street","unclassified","service", "footway")) %>% osmdata_sf()
mapview(callesMan$osm_lines)
