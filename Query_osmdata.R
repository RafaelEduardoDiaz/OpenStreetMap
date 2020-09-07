rm(list = ls()); gc(T)
#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonsf)
library(mapview)

#
available_features()

#amenities
available_tags("amenity")


#my_box <- c(4.6304414673187,-74.075607061386,4.6332058140013,-74.072549343109);
my_box <- c(-74.075607061386, 4.6304414673187, -74.072549343109, 4.6332058140013)
bank_pol <- opq(bbox = my_box, timeout = 25*100) %>% 
  add_osm_feature(key = "amenity", "bank") %>% 
  osmdata_sf()


mapview(subset(bank_pol$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"), map.types = "OpenStreetMap")
mapview(bank_pol$osm_polygons[,c("osm_id","name","amenity","atm","operator")], map.types = "OpenStreetMap")

res1 <- subset(bank_pol$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank")
res2 <- mutate(bank_pol$osm_polygons[,c("osm_id","name","amenity","atm","operator")], name = ifelse(is.na(name), operator, name)) %>% 
  select(-"operator") %>% mutate(geometry = st_centroid(geometry), id = osm_id) %>% remove_rownames %>% column_to_rownames(var = "id")

mapview(bind_rows(res1, res2))

bank_pol

bank_bog <- opq(bbox = getbb(place_name = "Bogota", display_name_contains = "Colombia"), timeout = 25*100) %>% 
  add_osm_feature(key = "amenity", "bank") %>% 
  osmdata_sf()

mapview(bank_bog$osm_polygons[,c("osm_id","name","amenity","atm","operator")])
mapview(subset(bank_bog$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"))

res_bank <- bind_rows(subset(bank_bog$osm_points[,c("osm_id","name","amenity","atm")], amenity == "bank"),
                      mutate(bank_bog$osm_polygons[,c("osm_id","name","amenity","atm","operator")], name = ifelse(is.na(name), operator, name)) %>% 
                        select(-"operator") %>% mutate(geometry = st_centroid(geometry), id = osm_id) %>% remove_rownames %>% column_to_rownames(var = "id"))
mapview(res_bank)

as.tibble(res_bank) %>% group_by(name) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% as.data.frame()

subset(res_bank,name == "Servintrega")

##################
available_features()
available_tags("boundary")


bog <- opq(bbox = c(-74.192733764648,4.5120831618535,-74.009399414062,4.778994921541), timeout = 25*100) %>% 
  add_osm_feature(key = "boundary", "administrative") %>% 
  osmdata_sf()

mapview(bog$osm_lines)


callesMan <- opq(bbox = c(-74.192733764648,4.5120831618535,-74.009399414062,4.778994921541)) %>% 
  add_osm_feature(key = "highway", value = c("residential", "living_street","unclassified","service", "footway")) %>% osmdata_sf()
mapview(callesMan$osm_lines)
