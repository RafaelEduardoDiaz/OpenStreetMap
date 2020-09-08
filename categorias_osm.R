rm(list = ls()); gc(T)
## Cargo los paquetes
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonsf)
library(mapview)
library(data.table)

## Categorias
amenity_cat <- c("atm", "bank", "bar","bicycle_parking","bus_station", "cafe", "clinic", "college", "fast_food", 
             "fuel", "hospital","ice_cream", "marketplace","motorcycle_parking", "Paga Todo", "parking", "payment_centre", 
             "payment_terminal", "pharmacy", "police", "post_office", "restaurant","school","university", "veterinary")

shop_cat <- c("alcohol", "baby_goods", "bakery", "butcher", "convenience","dairy", "de_barrio", "department_store", "food", "funeral_directors","general", 
          "greengrocer", "grocery", "ice_cream", "kiosk", "mall", "medical_supply","pet","sports", "supermarket")

leisure_cat <- c('fitness_centre','sports_centre')

building_cat <- c("apartments","bakehouse", "chapel", "church", "college", 
              "commercial", "hospital","hotel", "house", "industrial", "kiosk", "office", 
              "Oficina Central", "public", "residential", "retail","supermarket", "university","warehouse")

tourism_cat <- 'hotel'

highway_cat <- c("bus_stop","primary")

## Consulta de todas las cetogrias
query1 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "amenity",value = amenity_cat) %>% osmdata_sf()
query2 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "shop",value = shop_cat) %>% osmdata_sf()
query3 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "leisure",value = leisure_cat) %>% osmdata_sf()
query4 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "building",value = building_cat) %>% osmdata_sf()
query5 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "tourism",value = tourism_cat) %>% osmdata_sf()
query6 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "highway",value = "bus_stop") %>% osmdata_sf()

query_cate <- c(query1, query2, query3, query4, query5, query6)

## resultado poligonos
res_pol <- bind_rows(
  query_cate$osm_polygons[,c("osm_id","name","amenity")] %>% mutate(features = amenity, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","shop")] %>% mutate(features = shop, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","leisure")] %>% mutate(features = leisure, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","building")] %>% mutate(features = building, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","tourism")] %>% mutate(features = tourism, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","highway")] %>% mutate(features = highway, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"))

nrow(res_pol);uniqueN(res_pol$osm_id)
setDT(res_pol)
res_pol$features <- ifelse(res_pol$features == "", NA, res_pol$features)
res_pol <- st_sf(distinct(res_pol)[order(osm_id, features)][,head(.SD,1), by = osm_id]) #24161
colSums(is.na(res_pol))

## resultado multipoligonos
res_mul_pol <- bind_rows(
  query_cate$osm_multipolygons[,c("osm_id","name","amenity")] %>% mutate(features = amenity, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","shop")] %>% mutate(features = shop, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","building")] %>% mutate(features = building, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","highway")] %>% mutate(features = highway, geometry = st_centroid(geometry)) %>% select("osm_id", "name", "features"))

nrow(res_mul_pol);uniqueN(res_mul_pol$osm_id)
setDT(res_mul_pol)
res_mul_pol$features <- ifelse(res_mul_pol$features == "", NA, res_mul_pol$features)
res_mul_pol <- st_sf(na.omit(distinct(res_mul_pol))[order(osm_id, features)][,head(.SD,1), by = osm_id]) #190
colSums(is.na(res_mul_pol))
mapview(res_mul_pol)

## resultado puntos
res_points <- bind_rows(subset(query_cate$osm_points[,c("osm_id", "name", "amenity")], amenity %in% amenity_cat) %>% mutate(features = amenity) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "shop")], shop %in% shop_cat) %>% mutate(features = shop) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "leisure")], leisure %in% leisure_cat)%>% mutate(features = leisure) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "building")], building %in% building_cat)%>% mutate(features = building) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "tourism")], tourism %in% tourism_cat)%>% mutate(features = tourism) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "highway")], highway %in% "bus_stop")%>% mutate(features = highway) %>% select("osm_id", "name", "features"))

nrow(res_points);uniqueN(res_points$osm_id)
setDT(res_points)
res_points <- st_sf(distinct(res_points)[order(osm_id, features)][,head(.SD,1), by = osm_id]) 
colSums(is.na(res_points))

## Uno poligonos, con multipoligonos y puntos
res_query <- bind_rows(res_pol, res_mul_pol, res_points) #38820
res_query <- subset(res_query, !is.na(features)) #38402
colSums(is.na(res_query))

mapview(res_query)

## Gurado la base
saveRDS(object = as.data.table(res_query), file = "osmdata_base.RDS")
