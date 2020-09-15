rm(list = ls()); gc(T)
## Cargo los paquetes
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonsf)
library(mapview)
library(data.table)
library(geojsonio)

## Categorias
amenity_cat <- c("atm", "bank", "bar","bicycle_parking","bus_station", "cafe", "clinic", "college", "fast_food", 
                 "fuel","grave_yard", "hospital","ice_cream", "marketplace","motorcycle_parking", "parking", "payment_centre", 
                 "payment_terminal", "pharmacy", "place_of_worship", "police", "post_office", "restaurant","school","university", "veterinary")

shop_cat <- c("alcohol", "baby_goods", "bakery", "butcher", "convenience","dairy","department_store",	"doityourself", "food", "funeral_directors",
              "general", "greengrocer", "grocery", "ice_cream", "kiosk", "mall", "medical_supply","pet","sports", "supermarket")

leisure_cat <- c('fitness_centre','sports_centre')

building_cat <- c("apartments","bakehouse", "chapel", "church", "college","commercial", "hospital","hotel", "house", "industrial", 
                  "kiosk", "office","Oficina Central", "public", "residential", "retail","supermarket", "university","warehouse","yes")

tourism_cat <- c('hostel','hotel','motel')

highway_cat <- c("primary","secondary","trunk")

landuse_cat <- c("cemetery", "military", "residential")

office_cat <- c("company","financial","insurance","notary")

all_features <- sort(unique(c(amenity_cat, shop_cat, landuse_cat, leisure_cat, building_cat, tourism_cat, highway_cat, "bus_stop")))

## Consulta de todas las cetogrias
query1 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "amenity",value = amenity_cat) %>% osmdata_sf()
query2 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "shop",value = shop_cat) %>% osmdata_sf()
query3 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "leisure",value = leisure_cat) %>% osmdata_sf()
query4 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "building",value = building_cat) %>% osmdata_sf()
query5 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "tourism",value = tourism_cat) %>% osmdata_sf()
query6 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "highway",value = "bus_stop") %>% osmdata_sf()
query7 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "highway",value = highway_cat) %>% osmdata_sf()
query8 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "landuse",value = landuse_cat) %>% osmdata_sf()
query9 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "office",value = office_cat) %>% osmdata_sf()

query_cate <- c(query1, query2, query3, query4, query5, query6, query8, query9)

## resultado poligonos
res_pol <- bind_rows(
  query_cate$osm_polygons[,c("osm_id","name","amenity")] %>% mutate(features = amenity) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","shop")] %>% mutate(features = shop) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","leisure")] %>% mutate(features = leisure) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","building")] %>% mutate(features = building) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","tourism")] %>% mutate(features = tourism) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","highway")] %>% mutate(features = highway) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","landuse")] %>% mutate(features = landuse) %>% select("osm_id", "name", "features"),
  query_cate$osm_polygons[,c("osm_id","name","office")] %>% mutate(features = office) %>% select("osm_id", "name", "features"))

nrow(res_pol);uniqueN(res_pol$osm_id)
setDT(res_pol)
res_pol$features <- ifelse(res_pol$features == "", NA, res_pol$features)
res_pol <- st_sf(distinct(res_pol)[order(osm_id, features)][,head(.SD,1), by = osm_id]) #24161
colSums(is.na(res_pol))

## resultado multipoligonos
res_mul_pol <- bind_rows(
  query_cate$osm_multipolygons[,c("osm_id","name","amenity")] %>% mutate(features = amenity) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","shop")] %>% mutate(features = shop) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","building")] %>% mutate(features = building) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","highway")] %>% mutate(features = highway) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","landuse")] %>% mutate(features = landuse) %>% select("osm_id", "name", "features"),
  query_cate$osm_multipolygons[,c("osm_id","name","office")] %>% mutate(features = office) %>% select("osm_id", "name", "features"))

nrow(res_mul_pol);uniqueN(res_mul_pol$osm_id)
setDT(res_mul_pol)
res_mul_pol$features <- ifelse(res_mul_pol$features == "", NA, res_mul_pol$features)
res_mul_pol <- st_sf(na.omit(distinct(res_mul_pol))[order(osm_id, features)][,head(.SD,1), by = osm_id]) #190
colSums(is.na(res_mul_pol))

#cosa <- res_mul_pol %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84")); mapview(cosa)

## resultado puntos
res_points <- bind_rows(subset(query_cate$osm_points[,c("osm_id", "name", "amenity")], amenity %in% amenity_cat) %>% mutate(features = amenity) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "shop")], shop %in% shop_cat) %>% mutate(features = shop) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "leisure")], leisure %in% leisure_cat)%>% mutate(features = leisure) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "building")], building %in% building_cat)%>% mutate(features = building) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "tourism")], tourism %in% tourism_cat)%>% mutate(features = tourism) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "highway")], highway %in% "bus_stop")%>% mutate(features = highway) %>% select("osm_id", "name", "features"),
                        subset(query_cate$osm_points[,c("osm_id", "name", "office")], office %in% office_cat)%>% mutate(features = office) %>% select("osm_id", "name", "features"))

nrow(res_points);uniqueN(res_points$osm_id)
setDT(res_points)
res_points <- st_sf(distinct(res_points)[order(osm_id, features)][,head(.SD,1), by = osm_id]) 
colSums(is.na(res_points))

## Agrego las vías principales
res_vias <- query7$osm_lines[,c("osm_id","name","highway")]%>% mutate(features = highway) %>% select("osm_id", "name", "features")

## Uno poligonos, con multipoligonos y puntos
res_query <- bind_rows(res_pol, res_mul_pol, res_points, res_vias) #68215
res_query <- subset(res_query, !is.na(features)) #67330
res_query <- subset(res_query, features %in% all_features) #64009
res_query$name <- ifelse(res_query$name == "", NA, res_query$name)
res_query <- subset(res_query, !(features == "yes" & is.na(name))) #49515
res_query <- res_query %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84"))

mapview(res_query)

`%not_in%` <- Negate(`%in%`)
res_query <- subset(res_query, features %not_in% highway_cat) #43659

colSums(is.na(res_query))

mapview(res_query)
sort(unique(res_query$features))

## Gurado la base
saveRDS(object = as.data.table(res_query), file = "Bases/osmdata_base.RDS")
rm(list=ls())
gc(T)

#=====================================================================================================================================================

#----------------------------------------------------------------------#
#------ Codigo para extraer las coordenadas de un geometry POINT ------#
#----------------------------------------------------------------------#

## Leo los datos
crs <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "amenity",value = "bank") %>% osmdata_sf()
osmdata_base <- readRDS("Bases/osmdata_base.RDS") %>% st_sf() %>% st_transform(st_crs(crs$osm_points))

## Extraccion de las coordenadas del objeto geometry
base_puntos <- subset(osmdata_base, st_geometry_type(geometry) == "POINT")
base_puntos_coords <- do.call(rbind, st_geometry(base_puntos)) %>% as_tibble() %>% setNames(c("lon","lat"))

## Se pegan las coordenas de forma individual
base_puntos <- bind_cols(base_puntos, base_puntos_coords) %>% as.data.frame()
base_puntos <- base_puntos[,c(1:5)]

#------ Lectura de la subdivisión creada anterior ------#
pol_100 <- geojson_read("Bases/bogota_subdivi_100.geojson", what = "sp")
mapview(pol_100)

osm_coord <- data.frame(Longitude = base_puntos$lon, Latitude = base_puntos$lat, osm_id = base_puntos$osm_id)

#------ Alinear coordenadas ------#
sp::coordinates(osm_coord) <- ~ Longitude + Latitude
sp::proj4string(osm_coord) <- sp::proj4string(pol_100)

#------ Generar tabla con los puntos encontrados ------#
poligonos <- sp::over(osm_coord, pol_100)
colnames(poligonos) <- c("pol_id")

## Se agrega el id del poligono a la osm_data
base_puntos <- bind_cols(base_puntos, poligonos)

## Guardo la base final con la asignacion de los poligonos
saveRDS(object = base_puntos, file = "Bases/base_puntos_pol.RDS")

#=====================================================================================================================================================

#----------------------------------------------------------------------#
#------ Se obtienen las vías troncales, principales y secundarias -----#
#----------------------------------------------------------------------#

base_poligonos_multi <- subset(osmdata_base, st_geometry_type(geometry) != "POINT")
base_poligonos_multi <- base_poligonos_multi %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84")) %>% st_make_valid()

## Transformo las coordenadas de los poligonos
pol_100 <- st_as_sf(geojson_read("Bases/bogota_subdivi_100.geojson", what = "sp")) %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84")) %>% st_make_valid()

## Realizo el join para ver la interseccion entre las carreteras y los poligonos
pol_multi <- sf::st_join(pol_100, base_poligonos_multi, left = TRUE) %>% as.data.frame()

## Guardo la base final con la asignacion de los poligonos
saveRDS(object = pol_multi, file = "Bases/base_multi_pol.RDS")

## Miro las etiquetas para la característica de vías
available_tags("highway")

## Realizo el query a la app de overpass
query7 <- opq(bbox = "Bogota", timeout = 25*100) %>% add_osm_feature(key = "highway",value = c("primary","secondary","trunk")) %>% osmdata_sf()
res_highway <- query7$osm_lines[,c("osm_id","name","highway")] %>% mutate(features = highway) %>% dplyr::select("osm_id", "name", "features")
#mapview(res_highway)

## Transformo las coordenadas de las vias
carreteras <-  res_highway %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84")) %>% st_make_valid()

## Transformo las coordenadas de los poligonos
pol_100 <- st_as_sf(geojson_read("Bases/bogota_subdivi_100.geojson", what = "sp")) %>% st_transform(st_crs("+proj=utm +ellps=GRS80 +datum=WGS84")) %>% st_make_valid()

## Realizo el join para ver la interseccion entre las carreteras y los poligonos
pol_carretera <- sf::st_join( pol_100,carreteras,  left = TRUE) %>% as.data.frame()
colSums(is.na(pol_carretera))
pol_carretera_2 <- pol_carretera[!is.na(osm_id)]

## Gurado el resultado final
saveRDS(pol_carretera_2[,c("ID","features")],"Bases/vias_bogota.RDS")

#=====================================================================================================================================================

#----------------------------------------------------------------------#
#------ Uno la base de puntos, poligonos y multipoligonos         -----#
#----------------------------------------------------------------------#

list(rm = ls()); gc(T)

## Guardo la base final con la asignacion de los poligonos
base_puntos_pol <- as.data.table(readRDS("Bases/base_puntos_pol.RDS"))
base_multi_pol <- as.data.table(readRDS("Bases/base_multi_pol.RDS"))
vias_bogota <- as.data.table(readRDS("Bases/vias_bogota.RDS"))

## uno las bases
osmdata_base <- bind_rows(base_puntos_pol[,.(ID = pol_id, features)], base_multi_pol[,.(ID, features)], vias_bogota[,.(ID, features)])
osmdata_base <- na.omit(osmdata_base)
saveRDS(osmdata_base, "Bases/osmdata_pol.RDS")

#=====================================================================================================================================================

#----------------------------------------------------------------------#
#------ Agrupo las categorias y contarlas por el ID de poligono   -----#
#----------------------------------------------------------------------#

rm(list = ls());gc(T)
library(data.table)
osm_pol <- as.data.table(readRDS("Bases/osmdata_pol.RDS"))

sort(unique(osm_pol$features)); length(sort(unique(osm_pol$features)))

## Categorias
bares <- c("alcohol","bar")
residencial <- c("apartments","house", "residential")
bancario <- c("atm","bank")
panaderias <- c("bakery", "cafe")
parqueaderos <- c("bicycle_parking","motorcycle_parking","parking")
paraderos <- c("bus_station", "bus_stop")
templos <- c("chapel", "church", "place_of_worship")
hospitales <- c("clinic", "hospital")
educacion <- c("college","school","university")
comercial <- c("commercial", "general") 
tiendas <- c("baby_goods","butcher","convenience","dairy","food","greengrocer","grocery","kiosk","retail") 
grandes_superficies <- c("department_store",	"doityourself", "mall", "marketplace","supermarket") 
restaurantes <- c("fast_food","restaurant","ice_cream") 
gimnasios <- c("fitness_centre","sports","sports_centre") 
cementerios <- c("funeral_directors","cemetery","grave_yard") 
hoteles <- c("hostel", "hotel","motel") 
industrial <- c("industrial","warehouse") 
droguerias <- c("medical_supply","pharmacy") 
oficinas <- c("office", "Oficina Central","public", "company","financial","insurance","notary","yes") 
correo <- c("payment_centre","payment_terminal","post_office") 
mascotas <- c("pet", "veterinary") 
defensa <- c("police","military") 
vias <- c("fuel","primary","primary_link","secondary","trunk")

## Creo la variable categorias en la base
osm_pol <- osm_pol %>% mutate(categoria = case_when(features %in% bares ~ "bares",#1
                                                    features %in% residencial ~ "residencial",#2
                                                    features %in% bancario ~ "bancario",#3
                                                    features %in% panaderias ~ "panaderias",#4
                                                    features %in% parqueaderos ~ "parqueaderos",#5
                                                    features %in% paraderos ~ "paraderos",#6
                                                    features %in% templos ~ "templos",#7
                                                    features %in% hospitales ~ "hospitales",#8
                                                    features %in% educacion ~ "educacion",#9
                                                    features %in% comercial ~ "comercial",#10
                                                    features %in% tiendas ~ "tiendas",#11
                                                    features %in% grandes_superficies ~ "grandes_superficies",#12
                                                    features %in% restaurantes ~ "restaurantes",#13
                                                    features %in% gimnasios ~ "gimnasios",#14
                                                    features %in% cementerios ~ "cementerios",#15
                                                    features %in% hoteles ~ "hoteles",#16
                                                    features %in% industrial ~ "industrial",#17
                                                    features %in% droguerias ~ "droguerias",#18
                                                    features %in% oficinas ~ "oficinas",#19
                                                    features %in% correo ~ "correo",#20
                                                    features %in% mascotas ~ "mascotas",#21
                                                    features %in% defensa ~ "defensa",#22
                                                    features %in% vias ~ "vias"))#23

osm_dcast <- dcast(data = osm_pol, formula = ID ~ categoria, fun.aggregate = length)
# mi_pol <- osm_dcast[ID == 4044]; osm_pol[order(categoria)][ID == 4044,.(n = .N), by = categoria]
# leaflet() %>% addTiles() %>% addCircles(lng = mi_pol$lon, lat = mi_pol$lat, popup = mi_pol$name)
saveRDS(osm_dcast, "Bases/osm_dcast.RDS")

# https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

library(data.table)
library(dplyr)
osm_dcast <- as.data.table(readRDS("Bases/osm_dcast.RDS"))
Final_Todos <- as.data.table(readRDS("Bases/Final_Todos.RDS"))

Final_Todos_OSM <- left_join(x = Final_Todos, y = osm_dcast, by = c("ID_Poly_100"="ID"))
saveRDS(Final_Todos_OSM, "Bases/Final_Todos_OSM.RDS")

Final_Todos_OSM <- readRDS("Bases/Final_Todos_OSM.RDS")
