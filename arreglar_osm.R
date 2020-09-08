rm(list=ls())
gc(T)

library(dplyr)
library(data.table)
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(reshape2)

#categories <- available_features()[-c(5:23)]
osm_raw_info <- readRDS("Bases/osm_raw_info.RDS")


dd <- osm_raw_info[[1]]
interes_cat <- c("amenity","shop","leisure","building","healthcare")


for(i in 1:length(osm_raw_info)){
  cat("i:",i,"\n")
  dd_2 <- osm_raw_info[[i]]
  setDT(dd_2)
  dd_2 <- dd_2[k %in% interes_cat,]
  dd_2$pol <- i
  dd <- bind_rows(dd,dd_2)
}


base_comp <- dd
setDT(base_comp)

amenity <- c("bar","fuel","restaurant","college","atm","parking","fast_food","cafe","pharmacy","veterinary","bank","post_office","marketplace",
             "hospital","clinic","payment_terminal","police","payment_centre","Paga Todo","shop","restaurant;fast_food","restaurant;bank")
shop <- c('convenience','bakery','butcher','supermarket','yes','department_store',
          'dairy','baby_goods','sports','pet','kiosk','alcohol',
          'greengrocer','supermarket;radiotechnics','mall','grocery','food','medical_supply','supermaket',
          'funeral_directors','de_barrio','Plaza de mercado Tunjuelito','Plaza de mercado San Benito')
leisure <- c('park','dog_park','sport','stadium')
building <- c('yes','house','commercial','apartments','Banco Davivienda','public','residential',
              'university','church','office','industrial','retail','kiosk','chapel',
              'Oficina Central','hospital','warehouse','college')
tourism <- c('apartment','motel','hotel','guest_house','hostel','museum')
healtcare <- c('pharmacy','doctor','clinic','hospital')
base_comp <- base_comp[ v %in% c(amenity,shop,leisure,building,tourism,healtcare)]


#######################################################################################################
publicos <- c('park','stadium','public','church','chapel','museum')
tiendas <- c("yes","shop",'sports','sport')
restaurante <- c("bar","restaurant","fast_food","cafe","restaurant;fast_food","restaurant;bank")
servicios <- c("fuel","parking","post_office","police",'funeral_directors','motel','hotel','guest_house','hostel')
instituto <- c("college",'university','college')
atm <- c("atm","payment_terminal","payment_centre","Paga Todo")
farmacias <- c("pharmacy",'medical_supply','pharmacy')
pets <- c("veterinary",'pet','dog_park')
bancos <- c("bank",'Banco Davivienda')
supermercados <- c("marketplace",'convenience','supermarket','department_store','supermarket;radiotechnics','mall','supermaket')
hospitalarios <- c("hospital","clinic",'hospital','doctor','clinic','hospital')
pasteleria <- c("bakery")
tiendas_barrio <- c('butcher','dairy','baby_goods','kiosk','alcohol','greengrocer','grocery','food','de_barrio','Plaza de mercado Tunjuelito','Plaza de mercado San Benito','retail','kiosk',"commercial")
otros <- c('yes','office','industrial','Oficina Central','warehouse')
residencial <- c('house','apartments','residential','apartment')


base_comp[,':='(categoria = case_when(v %in% publicos ~ "publicos",
                                      v %in% tiendas ~ "tiendas",
                                      v %in% restaurante ~ "restaurante",
                                      v %in% servicios ~ "servicios",
                                      v %in% instituto ~ "instituto",
                                      v %in% atm ~ "atm",
                                      v %in% farmacias ~ "farmacias",
                                      v %in% pets ~ "pets",
                                      v %in% bancos ~ "bancos",
                                      v %in% supermercados ~ "supermercados",
                                      v %in% hospitalarios ~ "hospitalarios",
                                      v %in% pasteleria ~ "pasteleria",
                                      v %in% tiendas_barrio ~ "tiendas_barrio",
                                      v %in% otros ~ "otros",
                                      v %in% residencial ~ "residencial",
                                      TRUE ~ "otros"))]
base_comp[,':='(k=NULL,v=NULL)]
d <- base_comp[,.(n=.N),by=id][order(-n)]
base_comp_2 <- base_comp[id %in% d[n<5,]$id,][,head(.SD,1),by=id]
pol_cat <- base_comp_2[,.(cantidad=.N),by=.(pol,categoria)]
pol_cat2 <- dcast(pol_cat, pol~categoria,value.var = "cantidad")

saveRDS(pol_cat2,"externas/osm/osm_variables.RDS")

