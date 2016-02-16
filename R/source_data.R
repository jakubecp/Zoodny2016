rm(list = ls())
#install.packages (c("rgbif", "raster ", "maptools", "XML", "rgdal", "dismo",
#                  "sqldf", "maps ", "testthat","roxygen2",
#                  "celestial", "ggplot2", "rJava"))
#=======================================================================================
#PACKAGES used in this script
library (rgbif) #nefunguje 
library (raster)
library (maptools) # for wrld_simpl
library (rgdal) #nefunguje pod ubuntu
library (dismo)
library (celestial)
library(spocc)


#=======================================================================================
##Loading all datasets
cz.nic.raw = read.csv ("data/nicr.czech.csv", header= TRUE, sep=";")
sikes_sil <- read.csv ("data/Nicrophorinae_Sikes.csv", header=T, sep=";")
ruzicka_sil <- read.csv ("data/Nicrophorinae_Ruzicka.csv", header=T, sep=";")


#MANIpulation with my original dataset from soil study cz.nic.raw.csv
#=======================================================================================
## second solution for transformation of DMS to decimal degrees
## with celestila packages and function dms2deg
n=length(cz.nic.raw$lat)
cz.lat = c()
cz.long = c()
for (i in 1:n) {
  cz.lat[i]=dms2deg (as.character (cz.nic.raw[i,2]), sep="DMS")
  cz.long[i]=dms2deg (as.character (cz.nic.raw[i,3]), sep="DMS")
}
head (cz.nic.raw)

## merge three datasets cz.lat, cz.long, and spec
## + decimalLongitude and decimalLatitude
#coord = data.frame (endang_nicro, spec)
## bind data from previous manipulation (dms to dd) with presence/absence data
coord.cz = data.frame (long = cz.long, lat = cz.lat, 
  spec = cz.nic.raw$spec)

#=======================================================================================
# RAW data from GBIF (only records with coordinates and you should set up upper limit of them)
endang_nicro <- occ(query = c("Nicrophorus antennatus","Nicrophorus germanicus", "Nicrophorus sepultor"),
  from = 'gbif', has_coords = TRUE)
warnings()
str(endang_nicro)
endang_nicro_search<- occ_search(scientificName = c("Nicrophorus antennatus",
  "Nicrophorus germanicus", 
  "Nicrophorus sepultor", 
  "Nicrophorus vestigator"),
  hasCoordinate= TRUE, limit = 3000)
str (endang_nicro_search)

#coordinates of observations from GBIF (filter out NAs and obvious mistakes!)
Antennatus
coord.ant <- data.frame (long = endang_nicro_search$`Nicrophorus antennatus`$
    data$decimalLongitude,
  lat= endang_nicro_search$`Nicrophorus antennatus`$data$
    decimalLatitude,
  spec=rep ("antennatus", length (endang_nicro_search$
      `Nicrophorus antennatus`$data$decimalLongitude)))
#Garmanicus
coord.ger <- data.frame (long = endang_nicro_search$`Nicrophorus germanicus`$
    data$decimalLongitude,
  lat= endang_nicro_search$`Nicrophorus germanicus`$
    data$decimalLatitude,
  spec= rep ("germanicus", length (endang_nicro_search$
      `Nicrophorus germanicus`$data$decimalLongitude)))
coord.ger <- coord.ger [-c(2,3,4),] # nonsence data of occurence

#Sepultor
coord.sep <- data.frame (long = endang_nicro_search$`Nicrophorus sepultor`$
    data$decimalLongitude ,
  lat= endang_nicro_search$`Nicrophorus sepultor`$
    data$decimalLatitude,
  spec= rep ("sepultor", length (endang_nicro_search$
      `Nicrophorus sepultor`$data$decimalLongitude)))
#Vestigator
coord.vest <- data.frame (long = endang_nicro_search$`Nicrophorus vestigator`$
    data
  $decimalLongitude ,
  lat= endang_nicro_search$`Nicrophorus vestigator`$
    data$
    decimalLatitude,
  spec= rep ("vestigator", length (endang_nicro_search$
      `Nicrophorus vestigator`$data$decimalLongitude)))

endang_nicro <- rbind (coord.ant, coord.ger, coord.sep, coord.vest)
#=======================================================================================
#Data manipulation and selection for Nicrophorinae_Sikes

sikes.coord.ant<- data.frame (long = sikes_sil$long [sikes_sil$spec== 
    "antennatus"],
  lat =sikes_sil$lat [sikes_sil$spec== 
      "antennatus"],
  spec = sikes_sil$spec[sikes_sil$spec== 
      "antennatus"])
sikes.coord.ger<- data.frame (long = sikes_sil$long [sikes_sil$spec== 
    "germanicus"],
  lat =sikes_sil$lat [sikes_sil$spec== 
      "germanicus"],
  spec = sikes_sil$spec[sikes_sil$spec== 
      "germanicus"])
sikes.coord.sep<- data.frame (long = sikes_sil$long [sikes_sil$spec== 
    "sepultor"],
  lat =sikes_sil$lat [sikes_sil$spec== 
      "sepultor"],
  spec = sikes_sil$spec[sikes_sil$spec== 
      "sepultor"])
sikes.coord.vest<- data.frame (long = sikes_sil$long [sikes_sil$spec== 
    "vestigator"],
  lat =sikes_sil$lat [sikes_sil$spec== 
      "vestigator"],
  spec = sikes_sil$spec[sikes_sil$spec== 
      "vestigator"])

sikes.coord.with_NAs <- rbind (sikes.coord.ant, sikes.coord.ger,
  sikes.coord.sep, sikes.coord.vest)
sikes.coord<- sikes.coord.with_NAs [complete.cases(sikes.coord.with_NAs),]
#=======================================================================================
#Data manipulation and selection for Nicrophorinae_Ruzicka

ruzicka.coord.ant <- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== 
    "antennatus"],
  lat =ruzicka_sil$lat [ruzicka_sil$DRUH== 
      "antennatus"],
  spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== 
      "antennatus"])
ruzicka.coord.ger<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== 
    "germanicus"],
  lat =ruzicka_sil$lat [ruzicka_sil$DRUH== 
      "germanicus"],
  spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== 
      "germanicus"])
ruzicka.coord.sep<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== 
    "sepultor"],
  lat =ruzicka_sil$lat [ruzicka_sil$DRUH== 
      "sepultor"],
  spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== 
      "sepultor"])
ruzicka.coord.vest<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== 
    "vestigator"],
  lat =ruzicka_sil$lat [ruzicka_sil$DRUH== 
      "vestigator"],
  spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== 
      "vestigator"])

ruzicka.coord.with_NAs <- rbind (ruzicka.coord.ant, ruzicka.coord.ger, 
  ruzicka.coord.sep, ruzicka.coord.vest)
ruzicka.coord<- ruzicka.coord.with_NAs [complete.cases(ruzicka.coord.with_NAs),]
#=======================================================================================

## bind both dataframes (GBIF + CZ) together
coord.full = rbind (endang_nicro, coord.cz, ruzicka.coord, sikes.coord)
# coord.full = rbind (coord.cz, ruzicka.coord, sikes.coord)
head(coord.full)
write.csv2(coord.full, file= "data/coord.full.csv", row.names=F, dec=".")
write.table(coord.full, file= "data/coord.full.csv", row.names=F, dec=".", sep=";")
