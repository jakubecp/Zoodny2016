rm(list = ls())
#install.packages (c("rgbif", "raster ", "maptools", "XML", "rgdal", "dismo",
#                  "sqldf", "maps ", "testthat","roxygen2",
#                  "celestial", "ggplot2", "rJava"))
 library (rgbif) #nefunguje 
# library (raster)
# library (maptools)
# library (XML) #nefunguje pod ubuntu
# library (rgdal) #nefunguje pod ubuntu
# library (dismo)
# library (sqldf)
# library (maps)
# library (testthat)
# library (roxygen2)
library (celestial)
library (ggplot2)
library (maps)
#install.packages("spocc", dependencies = TRUE)
#install.packages("mapproj")
library(spocc)
#install.packages("mapr", dependencies = TRUE)
library (mapr)
#install.packages("devtools")
library (devtools)
#data(wrld_simpl) #create the World map with borders
##trasfering data from our observations in czech republic
#setwd ("C:/Users/jakubecp/Dropbox/SGEM_2015/Article_1")# skola
#setwd ("C:/Users/pavel/Downloads/Dropbox/SGEM_2015/Article_1/") #doma
#setwd ("/home/pavel/Dropbox/SGEM_2015/Article_1 ") #linux
cz.nic.raw = read.csv ("data/nicr.czech.csv", header= TRUE, sep=";")
sikes_sil <- read.csv ("data/Nicrophorinae_Sikes.csv", header=T, sep=";")
ruzicka_sil <- read.csv ("data/Nicrophorinae_Ruzicka.csv", header=T, sep=";")
head(ruzicka_sil)
## first solution for transformation of DMS into the decimal degrees 
## with function convert
# convert<-function(coord){
#   tmp1=strsplit(coord,"D")
#   tmp2=strsplit(tmp1[[1]][2],"M")
#   tmp3=strsplit(tmp2[[1]][2],"S")
#   dec=c(as.numeric(tmp1[[1]][1]),as.numeric(tmp2[[1]][1]),as.numeric(tmp3[[1]]))
#   c<-abs(dec[1])+dec[2]/60+dec[3]/3600
#   c<-ifelse(dec[1]<0,-c,c)
#   return(c)
# }
# n1=length(cz.nic.raw$lat)
# n2=length(cz.nic.raw$long)
# cz.lat= c()
# cz.long=c()
# for (i in 1:n1) {
#   cz.lat[i]=convert (as.character (cz.nic.raw[i,5]))
#   cz.long[i]=convert (as.character (cz.nic.raw[i,6]))
#   
# }

#MANIpulation with my original dataset from soil study cz.nic.raw.csv

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
coord = data.frame (endang_nicro, spec)
## bind data from previous manipulation (dms to dd) with presence/absence data
coord.cz = data.frame (long = cz.long, lat = cz.lat, 
                       spec = cz.nic.raw$spec)


#RAW data from GBIF (only records with coordinates and you should set up upper limit of them)
# endang_nicro <- occ(query = c("Nicrophorus antennatus","Nicrophorus germanicus", "Nicrophorus sepultor"),
#                     from = 'gbif', has_coords = TRUE)
# warnings()
# str(endang_nicro)
endang_nicro_search<- occ_search(scientificName = c("Nicrophorus antennatus",
                                                    "Nicrophorus germanicus", 
                                                    "Nicrophorus sepultor", 
                                                    "Nicrophorus vestigator"),
                           hasCoordinate= TRUE, limit = 3000)
str (endang_nicro)

#coordinates of observations from GBIF (filter out NAs and obvious mistakes!)
#Antennatus
coord.ant.gbif <- data.frame (long = endang_nicro_search$`Nicrophorus antennatus`$data
                         $decimalLongitude ,
                     lat= endang_nicro_search$`Nicrophorus antennatus`$data$
                       decimalLatitude)
spec.ant.gbif <- rep ("antennatus", length (coord.ant.gbif$long))

coord.ant <- data.frame (coord.ant.raw, spec= spec.ant.gbif)

#Garmanicus
coord.ger.gbif <- data.frame (long = endang_nicro_search$`Nicrophorus germanicus`$
                           data$decimalLongitude ,
                         lat= endang_nicro_search$`Nicrophorus germanicus`$
                           data$decimalLatitude)

spec.ger.gbif <- rep ("german", length (coord.ger.gbif$long))

coord.ger <- data.frame (coord.ger.gbif, spec= spec.ger.gbif)

#Sepultor
coord.sep.gbif <- data.frame (long = endang_nicro_search$`Nicrophorus sepultor`$
                           data$decimalLongitude ,
                         lat= endang_nicro_search$`Nicrophorus sepultor`$data$
                           decimalLatitude)
spec.sep.gbif <- rep ("sepult", length (coord.sep.gbif$long))

coord.sep <- data.frame (coord.sep.gbif, spec= spec.sep.gbif)
#Vestigator
coord.vest.gbif <- data.frame (long = endang_nicro_search$`Nicrophorus vestigator`$data
                              $decimalLongitude ,
                              lat= endang_nicro_search$`Nicrophorus vestigator`$data$
                                decimalLatitude)
spec.vest.gbif <- rep ("vestigator", length (coord.vest.gbif$long))
coord.vest <- data.frame (coord.vest.gbif, spec= spec.vest.gbif)


endang_nicro <- rbind (coord.ant, coord.ger, coord.sep, coord.vest)

#Data manipulation and selection for Nicrophorinae_Sikes


sikes.coord.ant<- data.frame (long = sikes_sil$long [sikes_sil$spec== "antennatus"],
                            lat =sikes_sil$lat [sikes_sil$spec== "antennatus"],
                            spec = sikes_sil$spec[sikes_sil$spec== "antennatus"])
sikes.coord.ger<- data.frame (long = sikes_sil$long [sikes_sil$spec== "germanicus"],
                              lat =sikes_sil$lat [sikes_sil$spec== "germanicus"],
                              spec = sikes_sil$spec[sikes_sil$spec== "germanicus"])
sikes.coord.sep<- data.frame (long = sikes_sil$long [sikes_sil$spec== "sepultor"],
                              lat =sikes_sil$lat [sikes_sil$spec== "sepultor"],
                              spec = sikes_sil$spec[sikes_sil$spec== "sepultor"])
sikes.coord.vest<- data.frame (long = sikes_sil$long [sikes_sil$spec== "vestigator"],
                              lat =sikes_sil$lat [sikes_sil$spec== "vestigator"],
                              spec = sikes_sil$spec[sikes_sil$spec== "vestigator"])

sikes.coord.with_NAs <- rbind (sikes.coord.ant, sikes.coord.ger, sikes.coord.sep, sikes.coord.vest)
sikes.coord<- sikes.coord.with_NAs [complete.cases(sikes.coord.with_NAs),]
#Data manipulation and selection for Nicrophorinae_Ruzicka

ruzicka.coord.ant <- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== "antennatus"],
                                 lat =ruzicka_sil$lat [ruzicka_sil$DRUH== "antennatus"],
                                 spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== "antennatus"])
ruzicka.coord.ger<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== "germanicus"],
                              lat =ruzicka_sil$lat [ruzicka_sil$DRUH== "germanicus"],
                              spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== "germanicus"])
ruzicka.coord.sep<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== "sepultor"],
                              lat =ruzicka_sil$lat [ruzicka_sil$DRUH== "sepultor"],
                              spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== "sepultor"])
ruzicka.coord.vest<- data.frame (long = ruzicka_sil$long [ruzicka_sil$DRUH== "vestigator"],
                                lat =ruzicka_sil$lat [ruzicka_sil$DRUH== "vestigator"],
                                spec = ruzicka_sil$DRUH[ruzicka_sil$DRUH== "vestigator"])

ruzicka.coord.with_NAs <- rbind (ruzicka.coord.ant, ruzicka.coord.ger, ruzicka.coord.sep, ruzicka.coord.vest)
ruzicka.coord<- ruzicka.coord.with_NAs [complete.cases(ruzicka.coord.with_NAs),]



## bind both dataframes (GBIF + CZ) together
coord.full = rbind (endang_nicro, coord.cz, ruzicka.coord, sikes.coord)


# ## created two data frames with presence data and absence data.
# coord = data.frame (long = coord.full$long [coord.full$antenn == "1"],
#                     lat = coord.full$lat [coord.full$antenn == "1"])
# coord.neg = data.frame (long = coord.full$long [coord.full$antenn == "0"],
#                         lat = coord.full$lat [coord.full$antenn == "0"])

X11()
plot (coord, xlim=c(12,25), ylim=c(40,55))
plot (wrld_simpl, add=T)
#choose the right (important) climatic variables (http://www.worldclim.org/bioclim) 
#for your species and stack them! USE ENFA (package adehabitat) for selection of the right variables 
#if you do not know a lot about them
setwd ("C:/Users/pavel/Downloads/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/worldclim/") #notas
#setwd ("/home/pavel/Documents/ENM_2015_Varela/climatic_layers/worldclim") #linux
?enfa
variable<- stack (c("bio10.bil", "bio8.bil", "bio16.bil", "bio1.bil"))

#optional-if you are interested in more local (and quicker) predictions 
#make an object (e) of certain extant (xmin, xmax, ymin, ymax) for croping
e<-extent (0,40,40,53)
#crop your climatic maps
variable_crop<- crop (variable, e)

#Project niches of target species by extracting values from raster and ploting them
niche <- extract (variable_crop, coord)
niche <- as.data.frame (niche)
X11()
par (mfrow=c(1,2))
plot (niche$bio1, niche$bio10, xlab= "prectip of warmest qrt" 
      , ylab= "temp warmest qurt" )
plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" ,
      ylab= "temp of wettest qrt" )

# MAXENT model (basic setup) - creates values of the model,
# which are used in checking the behavior of the model 
# and making predictions (fallowing steps) 
maxent_all <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
                                                   "betamultiplier=1",
                                                   "defaultprevalence=0.5"))
# #maxent_all2 <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
#                                                     "betamultiplier=2",
#                                                     "defaultprevalence=0.5"))
# #maxent_all5 <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
#                                                     "betamultiplier=5",
#                                                     "defaultprevalence=0.5"))
#check the behavior of your data to variables (graph) and play
#with "betamultiplier" for smoother model of climatic variables (values= 1 - inf)
X11()
response (maxent_all)
# response (maxent_all2)
# response (maxent_all5)

#all values
maxent_all@results

#just AUC
maxent_all@results[5]
# maxent_all2@results[5]
# maxent_all5@results[5]

#Predict probability of occurence
maxent_all_predict<- predict (maxent_all, variable_crop)

#Plot the prediction
X11()
plot (maxent_all_predict, 
      main="Nicrophorus antennatus distribution (Maxent/all)", xlim =c(-120,40),ylim=c(30,70) )
plot (wrld_simpl, add=TRUE, xlim=c(-120,40),ylim=c(30,70))


#reclasification reclasification (based on maximum training sensitivityplus specificity logistic treshold)
m = c(0.5014,1,1,0,0.5013,0)
rclmat = matrix (m,ncol=3,byrow=TRUE)
endangered_reclas<- reclassify (maxent_all_predict, rclmat)
#X11()
#plot (endangered_reclas)
#plot (wrld_simpl, add=TRUE, axes=FALSE) #not a best resolution
#map("world", interior = TRUE, xlim=c(0,80), ylim=c(20,70), add=TRUE)#this is better resolution
#map("world", boundary = FALSE, col="gray", add = TRUE) #this could make an interior 
#of europe be with gray boarders

#experiments with maps - This is IT!!!

newmap = getMap(resolution="low")
plot (endangered_reclas)
plot (newmap, xlim=c(5,45), ylim=c(42,56), add=T)

##EXport to TIFF
setwd ("C:/Users/jakubecp/Dropbox/SGEM_2015/Article_1")# skola
tiff (filename="anennatus.tiff", width=5000, height=5000, 
      compression="lzw", res= 800)
plot (endangered_reclas, legend=F, xlim=c(-10,35), ylim=c(35,65))
plot (newmap, xlim=c(5,45), ylim=c(42,56), add=T)
dev.off()
#EVALUATION OF THE MAXENT MODEL
#crete object with random split of the data into k(5) subsamples by kfold
fold <- kfold(coord,k=5)
#Create training subdataset of 80% of the data for modeling by selecting 
#everything except the number one ((100%/5)*4=80%)
occtrain <- coord[fold !=1,]
#Create testing subdataset of 20% of the data to validate the model by selecting
#only the number one
occtest <- coord [fold==1,]

# MAXENT model (basic setup) for training data
maxent_occtrain <- maxent (variable_crop, occtrain, args=c("maximumbackground=1000",
                                                           "betamultiplier=5",
                                                           "defaultprevalence=0.5"))
#Prediction for training data
maxent_occtrain_predict <- predict (maxent_occtrain, variable_crop )

#PLotting training subdataset vs. whole dataset
x11()
plot (maxent_all_predict, main="Sciodrepoides watsoni distribution (Maxent/all)")
plot (wrld_simpl, add=TRUE)
x11()
plot (maxent_occtrain_predict, main="Sciodrepoides watsoni distribution (Maxent/training)")
plot (wrld_simpl, add=TRUE)

#what is discriminant value (AUC)
maxent_occtrain@results[5]
#what is maximum training sensitivityplus specificity logistic treshold 
#aka where to split
maxent_occtrain@results
#what is dimmension of my data to enter right number of random points
dim(occtest)

#testing on pseudoabsences (number 100 is estimated based on the dim value)
pseudoabsence <- randomPoints (variable_crop, 650)
x11()
plot (coord.neg)
plot (wrld_simpl,add=T)
points (coord, col="red")

#evaluation compare the value of AUC 
#from this evaluate_nicveo with value of training set 
#evaluate will compere with random pseudoabsence set and 
#come with new AUC = compare it with maxent_training_nicveo@results[5]

evaluate_maxent_occtrain <- evaluate (maxent_occtrain,
                                      p=occtest, a=pseudoabsence,
                                      x=variable_crop)

#Evaluate AUC between these two models:
evaluate_maxent_occtrain
maxent_occtrain@results[5]

#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE

#predicting in the past by function predict (every prediction should be run on the current 
#data and then the prediction for the future or the past - use the same mathematical model for both)
setwd ("C:/Users/pavel/Downloads/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/CCSM_21/")
variable21<- stack (c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil"))
glac_crop <- crop (variable21, e)
nic_glac <- predict (maxent_occtrain, glac_crop)
X11()
plot (nic_glac, main="Sciodrepoides watsoni in the last glacial (Maxent/training)")
plot (wrld_simpl,add=T)

#other maps of climate models (ECOclim - past layers )
#future MIROC_21 ()
setwd ("C:/Users/pavel/Downloads/Spatial_modeling/ENM_2015_Varela/climatic_layers/MIROC_21/")
variable22<- stack (c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil"))
fut_crop <- crop (variable22, e)
nic_fut <- predict (maxent_occtrain, fut_crop)
x11()
plot (nic_fut, main="Sciodrepoides watsoni MIROC_21 (Maxent/training)")
plot (wrld_simpl,add=T)


##ADITIONAL STUFF
#extract = take values from raster
niche <- extract (var_imp_crop, coord)
niche <- as.data.frame (niche)
plot (niche$bio18, niche$bio10, xlab= "prectip of warmest qrt" , ylab= "temp warmest qurt" )
plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" , ylab= "temp of wettest qrt" )

#reclassify to places where it is and where it is not (1 or 0)
nicveo_reclas<- reclassify (map_nicveo_new, c(0,1,1))
plot (nicveo_reclas)
points (nicveo$data$decimalLongitude [nicveo$data$decimalLongitude>0],
        nicveo$data$decimalLatitude[nicveo$data$decimalLongitude>0], 
        pch=16, col="red", main = "wettest" )
