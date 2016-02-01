rm(list = ls())
#install.packages (c("rgbif", "raster ", "maptools", "XML", "rgdal", "dismo",
#                  "sqldf", "maps ", "testthat","roxygen2",
#                  "celestial", "ggplot2", "rJava"))
#=======================================================================================
#PACKAGES used in this script
 library (raster)
 library (maptools) # for wrld_simpl
# library (XML) #nefunguje pod ubuntu
library (rgdal) #nefunguje pod ubuntu
 library (dismo)
# library (sqldf)
# library (maps)
# library (testthat)
# library (roxygen2)
#install.packages("rJava")
library (rJava)
library (celestial)
library (ggplot2)
library (maps)
#install.packages("spocc", dependencies = TRUE)
library (mapproj)
#install.packages("mapr", dependencies = TRUE)
library (mapr)
#install.packages("ggmap")
library(ggmap)
library(rgdal)
#install.packages(c("mapproj", "maps"))
#install.packages("devtools")
#library (devtools)
#=======================================================================================
data(wrld_simpl) #create the World map with borders
#=======================================================================================
##Loading all datasets
coord.full = read.csv ("data/coord.full.csv", header= TRUE, sep=";")

#=======================================================================================
#map of endangered Nicrophorinae occurence in Europe
map=get_map (location="Europe", zoom=4)
ggmap(map)
str(map)

# tiff (filename="outputs/Nicrophoriane_occurence.tiff", 
#       width=2000, height=2000, 
#       compression="lzw", res= 300)

nicroph.occur=  ggmap(map)+
  geom_point (aes (x = endang_nicro$long, 
                   y = endang_nicro$lat, colour=endang_nicro$spec ), data = endang_nicro)+
  xlab("")+
  ylab("")

plot (coord.full$long,coord.full$lat)
plot (wrld_simpl, add=T)

# dev.off()

#=======================================================================================
# ## created two data frames with presence data and absence data.
# coord = data.frame (long = coord.full$long [coord.full$antenn == "1"],
#                     lat = coord.full$lat [coord.full$antenn == "1"])
# coord.neg = data.frame (long = coord.full$long [coord.full$antenn == "0"],
#                         lat = coord.full$lat [coord.full$antenn == "0"])

# X11()
# #plot (coord.full, xlim=c(12,25), ylim=c(40,55))
# plot (coord.full)
# plot (wrld_simpl, add=T)
#choose the right (important) climatic variables (http://www.worldclim.org/bioclim) 
#for your species and stack them! USE ENFA (package adehabitat) for selection of the right variables 
#if you do not know a lot about them
#setwd ("C:/Users/pavel/Downloads/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/worldclim/") #notas
#setwd ("/home/pavel/Documents/ENM_2015_Varela/climatic_layers/worldclim") #linux
#
#=======================================================================================
#lLading ecoregions in R
# ecoregions <- readOGR (dsn = "D:/Spatial_modeling/ENM_2015_Varela/climatic_layers/WWE_ecoregions",
#                        layer = "wwf_terr_ecos")

ext <-  extent (-20, 45, 20, 63)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 5
r <- raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)

# raster_ecoregions <- rasterize (ecoregions,r)

#=======================================================================================

#loading and stacking bioclimatic data
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

#Loading data from worldclim
in_dir ("D://Zaloha_notebook/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/worldclim", variable_clim<- stack (c("bio1.bil", 
  "bio2.bil", 
  "bio3.bil", 
  "bio4.bil",
  "bio5.bil",
  "bio6.bil",
  "bio7.bil",
  "bio8.bil",
  "bio9.bil",
  "bio10.bil",
  "bio11.bil",
  "bio12.bil",
  "bio13.bil",
  "bio14.bil",
  "bio15.bil",
  "bio16.bil",
  "bio17.bil",
  "bio18.bil",
  "bio19.bil"))) 

variable_clim_crop<- crop (variable_clim, ext)
#=======================================================================================
#merging all rasters
# variable <- stack (c(variable_clim_crop, raster_ecoregions))
# 
# plot (variable_clim_crop)
#=======================================================================================
#subset of long and latitude data for each Nicrophorus species
coord.antennatus <- subset (coord.full [coord.full$spec == "antennatus",], select = c(long, lat))
coord.germanicus <- subset (coord.full [coord.full$spec == "germanicus",], select = c(long, lat))
coord.sepultor <- subset (coord.full [coord.full$spec == "sepultor",], select = c(long, lat))
coord.vestigator <- subset (coord.full [coord.full$spec == "vestigator",], select = c(long, lat))

#=======================================================================================

#Project niches of target species by extracting values from raster and ploting them
niche <- extract (variable_clim_crop, coord.antennatus)
niche <- as.data.frame (niche)
X11()
par (mfrow=c(1,2))
str(niche)
plot (niche)
plot (niche$bio1, niche$bio12, xlab= "prectip of warmest qrt" 
      , ylab= "temp warmest qurt" )
plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" ,
      ylab= "temp of wettest qrt" )
#=======================================================================================
# MAXENT model (basic setup) - creates values of the model,
# which are used in checking the behavior of the model 
# and making predictions (fallowing steps) 
maxent_ant <- maxent (variable_clim_crop, coord.antennatus, args=c("maximumbackground=1000",
                                                   "betamultiplier=1",
                                                   "defaultprevalence=0.5"))
maxent_ger <- maxent (variable_clim_crop, coord.germanicus, args=c("maximumbackground=1000",
  "betamultiplier=1",
  "defaultprevalence=0.5"))
maxent_sep <- maxent (variable_clim_crop, coord.sepultor, args=c("maximumbackground=1000",
  "betamultiplier=1",
  "defaultprevalence=0.5"))
maxent_ves <- maxent (variable_clim_crop, coord.vestigator, args=c("maximumbackground=1000",
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
# X11()
# par (mfrow=c(2,2))
# response (maxent_ant)
# response (maxent_ger)
# response (maxent_sep)
# response (maxent_ves)

# response (maxent_all2)
# response (maxent_all5)

#all values
# maxent_all@results

#just AUC
maxent_ant@results[5]
maxent_ger@results[5]
maxent_sep@results[5]
maxent_ves@results[5]
# maxent_all2@results[5]
# maxent_all5@results[5]

#Predict probability of occurence
maxent_ant_predict<- predict (maxent_ant, variable_clim_crop)
maxent_ger_predict<- predict (maxent_ger, variable_clim_crop)
maxent_sep_predict<- predict (maxent_sep, variable_clim_crop)
maxent_ves_predict<- predict (maxent_ves, variable_clim_crop)

##EXport to TIFF
tiff (filename="outputs/antennatus.tiff", width=5000, height=5000, compression="lzw", res= 800)
plot (maxent_ant_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)

plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/germanicu.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (maxent_ger_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)

plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/sepultor.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (maxent_sep_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)

plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/vestigator.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (maxent_ves_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)
plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()


#reclasification reclasification (based on maximum training sensitivityplus specificity logistic treshold)
reclas <- c(maxent_ant@results[72], maxent_ger@results[72],maxent_sep@results[72],maxent_ves@results[72]) # maximum training specificity and sensitivity log. tresh


am = c(reclas[1],1,1,0,reclas[1],0)
arclmat = matrix (am,ncol=3,byrow=TRUE)
ant_reclas<- reclassify (maxent_ant_predict, arclmat)

gm = c(reclas[2],1,1,0,reclas[2],0)
grclmat = matrix (gm,ncol=3,byrow=TRUE)
ger_reclas<- reclassify (maxent_ger_predict, grclmat)


sm = c(reclas[3],1,1,0,reclas[3],0)
srclmat = matrix (am,ncol=3,byrow=TRUE)
sep_reclas<- reclassify (maxent_sep_predict, srclmat)


vm = c(reclas[4],1,1,0,reclas[4],0)
vrclmat = matrix (vm,ncol=3,byrow=TRUE)
ves_reclas<- reclassify (maxent_ves_predict, vrclmat)


#map of reclas


#X11()
#plot (endangered_reclas)
#plot (wrld_simpl, add=TRUE, axes=FALSE) #not a best resolution
#map("world", interior = TRUE, xlim=c(0,80), ylim=c(20,70), add=TRUE)#this is better resolution
#map("world", boundary = FALSE, col="gray", add = TRUE) #this could make an interior 

tiff (filename="outputs/antennatus_reclas.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (ant_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)
plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/germanicus_reclas.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (ger_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)
plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/sepultor_reclas.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (sep_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)
plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()

tiff (filename="outputs/vestigator_reclas.tiff", width=5000, height=5000, 
  compression="lzw", res= 800)
plot (ves_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# plot (wrld_simpl, add=T)
plot (map, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
dev.off()


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
