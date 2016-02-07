rm(list = ls())
# install.packages (c("rgbif", "raster ", "maptools", "XML", "rgdal", "dismo",
#                  "sqldf", "maps ", "testthat","roxygen2",
#                  "celestial", "ggplot2", "rJava"))
# install.packages("rasterVis")
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
# install.packages("rJava")
library (rJava)
library (celestial)
library (ggplot2)
library (maps)
#install.packages("spocc", dependencies = TRUE)
library (mapproj)
# install.packages("mapr", dependencies = TRUE)
library (mapr)
# install.packages("ggmap")
library(ggmap)
library(rasterVis) #ploting rasters in ggplot2
library(rgdal)
# install.packages("rworldxtra")
library(rworldmap)
library(rworldxtra)
#install.packages(c("mapproj", "maps"))
#install.packages("devtools")
#library (devtools)
# library(devtools)
# install_github("ggplot2", "kohske", "fix/geom-raster-alpha")
#=======================================================================================
data(wrld_simpl) #create the World map with borders

newmap = getMap(resolution="high")

#=======================================================================================
##Loading all datasets
coord.full = read.table ("data/coord.full.csv", header= TRUE, sep=";")
head (coord.full)
#=======================================================================================
#map of endangered Nicrophorinae occurence in Europe
map=get_map (location="Europe", zoom=4)
ggmap(map)
str(map)

# tiff (filename="outputs/Nicrophoriane_occurence.tiff", 
#       width=2000, height=2000, 
#       compression="lzw", res= 300)

# nicroph.occur=  ggmap(map)+
#   geom_point (aes (x = endang_nicro$long, 
#                    y = endang_nicro$lat, colour=endang_nicro$spec ), data = endang_nicro)+
#   xlab("")+
#   ylab("")
# 
# plot (coord.full$long,coord.full$lat)
# plot (wrld_simpl, add=T)

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

ext <-  extent (-15, 55, 25, 75)
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

#===================================================================================
# #exract variables from coords of each species
# clim_var.ant <- extract (variable_clim_crop, coord.antennatus)
# clim_var.ger <- extract (variable_clim_crop, coord.germanicus)
# clim_var.sep <- extract (variable_clim_crop, coord.sepultor)
# clim_var.ves <- extract (variable_clim_crop, coord.vestigator)
# 
# clim_var.ant<- as.data.frame (clim_var.ant)
# 
# #sampling 
# coord.antennatus.sample <- envSample (coord.antennatus, filters=list(clim_var.ant$bio1,clim_var.ant$bio12), res=list(10,100))
# 
# maxent_ant_sampl <- maxent (variable_clim_crop, coord.antennatus.sample, args=c("maximumbackground=1000",
#   "betamultiplier=1",
#   "defaultprevalence=0.5"))
# 
# maxent_ant_sampl@results[5]
# 
# maxent_ant_predict<- predict (maxent_ant_sampl, variable_clim_crop)
# 
# reclas <- c(maxent_ant_sampl@results[72]) # maximum training specificity and sensitivity log. tresh
# 
# am = c(reclas[1],1,1,0,reclas[1],0)
# arclmat = matrix (am,ncol=3,byrow=TRUE)
# ant_reclas<- reclassify (maxent_ant_predict, arclmat)
# 
# tiff (filename="outputs/antennatus_reclas_sampl.tiff", width=5000, height=5100, 
#   compression="lzw", res= 800)
# plot (ant_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.antennatus.sample$lon, coord.antennatus.sample$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=20, col="red", cex=.6 )
# plot (wrld_simpl, add=T)
# dev.off()
#===================================================================================

#Project niches of target species by extracting values from raster and ploting them
# niche <- extract (variable_clim_crop, coord.antennatus)
# niche <- as.data.frame (niche)
# X11()
# par (mfrow=c(1,2))
# str(niche)
# plot (niche)
# plot (niche$bio1, niche$bio12, xlab= "prectip of warmest qrt" 
#       , ylab= "temp warmest qurt" )
# plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" ,
#       ylab= "temp of wettest qrt" )
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
  "betamultiplier=2",
  "defaultprevalence=0.5"))
maxent_ves <- maxent (variable_clim_crop, coord.vestigator, args=c("maximumbackground=1000",
  "betamultiplier=2",
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
# par (mfrow=c(2,2))
response (maxent_ant)
response (maxent_ger)
response (maxent_sep)
response (maxent_ves)

# response (maxent_all2)
# response (maxent_all5)


#just AUC
maxent_ant@results[5]
maxent_ger@results[5]
maxent_sep@results[5]
maxent_ves@results[5]

#Predict probability of occurence
maxent_ant_predict<- predict (maxent_ant, variable_clim_crop)
maxent_ger_predict<- predict (maxent_ger, variable_clim_crop)
maxent_sep_predict<- predict (maxent_sep, variable_clim_crop)
maxent_ves_predict<- predict (maxent_ves, variable_clim_crop)

##EXport to TIFF
# tiff (filename="outputs/antennatus.tiff", width=5000, height=5000, compression="lzw", res= 800)
# X11()
# 
# plot (maxent_ger_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.antennatus$long, coord.antennatus$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=20,add=T)
# plot (wrld_simpl, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), add=T)
# 
# dev.off()

# 
# tiff (filename="outputs/germanicu.tiff", width=5000, height=5000, 
#   compression="lzw", res= 800)
# plot (maxent_ger_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# # plot (wrld_simpl, add=T)
# dev.off()
# 
# tiff (filename="outputs/sepultor.tiff", width=5000, height=5000, 
#   compression="lzw", res= 800)
# plot (maxent_sep_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# # plot (wrld_simpl, add=T)
# dev.off()
# 
# tiff (filename="outputs/vestigator.tiff", width=5000, height=5000, 
#   compression="lzw", res= 800)
# plot (maxent_ves_predict, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# # plot (wrld_simpl, add=T)
# dev.off()


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

#====================================================================================
# #map of reclas
# plot(ant_reclas)
# r.spdf <- as(ant_reclas, "SpatialPixelsDataFrame")
# r.df <- as.data.frame(r.spdf)
# head(r.df)
# 
# # then you can use ggplot2 to plot that object
# 
# 
# 
# g <- ggplot (r.df, aes(x=x, y=y)) + geom_tile(aes(fill = layer)) + coord_equal()
# print(g)
# 
# map=get_map (location="Europe", zoom=4)
# europe= qmap (location="Europe", zoom=4)
# ggmap(map)
# gglocator(2)
# 
# HoustonMap <- ggmap(map,
#   base_layer = ggplot (r.df, aes(x=x, y=y)))
# 
# HoustonMap+ggplot(r.df,aes(x=x, y=y)) + geom_tile(aes(fill = layer)) + coord_equal()
# 
# ant.occur = qmap (map,
#   base_layer = g)
# 
# ant.occur= map + geom_tile(aes(fill = layer)) + coord_equal()
# 
# points.ant=geom_point (aes (x = long, y = lat), data = coord.antennatus)
# 
# p=map+ant.occur+points.ant
#   annotation_raster(map,
#     xmin=ext[1], xmax=ext[2], ymin=ext[3], ymax=ext[4])+
#   xlab("")+
#   ylab("")
#====================================================================================
#X11()
#plot (endangered_reclas)
#plot (wrld_simpl, add=TRUE, axes=FALSE) #not a best resolution
# plot (coord.full$long, coord.full$lat)
# map("world", interior = TRUE, xlim=c(0,80), ylim=c(20,70), add=TRUE)#this is better resolution
# map("world", boundary = FALSE, col="gray", add = TRUE) #this could make an interior #====================================================================================
pdf (file="outputs/antennatus_reclas.pdf")
plot (ant_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
points (coord.antennatus$long, coord.antennatus$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", bg="red", cex=.6 )
dev.off()

pdf (file="outputs/germanicus_reclas.pdf")
plot (ger_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
points (coord.germanicus$long, coord.germanicus$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", bg="red", cex=.6 )
dev.off()

pdf (file="outputs/sepultor_reclas.pdf")
plot (sep_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
points (coord.sepultor$long, coord.sepultor$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", bg="red", cex=.6 )
dev.off()

pdf (file="outputs/vestigator_reclas.pdf")
plot (ves_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
points (coord.vestigator$long, coord.vestigator$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", bg="red", cex=.6 )
dev.off()

#====================================================================================

# tiff (filename="outputs/antennatus_reclas1.tiff", width=5000, height=5100, 
#   compression="lzw", res= 800)
# plot (ant_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.antennatus$long, coord.antennatus$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", bg="red", cex=.6 )
# plot (newmap, add=T)
# dev.off()
# 
# tiff (filename="outputs/germanicus_reclas.tiff", width=5000, height=5100, 
#   compression="lzw", res= 800)
# plot (ger_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.germanicus$long, coord.germanicus$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=21, col="black", cex=.6)
# plot (newmap, add=T)
# dev.off()
# 
# tiff (filename="outputs/sepultor_reclas.tiff", width=5000, height=5100, 
#   compression="lzw", res= 800)
# plot (sep_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.sepultor$long, coord.sepultor$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=20, col="red", cex=.6)
# plot (newmap, add=T)
# dev.off()
# 
# tiff (filename="outputs/vestigator_reclas.tiff", width=5000, height=5100, 
#   compression="lzw", res= 800)
# plot (ves_reclas, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
# points (coord.vestigator$long, coord.vestigator$lat, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]), pch=20, col="red", cex=.6)
# plot (newmap, add=T)
# dev.off()


#of europe be with gray boarders


# #EVALUATION OF THE MAXENT MODEL
# #crete object with random split of the data into k(5) subsamples by kfold
# fold <- kfold(coord,k=5)
# #Create training subdataset of 80% of the data for modeling by selecting 
# #everything except the number one ((100%/5)*4=80%)
# occtrain <- coord[fold !=1,]
# #Create testing subdataset of 20% of the data to validate the model by selecting
# #only the number one
# occtest <- coord [fold==1,]
# 
# # MAXENT model (basic setup) for training data
# maxent_occtrain <- maxent (variable_crop, occtrain, args=c("maximumbackground=1000",
#                                                            "betamultiplier=5",
#                                                            "defaultprevalence=0.5"))
# #Prediction for training data
# maxent_occtrain_predict <- predict (maxent_occtrain, variable_crop )
# 
# #PLotting training subdataset vs. whole dataset
# x11()
# plot (maxent_all_predict, main="Sciodrepoides watsoni distribution (Maxent/all)")
# plot (wrld_simpl, add=TRUE)
# x11()
# plot (maxent_occtrain_predict, main="Sciodrepoides watsoni distribution (Maxent/training)")
# plot (wrld_simpl, add=TRUE)
# 
# #what is discriminant value (AUC)
# maxent_occtrain@results[5]
# #what is maximum training sensitivityplus specificity logistic treshold 
# #aka where to split
# maxent_occtrain@results
# #what is dimmension of my data to enter right number of random points
# dim(occtest)
# 
# #testing on pseudoabsences (number 100 is estimated based on the dim value)
# pseudoabsence <- randomPoints (variable_crop, 650)
# x11()
# plot (coord.neg)
# plot (wrld_simpl,add=T)
# points (coord, col="red")
# 
# #evaluation compare the value of AUC 
# #from this evaluate_nicveo with value of training set 
# #evaluate will compere with random pseudoabsence set and 
# #come with new AUC = compare it with maxent_training_nicveo@results[5]
# 
# evaluate_maxent_occtrain <- evaluate (maxent_occtrain,
#                                       p=occtest, a=pseudoabsence,
#                                       x=variable_crop)
# 
# #Evaluate AUC between these two models:
# evaluate_maxent_occtrain
# maxent_occtrain@results[5]
# 
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
# #DONE
#===================================================================================
#predicting in the past by function predict (every prediction should be run on the current 
#data and then the prediction for the future or the past - use the same mathematical model for both)

in_dir ("D://Zaloha_notebook/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/CCSM_21", variable_past<- stack (c("bio1.bil", 
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

variable_glac_crop<- crop (variable_past, ext)
#===================================================================================
#predict glac occurence
maxent_ant_predict_glac<- predict (maxent_ant, variable_glac_crop)
maxent_ger_predict_glac<- predict (maxent_ger, variable_glac_crop)
maxent_sep_predict_glac<- predict (maxent_sep, variable_glac_crop)
maxent_ves_predict_glac<- predict (maxent_ves, variable_glac_crop)
#===================================================================================
reclas <- c(maxent_ant@results[72], maxent_ger@results[72],maxent_sep@results[72],maxent_ves@results[72]) # maximum training specificity and sensitivity log. tresh


am = c(reclas[1],1,1,0,reclas[1],0)
arclmat = matrix (am,ncol=3,byrow=TRUE)
ant_reclas_glac<- reclassify (maxent_ant_predict_glac, arclmat)

gm = c(reclas[2],1,1,0,reclas[2],0)
grclmat = matrix (gm,ncol=3,byrow=TRUE)
ger_reclas_glac<- reclassify (maxent_ger_predict_glac, grclmat)


sm = c(reclas[3],1,1,0,reclas[3],0)
srclmat = matrix (am,ncol=3,byrow=TRUE)
sep_reclas_glac<- reclassify (maxent_sep_predict_glac, srclmat)


vm = c(reclas[4],1,1,0,reclas[4],0)
vrclmat = matrix (vm,ncol=3,byrow=TRUE)
ves_reclas_glac<- reclassify (maxent_ves_predict_glac, vrclmat)

#====================================================================================
pdf (file="outputs/antennatus_reclas_glac.pdf")
plot (ant_reclas_glac, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
dev.off()

pdf (file="outputs/germanicus_reclas_glac.pdf")
plot (ger_reclas_glac, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
dev.off()

pdf (file="outputs/sepultor_reclas_glac.pdf")
plot (sep_reclas_glac, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
dev.off()

pdf (file="outputs/vestigator_reclas_glac.pdf")
plot (ves_reclas_glac, legend=F, xlim =c(ext[1],ext[2]),ylim=c(ext[3],ext[4]))
plot (newmap, add=T)
dev.off()


#other maps of climate models (ECOclim - past layers )
#future MIROC_21 ()
setwd ("C:/Users/pavel/Downloads/Spatial_modeling/ENM_2015_Varela/climatic_layers/MIROC_21/")
variable22<- stack (c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil"))
fut_crop <- crop (variable22, e)
nic_fut <- predict (maxent_occtrain, fut_crop)
x11()
plot (nic_fut, main="Sciodrepoides watsoni MIROC_21 (Maxent/training)")
plot (wrld_simpl,add=T)


