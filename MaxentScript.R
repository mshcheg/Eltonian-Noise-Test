options(java.parameters = "-Xmx8g" )
Sys.setenv(NOAWT=TRUE)
library(raster)
library(dismo)
library(rgdal)
library(maptools)
library(rJava)
library(SDMTools)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Parrot Species Dictionary~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#1: Aae, 2: Aar, 3: Aau, 4: Ach, 5: Ahy, 6: Axa, 7: Bch, 8: Dno, 9: Fxa, 10: Oma, 11: Pma

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Plant Species Dictionary~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#1: Astronium_fraxinifolium, 2: Tabebuia_heptaphylla, 3: Tabebuia_impetiginosa, 4: Vitex_cymosa, 5: Psidium_guajava, 6: Orbignya_speciosa, 7: Vatairea_macrocarpa, 8: Buchenavia_capitata, 9: Scheelea_phalerata, 10: Caryocar_brasiliense, 11: Mimosa_claussenii
#12: Chorisia_speciosa, 13: Inga_vera, 14: Ochroma_pyramidale, 15: Protium_heptaphyllum, 16: Syagrus_romanzoffiana, 17: Melia_azedarach, 18: Syzygium_cumini, 19: Cecropia_pachystachya, 20: Ficus_guaranitica, 21: Mauritia_flexuosa         
#22: Croton_floribundus, 23: Dicella_bracteosa, 24: Pachystroma_longifolium   

#Create a list of all locality files
(LocFiles <- file.path("~/PsittaJBI/Pontos2", c("Aae.csv", "Aar.csv", "Aau.csv", "Ach.csv", "Ahy.csv", "Axa.csv", "Bch.csv", "Dno.csv", "Fxa.csv", "Oma.csv", "Pma.csv")))
#Create a list of all ocality data acess by number, sp to number correspondance above
Locs <- lapply(LocFiles, function(x) read.csv(x, h=T))

#function for stacking and cropping rasters 

StackClip <- function(directory){
	#Create list of abiotic raster files to stack
	(RasterList <- (dir(path=directory, pattern="*.bil"))
	#Concatonate path with file name
	RasterPath <- lapply(RasterList, function(x) paste(directory, x, sep =""))
	#Stack rasters
	RasterStack <-stack(RasterPath)

	#set extent for cropping rasters
	#notation -- (xmin, xmax, ymin, ymax)
	e <- extent(-78.5,-34, -34.75, 12.79167)

	#crop the stack
	RasterExtent <- crop(RasterStack, e)
	return(RasterExtent)
}

Abiotic30sec <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/30/")
Abiotic2.5arc <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/2.5/")
Abiotic5arc <-  StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/5/")
Abiotic10arc <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/10/")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Plant Species Dictionary~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#1: Astronium_fraxinifolium, 2: Tabebuia_heptaphylla, 3: Tabebuia_impetiginosa, 4: Vitex_cymosa, 5: Psidium_guajava, 6: Orbignya_speciosa, 7: Vatairea_macrocarpa, 8: Buchenavia_capitata, 9: Scheelea_phalerata, 10: Caryocar_brasiliense, 11: Mimosa_claussenii
#12: Chorisia_speciosa, 13: Inga_vera, 14: Ochroma_pyramidale, 15: Protium_heptaphyllum, 16: Syagrus_romanzoffiana, 17: Melia_azedarach, 18: Syzygium_cumini, 19: Cecropia_pachystachya, 20: Ficus_guaranitica, 21: Mauritia_flexuosa         
#22: Croton_floribundus, 23: Dicella_bracteosa, 24: Pachystroma_longifolium   

#Create list of plant model files
(PlantFiles2.5 <- file.path("~/PsittaJBI/Bioticas Logistic 2.5 arc/", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 
(PlantFiles5 <- file.path("~/PsittaJBI/Bioticas Logistic 5 arc/", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 
(PlantFiles10 <- file.path("~/PsittaJBI/Bioticas Logistic 10 arc/", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 
(PlantFiles30 <- file.path("~/PsittaJBI/Bioticas Logistic 30 sec/", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 

aae.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[1]], PlantFiles10[[2]], PlantFiles10[[3]], PlantFiles10[[4]])
aar.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[5]])
aau.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[6]], PlantFiles10[[7]])
ach.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[8]])
ahy.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[9]])
axa.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[10]], PlantFiles10[[11]])
bch.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[12]], PlantFiles10[[13]], PlantFiles10[[14]], PlantFiles10[[15]], PlantFiles10[[16]])
dno.pred2.5sec <- stack(Abiotic10arc, PlantFiles10[[17]], PlantFiles10[[18]])
fxa.pred10arc <- stack(Abiotic10arc, PlantFiles10[[19]], PlantFiles10[[20]])
oma.pred10arc <- stack(Abiotic10arc, PlantFiles10[[21]])
pma.pred10arc <- stack(Abiotic10arc, PlantFiles10[[22]], PlantFiles10[[23]], PlantFiles10[[24]])

aae.pred10arc <- stack(Abiotic10arc, PlantFiles10[[1]], PlantFiles10[[2]], PlantFiles10[[3]], PlantFiles10[[4]])
aar.pred10arc <- stack(Abiotic10arc, PlantFiles10[[5]])
aau.pred10arc <- stack(Abiotic10arc, PlantFiles10[[6]], PlantFiles10[[7]])
ach.pred10arc <- stack(Abiotic10arc, PlantFiles10[[8]])
ahy.pred10arc <- stack(Abiotic10arc, PlantFiles10[[9]])
axa.pred10arc <- stack(Abiotic10arc, PlantFiles10[[10]], PlantFiles10[[11]])
bch.pred10arc <- stack(Abiotic10arc, PlantFiles10[[12]], PlantFiles10[[13]], PlantFiles10[[14]], PlantFiles10[[15]], PlantFiles10[[16]])
dno.pred10arc <- stack(Abiotic10arc, PlantFiles10[[17]], PlantFiles10[[18]])
fxa.pred10arc <- stack(Abiotic10arc, PlantFiles10[[19]], PlantFiles10[[20]])
oma.pred10arc <- stack(Abiotic10arc, PlantFiles10[[21]])
pma.pred10arc <- stack(Abiotic10arc, PlantFiles10[[22]], PlantFiles10[[23]], PlantFiles10[[24]])

aae.pred30arc <- stack(Abiotic30arc, PlantFiles30[[1]], PlantFiles30[[2]], PlantFiles30[[3]], PlantFiles30[[4]])
aar.pred30arc <- stack(Abiotic30arc, PlantFiles30[[5]])
aau.pred30arc <- stack(Abiotic30arc, PlantFiles30[[6]], PlantFiles30[[7]])
ach.pred30arc <- stack(Abiotic30arc, PlantFiles30[[8]])
ahy.pred30arc <- stack(Abiotic30arc, PlantFiles30[[9]])
axa.pred30arc <- stack(Abiotic30arc, PlantFiles30[[10]], PlantFiles30[[11]])
bch.pred30arc <- stack(Abiotic30arc, PlantFiles30[[12]], PlantFiles30[[13]], PlantFiles30[[14]], PlantFiles30[[15]], PlantFiles30[[16]])
dno.pred30arc <- stack(Abiotic30arc, PlantFiles30[[17]], PlantFiles30[[18]])
fxa.pred30arc <- stack(Abiotic30arc, PlantFiles30[[19]], PlantFiles30[[20]])
oma.pred30arc <- stack(Abiotic30arc, PlantFiles30[[21]])
pma.pred30arc <- stack(Abiotic30arc, PlantFiles30[[22]], PlantFiles30[[23]], PlantFiles30[[24]])



#run maxent models 
for (i in 1:11){
	SpName <- Locs[[i]]$Species[1]
	MaxentModel <- maxent(Abiotic30sec, Locs[[i]][,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste("/home/mshcheglovitova/PsittaJBI/MaxentModels_30sec_Abiotic/", SpName, sep=""))
#Write out 100 rasters??
#Save maxent out
  r <- predict(me,predictors10arc)
  writeRaster(r,filename=paste(getwd(),"/Bioticas Logistic 10 arc/", splist[i], ".grd", sep=""))
  png(file=paste(getwd(),"/Modelos Plantas 10 arc/", splist[i], "/", splist[i], ".png", sep=""))
  plot(r,main=splist[i])
  points(sp.occ, col="red", cex=1, pch=16)
  dev.off()




~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Biotic Models~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################Abiotic and Biotic analysis################################################
###################################Defining each species predictors 10arc###########################################
###################################Open all Biotic Logistic 10 arc#################################################
Astronium_fraxinifolium <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Astronium_fraxinifolium.grd")
Tabebuia_heptaphylla <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Tabebuia_heptaphylla.grd")
Tabebuia_impetiginosa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Tabebuia_impetiginosa.grd")
Vitex_cymosa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Vitex_cymosa.grd")
Psidium_guajava <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Psidium_guajava.grd")
Orbignya_speciosa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Orbignya_speciosa.grd")
Vatairea_macrocarpa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Vatairea_macrocarpa.grd")
Buchenavia_capitata <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Buchenavia_capitata.grd")
Scheelea_phalerata <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Scheelea_phalerata.grd")
Caryocar_brasiliense <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Caryocar_brasiliense.grd")
Mimosa_claussenii <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Mimosa_claussenii.grd")
Chorisia_speciosa<- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Chorisia_speciosa.grd")
Inga_vera <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Inga_vera.grd")
Ochroma_pyramidale <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Ochroma_pyramidale.grd")
Protium_heptaphyllum <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Protium_heptaphyllum.grd")
Syagrus_romanzoffiana <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Syagrus_romanzoffiana.grd")
Melia_azedarach <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Melia_azedarach.grd")
Syzygium_cumini <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Syzygium_cumini.grd")
Cecropia_pachystachya <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Cecropia_pachystachya.grd")
Ficus_guaranitica <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Ficus_guaranitica.grd")
Mauritia_flexuosa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Mauritia_flexuosa.grd")
Croton_floribundus <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Croton_floribundus.grd")
Dicella_bracteosa <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Dicella_bracteosa.grd")
Pachystroma_longifolium <- raster("~/Documents/Manuscripts/Psitta JBI/Bioticas Logistic 10 arc/Pachystroma_longifolium.grd")

names(Astronium_fraxinifolium) <- c('Astronium_fraxinifolium')
names(Tabebuia_heptaphylla) <- c('Tabebuia_heptaphylla')
names(Tabebuia_impetiginosa) <- c('Tabebuia_impetiginosa')
names(Vitex_cymosa) <- c('Vitex_cymosa')
names(Psidium_guajava) <- c('Psidium_guajava')
names(Orbignya_speciosa) <- c('Orbignya_speciosa')
names(Vatairea_macrocarpa) <- c('Vatairea_macrocarpa')
names(Buchenavia_capitata) <- c('Buchenavia_capitata')
names(Scheelea_phalerata) <- c('Scheelea_phalerata')
names(Caryocar_brasiliense) <- c('Caryocar_brasiliense')
names(Mimosa_claussenii) <- c('Mimosa_claussenii')
names(Chorisia_speciosa) <- c('Chorisia_speciosa')
names(Inga_vera) <- c('Inga_vera')
names(Ochroma_pyramidale) <- c('Ochroma_pyramidale')
names(Protium_heptaphyllum) <- c('Protium_heptaphyllum')
names(Syagrus_romanzoffiana) <- c('Syagrus_romanzoffiana')
names(Melia_azedarach) <- c('Melia_azedarach')
names(Syzygium_cumini) <- c('Syzygium_cumini')
names(Cecropia_pachystachya) <- c('Cecropia_pachystachya')
names(Ficus_guaranitica) <- c('Ficus_guaranitica')
names(Mauritia_flexuosa) <- c('Mauritia_flexuosa')
names(Croton_floribundus) <- c('Croton_floribundus')
names(Dicella_bracteosa) <- c('Dicella_bracteosa')
names(Pachystroma_longifolium) <- c('Pachystroma_longifolium')

##################################################Predictors for analysis 10 arc###########################
aae.pred10arc <- stack(predictors10arc, Astronium_fraxinifolium, Tabebuia_heptaphylla, Tabebuia_impetiginosa, Vitex_cymosa)
aar.pred10arc <- stack(predictors10arc, Psidium_guajava)
aau.pred10arc <- stack(predictors10arc, Orbignya_speciosa, Vatairea_macrocarpa)
ach.pred10arc <- stack(predictors10arc, Buchenavia_capitata)
ahy.pred10arc <- stack(predictors10arc, Scheelea_phalerata)
axa.pred10arc <- stack(predictors10arc, Caryocar_brasiliense, Mimosa_claussenii)
bch.pred10arc <- stack(predictors10arc, Chorisia_speciosa, Inga_vera, Ochroma_pyramidale, Protium_heptaphyllum, Syagrus_romanzoffiana)
dno.pred10arc <- stack(predictors10arc, Melia_azedarach, Syzygium_cumini)
fxa.pred10arc <- stack(predictors10arc, Cecropia_pachystachya, Ficus_guaranitica)
oma.pred10arc <- stack(predictors10arc, Mauritia_flexuosa)
pma.pred10arc <- stack(predictors10arc, Croton_floribundus, Dicella_bracteosa, Pachystroma_longifolium)

################################Run models with abiotic and Biotic 10arc##################################
me.aae10arc <- maxent(aae.pred10arc, aae[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/aae", sep=""))
me.aar10arc <- maxent(aar.pred10arc, aar[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/aar", sep=""))
me.aau10arc <- maxent(aau.pred10arc, aau[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arcc/aau", sep=""))
me.ach10arc <- maxent(ach.pred10arc, ach[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/ach", sep=""))
me.ahy10arc <- maxent(ahy.pred10arc, ahy[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/ahy", sep=""))
me.axa10arc <- maxent(axa.pred10arc, axa[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/axa", sep=""))
me.bch10arc <- maxent(bch.pred10arc, bch[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/bch", sep=""))
me.dno10arc <- maxent(dno.pred10arc, dno[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/dno", sep=""))
me.fxa10arc <- maxent(fxa.pred10arc, fxa[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/fxa", sep=""))
me.oma10arc <- maxent(oma.pred10arc, oma[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/oma", sep=""))
me.pma10arc <- maxent(pma.pred10arc, pma[,2:3], args=c("randomtestpoints=25", "replicates=100", "replicatetype=bootstrap", "randomseed"), path=paste(getwd(),"/Abiotic and Biotic Results/10arc/pma", sep=""))
