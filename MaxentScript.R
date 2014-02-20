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

#Create a list of all locality files
(LocFiles <- file.path("~/PsittaJBI/Pontos2", c("Aae.csv", "Aar.csv", "Aau.csv", "Ach.csv", "Ahy.csv", "Axa.csv", "Bch.csv", "Dno.csv", "Fxa.csv", "Oma.csv", "Pma.csv")))
#Create a list of all ocality data acess by number, sp to number correspondance above
Locs <- lapply(LocFiles, function(x) read.csv(x, h=T))

#function for stacking and cropping rasters 
StackClip <- function(directory){
	#Create list of abiotic raster files to stack
	(RasterList <- (dir(path=directory, pattern="*.bil")))
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

Abiotic30 <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/30/")
Abiotic2.5 <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/2.5/")
Abiotic5 <-  StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/5/")
Abiotic10 <- StackClip("/home/mshcheglovitova/PsittaJBI/AbioticVariables/10/")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Plant Species Dictionary~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#1: Astronium_fraxinifolium, 2: Tabebuia_heptaphylla, 3: Tabebuia_impetiginosa, 4: Vitex_cymosa, 5: Psidium_guajava, 6: Orbignya_speciosa, 7: Vatairea_macrocarpa, 8: Buchenavia_capitata, 9: Scheelea_phalerata, 10: Caryocar_brasiliense, 11: Mimosa_claussenii
#12: Chorisia_speciosa, 13: Inga_vera, 14: Ochroma_pyramidale, 15: Protium_heptaphyllum, 16: Syagrus_romanzoffiana, 17: Melia_azedarach, 18: Syzygium_cumini, 19: Cecropia_pachystachya, 20: Ficus_guaranitica, 21: Mauritia_flexuosa         
#22: Croton_floribundus, 23: Dicella_bracteosa, 24: Pachystroma_longifolium   

#Create list of plant model files
(PlantFiles2.5 <- file.path("~/PsittaJBI/Bioticas Logistic 2.5 arc2", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 

#The 5 arc plant models do not have the same extent as the other models, the 5 arc abiotic predictors need to be clipped
(PlantFiles5 <- file.path("~/PsittaJBI/Bioticas Logistic 5 arc2", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 

#The 10 arc plant models do not have the same extent as the other models, the 10 arc abiotic predictors need to be clipped
(PlantFiles10 <- file.path("~/PsittaJBI/Bioticas Logistic 10 arc2", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 


#Plant grids were generated in MaxEnt for 30 arc was getting error "Error in if (prj == "GEOGRAPHIC") { : missing value where TRUE/FALSE needed" reading in as raster. Reran models through R and put in Bioticas Logistic 30 sec2 
(PlantFiles30 <- file.path("~/PsittaJBI/Bioticas Logistic 30 sec2", c("Astronium_fraxinifolium.grd", "Tabebuia_heptaphylla.grd", "Tabebuia_impetiginosa.grd", "Vitex_cymosa.grd", "Psidium_guajava.grd", "Orbignya_speciosa.grd", "Vatairea_macrocarpa.grd", "Buchenavia_capitata.grd", "Scheelea_phalerata.grd", "Caryocar_brasiliense.grd", "Mimosa_claussenii.grd", "Chorisia_speciosa.grd", "Inga_vera.grd", "Ochroma_pyramidale.grd", "Protium_heptaphyllum.grd", "Syagrus_romanzoffiana.grd", "Melia_azedarach.grd", "Syzygium_cumini.grd", "Cecropia_pachystachya.grd", "Ficus_guaranitica.grd", "Mauritia_flexuosa.grd", "Croton_floribundus.grd", "Dicella_bracteosa.grd", "Pachystroma_longifolium.grd"))) 

Aae.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[1]], PlantFiles2.5[[2]], PlantFiles2.5[[3]], PlantFiles2.5[[4]])
Aar.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[5]])
Aau.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[6]], PlantFiles2.5[[7]])
Ach.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[8]])
Ahy.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[9]])
Axa.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[10]], PlantFiles2.5[[11]])
Bch.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[12]], PlantFiles2.5[[13]], PlantFiles2.5[[14]], PlantFiles2.5[[15]], PlantFiles2.5[[16]])
Dno.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[17]], PlantFiles2.5[[18]])
Fxa.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[19]], PlantFiles2.5[[20]])
Oma.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[21]])
Pma.AB2.5 <- stack(Abiotic2.5, PlantFiles2.5[[22]], PlantFiles2.5[[23]], PlantFiles2.5[[24]])

Aae.AB5 <- stack(Abiotic5, PlantFiles5[[1]], PlantFiles5[[2]], PlantFiles5[[3]], PlantFiles5[[4]])
Aar.AB5 <- stack(Abiotic5, PlantFiles5[[5]])
Aau.AB5 <- stack(Abiotic5, PlantFiles5[[6]], PlantFiles5[[7]])
Ach.AB5 <- stack(Abiotic5, PlantFiles5[[8]])
Ahy.AB5 <- stack(Abiotic5, PlantFiles5[[9]])
Axa.AB5 <- stack(Abiotic5, PlantFiles5[[10]], PlantFiles5[[11]])
Bch.AB5 <- stack(Abiotic5, PlantFiles5[[12]], PlantFiles5[[13]], PlantFiles5[[14]], PlantFiles5[[15]], PlantFiles5[[16]])
Dno.AB5 <- stack(Abiotic5, PlantFiles5[[17]], PlantFiles5[[18]])
Fxa.AB5 <- stack(Abiotic5, PlantFiles5[[19]], PlantFiles5[[20]])
Oma.AB5 <- stack(Abiotic5, PlantFiles5[[21]])
Pma.AB5 <- stack(Abiotic5, PlantFiles5[[22]], PlantFiles5[[23]], PlantFiles5[[24]])

Aae.AB10 <- stack(Abiotic10, PlantFiles10[[1]], PlantFiles10[[2]], PlantFiles10[[3]], PlantFiles10[[4]])
Aar.AB10 <- stack(Abiotic10, PlantFiles10[[5]])
Aau.AB10 <- stack(Abiotic10, PlantFiles10[[6]], PlantFiles10[[7]])
Ach.AB10 <- stack(Abiotic10, PlantFiles10[[8]])
Ahy.AB10 <- stack(Abiotic10, PlantFiles10[[9]])
Axa.AB10 <- stack(Abiotic10, PlantFiles10[[10]], PlantFiles10[[11]])
Bch.AB10 <- stack(Abiotic10, PlantFiles10[[12]], PlantFiles10[[13]], PlantFiles10[[14]], PlantFiles10[[15]], PlantFiles10[[16]])
Dno.AB10 <- stack(Abiotic10, PlantFiles10[[17]], PlantFiles10[[18]])
Fxa.AB10 <- stack(Abiotic10, PlantFiles10[[19]], PlantFiles10[[20]])
Oma.AB10 <- stack(Abiotic10, PlantFiles10[[21]])
Pma.AB10 <- stack(Abiotic10, PlantFiles10[[22]], PlantFiles10[[23]], PlantFiles10[[24]])

Aae.AB30 <- stack(Abiotic30, PlantFiles30[[1]], PlantFiles30[[2]], PlantFiles30[[3]], PlantFiles30[[4]])
Aar.AB30 <- stack(Abiotic30, PlantFiles30[[5]])
Aau.AB30 <- stack(Abiotic30, PlantFiles30[[6]], PlantFiles30[[7]])
Ach.AB30 <- stack(Abiotic30, PlantFiles30[[8]])
Ahy.AB30 <- stack(Abiotic30, PlantFiles30[[9]])
Axa.AB30 <- stack(Abiotic30, PlantFiles30[[10]], PlantFiles30[[11]])
Bch.AB30 <- stack(Abiotic30, PlantFiles30[[12]], PlantFiles30[[13]], PlantFiles30[[14]], PlantFiles30[[15]], PlantFiles30[[16]])
Dno.AB30 <- stack(Abiotic30, PlantFiles30[[17]], PlantFiles30[[18]])
Fxa.AB30 <- stack(Abiotic30, PlantFiles30[[19]], PlantFiles30[[20]])
Oma.AB30 <- stack(Abiotic30, PlantFiles30[[21]])
Pma.AB30 <- stack(Abiotic30, PlantFiles30[[22]], PlantFiles30[[23]], PlantFiles30[[24]])

MaxEntAnalysis <- function(predictor, locs, species, directory){
	print(species)
	#Run Maxent Model -- Remember to change settings!	
	MaxentModel <- maxent(predictor, locs, args=c("randomtestpoints=25", "randomseed"), path=directory)
	r <- predict(MaxentModel,predictor)
	writeRaster(r,filename=paste(directory, ".grd", sep=""), overwrite=T)
	pdf(file=paste(directory, ".pdf", sep=""))
	plot(r)
	points(locs, col="red", cex=1, pch=16)
	dev.off()
	#Normalize raster 
	sum <- cellStats(r, stat="sum")
	r.norm <- calc(r, fun=function(x){x/sum})

	return(r.norm)
}

I.Values <- data.frame(Species=NA, Resolution=NA, I=NA)

WorkingDir <- "/home/mshcheglovitova/PsittaJBI/MS_R_Tests"
dir.create(file.path(WorkingDir), showWarnings = FALSE)
setwd(file.path(WorkingDir))

#run maxent models 
k <- 1
for (i in 1:11){
	SpName <- Locs[[i]]$Species[1]
	print(paste("Beginning to process", SpName, sep=" ")) 
	for (j in c('2.5', '5', '10', '30')){
		AB.Pred <- paste(SpName,".AB",j, sep="")
		A.Pred <- paste("Abiotic",j, sep="")
		print(paste("On resolution", j, sep=" "))
		#Run Abiotic model
		norm.A <- MaxEntAnalysis(eval(parse(text=A.Pred)), Locs[[i]][,2:3], SpName, paste("MaxentModels",j,"_Abiotic/", SpName, sep="")) 	
		#Run Abiotic + Biotic model
		norm.AB <- MaxEntAnalysis(eval(parse(text=AB.Pred)), Locs[[i]][,2:3], SpName, paste("MaxentModels",j,"_AbioticBiotic/", SpName, sep=""))
		
		#Calculate Difference Grid
		r.diff <- norm.A - norm.AB

		#Plot difference grid
		myDir <- paste("MaxentModels",j,"_Comparison/", sep="")		
		dir.create(file.path(myDir), showWarnings = FALSE)		
		pdf(file=paste(myDir, SpName, ".pdf", sep=""), width=11, height=8)
		par(mfrow=c(1,3))
		plot(norm.A, main="Normalized Abiotic", legend=F)
		plot(norm.AB, main="Normalized Abiotic and Biotic", legend=F)
		plot(r.diff, main="Normalized Difference: Abiotic - Abiotic and Biotic", legend=T)
		dev.off()
		
		#Calculate I values 
		IValue <- Istat(norm.A, norm.AB)	

		I.Values[k,] <- c(paste(SpName), j, IValue)
		k <- k+1		
	} 	
    print(paste("Finished processing", SpName, sep=" "))
}

write.csv(I.Values, file="IValues.csv")

