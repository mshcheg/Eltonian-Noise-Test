options(java.parameters = "-Xmx8g" )
Sys.setenv(NOAWT=TRUE)
library(raster)
library(dismo)
library(rgdal)
library(maptools)
library(rJava)
library(SDMTools)


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

occ.sps <- list.files(paste("~/PsittaJBI/Pontos Plantas/",sep=""),pattern="csv")
splist <-unlist(lapply(occ.sps, FUN = strsplit, split=("\\.csv")))

i=1

for (i in 1:length(occ.sps)){
  sp.file <- read.csv(paste("~/PsittaJBI/Pontos Plantas/", occ.sps[i],sep=""),h=T)
  sp.occ <- sp.file
  me <- maxent(Abiotic30, sp.occ, path=paste(getwd(),"/Modelos Plantas 30 sec/", splist[i], sep=""))
  r <- predict(me,Abiotic30)
  writeRaster(r,filename=paste("~/PsittaJBI/Bioticas Logistic 30 sec2/", splist[i], ".grd", sep=""), overwrite=T)
}
