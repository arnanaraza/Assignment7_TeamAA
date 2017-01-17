##Assignment Lesson 7
##Arias, Francisco ; Araza Arnan
##January 17, 2017

#Import Libraries
library(sp)
library(rgdal)
library(rgeos)
library(raster)

#Call functions
source('R/GetData.R')
source('R/PreProcessing.R')

#Create data folder,  unpack zip file, and select raster
RasterFile <- file.source("https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/MODIS.zip", "data", "MODIS.zip")

#Preprocess raster (brick, check negative)
MODISData <- PreProcessRaster(RasterFile)

#Download country of interest
NLAdmin <- MyCountry ("NLD", 2)

#Reproject and get rid of NA rows from administrative map
NLUTM <- spTransform(NLAdmin, CRS(proj4string(MODData)))
NLUTM@data <- NLUTM@data [!is.na(NLUTM$NAME_2),]

#Mask raster data using municipal boundary
MODISNL <- mask(MODISData, NLUTM)

#Extract values from masked raster and rename municipality column from the data frame
JanNDVI <- extract(MODISNL[[1]], NLUTM, df=TRUE, fun=mean, na.rm=TRUE)
AugNDVI <- extract(MODISNL[[8]], NLUTM, df=TRUE, fun=mean, na.rm=TRUE)
YearNDVI <- extract(mean(MODISNL), NLUTM, df=TRUE, fun=mean, na.rm=TRUE) #mean computed for year-round NDVI
JanMuniNDVI <- cbind(NLUTM$NAME_2, JanNDVI)
AugMuniNDVI <- cbind(NLUTM$NAME_2, AugNDVI)
YearMuniNDVI <- cbind(NLUTM$NAME_2, YearNDVI)
colnames(JanMuniNDVI)[1]<- "Municipality"
colnames(AugMuniNDVI)[1]<- "Municipality"
colnames(YearMuniNDVI)[1]<- "Municipality"

#Get highest NDVI for required periods
HighestJan <- JanMuniNDVI[which(JanMuniNDVI$January == max(JanMuniNDVI$January, na.rm = TRUE)), ]
print (paste("Highest is", as.character(HighestJan$Municipality),"with NDVI of", HighestJan[3]))
HighestAug <- AugMuniNDVI[which(AugMuniNDVI$August == max(AugMuniNDVI$August, na.rm = TRUE)), ]
print (paste("Highest is", as.character(HighestAug$Municipality),"with NDVI of", HighestAug[3]))
HighestYear <- YearMuniNDVI[which(YearMuniNDVI$layer == max(YearMuniNDVI$layer, na.rm = TRUE)), ]
print (paste("Highest is", as.character(HighestYear$Municipality),"with NDVI of", HighestYear[3]))

#Join data frame to City data and rename NDVI column properly
JanMuniNDVI <- cbind(NLUTM, NDVI=JanMuniNDVI$January)
AugMuniNDVI <- cbind(NLUTM, NDVI=AugMuniNDVI$August)
YearMuniNDVI <- cbind(NLUTM, NDVI=YearMuniNDVI$layer)
names(JanMuniNDVI)[16] = "NDVI"
names(AugMuniNDVI)[16] = "NDVI"
names(YearMuniNDVI)[16] = "NDVI"

#Plot NDVI at municipality level
spplot(JanMuniNDVI, zcol="NDVI", main="NDVI per Municipality, January")
spplot(AugMuniNDVI, zcol="NDVI", main="NDVI per Municipality, August")
spplot(YearMuniNDVI, zcol="NDVI", main="NDVI per Municipality, Year-round")

#Compute for highest province and plot NDVI for January, province level
HighestProv <- JanProvNDVI[which(JanProvNDVI$NDVI == max(JanProvNDVI$NDVI, na.rm = TRUE)), ]
print (paste("Highest is", as.character(HighestProv[[1]]),"with NDVI of", HighestProv[[2]]))
JanProvNDVI <- aggregate(JanMuniNDVI, by='NAME_1', dissolve=TRUE, sums=list(list(mean, 'NDVI')))
spplot(JanProvNDVI, zcol="NDVI", main="NDVI per Province, January")

