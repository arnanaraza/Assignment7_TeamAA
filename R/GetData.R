#Function to download and unzip raster image

file.source <- function(url, dir, filename) {
  dir.create(dir) #i.e. /data
  file = paste0(dir, '/', filename) #i.e. data/railways.zip
  #dest <- file.path(dir, basename(url))
  download.file (url=url, destfile=file, method="auto", mode="wb")
  unzip (file, exdir=dir)
  RasterFile <- list.files(dir, pattern = glob2rx('*.grd'), full.names = TRUE)
  return (RasterFile)
}

#Function to download administrative boundary

MyCountry <- function(country, level) {
  raster::getData("ISO3")
  adm <- raster::getData("GADM", country=country, 
                         level=level)
  plot(adm, bg = "dodgerblue", axes=T)
  plot(adm, lwd = 10, border = "skyblue", add=T)
  plot(adm, col = "green4", add = T)
  grid()
  box()
  return (adm)
}
