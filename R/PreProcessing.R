#Function to create raster bricks
PreProcessRaster <- function (x) {
  RasterBr <- brick(x)
  RasterBr [RasterBr < 0] <- NA
  return (RasterBr)
}
