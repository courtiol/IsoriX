createcalibData <- function(calibFunction = list(slope = 0.5, intercept = 3),
                            isoscape=isoscape,
                            nsites = 10) {
  
  ## choose location for these sendentary animals
  repeats <- sample(1:10, nsites, replace=T)
  cells <- rep(sample(1:ncell(isoscape$isoscape$mean), nsites, replace=F), repeats)
  xy <- xyFromCell(isoscape$isoscape$mean, cells)
  
  ## get the isotopic value in the environment
  environment.value <- extract(isoscape$isoscape$mean, xy)
  residualVar <- rnorm(length(cells), mean = 1, sd = 0.02)
  
  ## prepare the dataset
  siteID <- cells
  long <- xy[, 1]
  lat <- xy[, 2]
  animalID <- paste("animal", cells, sep = "_")
  tissue.value <- (intercept + environment.value * slope) * residualVar
  calibData <- data.frame(siteID, long, lat, animalID, tissue.value)
  
  ## add elevation
  elevationrasterbig <- raster("gmted2010_30mn.tif")
  calibData$elev <- extract(
    elevationrasterbig,
    cbind(calibData$long, calibData$lat))
  
  return(calibData)
}

