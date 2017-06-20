create_aliens <- function(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
                          isoscape = NULL,
                          elevation.raster = NULL,
                          nsites = 1,
                          min.n.samples = 1,
                          max.n.samples = 10) {
  
  ## choose location for the aliens
  LocationData <- data.frame(siteID = sample(1:ncell(isoscape$isoscape$mean), nsites, replace = FALSE))
  xy <- raster::xyFromCell(isoscape$isoscape$mean, LocationData$siteID)
  LocationData$long <- xy[, "x"]
  LocationData$lat  <- xy[, "y"]
  LocationData$elev = raster::extract(x = elevation.raster, y = xy)
  LocationData$n.samples <- round(stats::runif(n = nsites, min = min.n.samples, max = max.n.samples))
  
  ## predict environmental values at the locations
  LocationData$env.value <- raster::extract(isoscape$isoscape$mean, xy)
 
  ## replicate the dataset per animal
  AlienData <- LocationData[rep(1:nrow(LocationData), times = LocationData$n.samples), ]
  AlienData$animalID <- paste("Alien", 1:nrow(AlienData), sep = "_")
  
  ## predict the tissue value for each animal
  AlienData$tissue.value <- rnorm(n = nrow(AlienData),
                                  mean = calib_fn$intercept + LocationData$env.value * calib_fn$slope,
                                  sd = sqrt(calib_fn$resid_var))
  
  ## cleanup and export
  rownames(AlienData) <- NULL
  return(AlienData[, c("siteID", "long", "lat", "elev", "animalID", "tissue.value")])
}

library("IsoriX")
data("Europefit")
data("elevraster")
elevationraster <- relevate(elevation.raster = elevraster, isofit = Europefit)
isoscape <- isoscape(elevation.raster = elevationraster, isofit = Europefit)
set.seed(1L)

create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
                          isoscape = isoscape,
                          elevation.raster = elevationraster,
                          nsites = 10,
                          min.n.samples = 1,
                          max.n.samples = 10)

