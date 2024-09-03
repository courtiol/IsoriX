createcalibData <- function(fn = list(intercept = 3,
                                      slope = 0.5,
                                      fixef_cov = matrix(c(100, 0.3, 0.3, 0.01), nrow = 2),
                                      resid_var = 10),
                            isoscape = NULL,
                            elevation.raster = NULL,
                            nsites = 10,
                            min.n.samples = 1,
                            max.n.samples = 10) {
  
  ## choose location for these sendentary animals
  
  locationData <- data.frame(siteID = sample(1:ncell(isoscape$isoscapes$mean), nsites, replace = FALSE))
  xy <- raster::xyFromCell(isoscape$isoscapes$mean, locationData$siteID)
  locationData$long <- xy[, "x"]
  locationData$lat  <- xy[, "y"]
  locationData$elev = raster::extract(x = elevation.raster, y = xy)
  locationData$n.samples <- round(stats::runif(n = nsites, min = min.n.samples, max = max.n.samples))
  
  ## predict environmental values at the locations
  
  locationData$mean.env.value <- raster::extract(isoscape$isoscapes$mean, xy)
  locationData$var.env.value  <- raster::extract(isoscape$isoscapes$mean.respVar, xy)  ## to check with Francois!!!
  locationData$var.env.value  <- 0
  ## It is not clear to me if respVar, predVar or no variance should be consider in this last step.
  ## Considering respVar would imply to assume that the animal values are derived from environmental values at one time step.
  ## Considering predVar would imply to assume that the animal values are derived from the average of environmental values
  ## across all time steps...
  ## Considering no variance would imply to assume that all animal values are derived from a unique random draw.
  locationData$env.value <- rnorm(n = nrow(locationData),
                                  mean = locationData$mean.env.value,
                                  sd = sqrt(locationData$var.env.value))
  ## Here, having drawn a single value from the environement for all observations occuring at one location,
  ## If we would use respVar above, this would imply to assume that either all individuals have been sampled at the same time.
  
  
  ## prepare predict tissue values at the locations
  
  locationData$mean.tissue.value <- fn$intercept + locationData$env.value * fn$slope
  X <- cbind(1, locationData$env.value)  ## compute desing matrix
  locationData$var.fixef.tissue.value <- rowSums(X * (X %*% fn$fixef_cov)) ## same as diag(X %*% fn$fixef_cov %*% t(X))
  locationData$var.tissue.value <- locationData$var.fixef.tissue.value + fn$resid_var
  
  
  ## replicate the dataset per animal
  
  calibData <- locationData[rep(1:nrow(locationData), times = locationData$n.samples), ]
  calibData$animalID <- paste("animal", 1:nrow(calibData), sep = "_")
  
  
  ## predict the tissue value for each animal
  
  calibData$tissue.value <- rnorm(n = nrow(calibData),
                                  mean = calibData$mean.tissue.value,
                                  sd = sqrt(calibData$var.tissue.value))
  
  
  ## cleanup and export
  
  rownames(calibData) <- NULL
  return(calibData[, c("siteID", "long", "lat", "elev", "animalID", "tissue.value")])
}

library("IsoriX")
data("Europefit")
data("elevraster")
elevationraster <- relevate(elevation.raster = elevraster, isofit = Europefit)
isoscape <- isoscape(elevation.raster = elevationraster, isofit = Europefit)
set.seed(1L)
res <- replicate(1000, {
  calibData <- createcalibData(fn = list(intercept = 3,
                                         slope = 0.5,
                                         fixef_cov = matrix(c(100, 0.3, 0.3, 0.1), nrow = 2),
                                         resid_var = 10),
                               isoscape = isoscape,
                               elevation.raster = elevationraster,
                               nsites = 10)
  calib <- calibfit(calib.data = calibData, isofit = Europefit)
  plot(calib)
  abline(3, 0.5, col = "green", lwd = 2, lty = 2)
  cbind(calib$param, calib$fixefCov)}, simplify = FALSE)
Reduce("+", res)/length(res)



