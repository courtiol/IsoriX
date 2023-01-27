## Small script to test that all functions in the package work properly

run <- FALSE ## To avoid cyclic issues in R CMD check
#run <- TRUE

if (run) {
  library(IsoriX)
  options_IsoriX(example_maxtime = Inf)
  
  example(options_IsoriX)
  
  example(create_aliens)
  
  example(AssignDataBat)
  example(AssignDataBat2)
  example(CalibDataBat)
  example(CalibDataBat2)
  CalibDataBat2$elev <- NULL
  foo <- tempfile()
  getelev(file = foo, z = 1,
          lat_min = min(CalibDataBat2$lat),
          lat_max = max(CalibDataBat2$lat),
          long_min = min(CalibDataBat2$long),
          long_max = max(CalibDataBat2$long))
  ElevationRasterBig <- raster(foo)
  CalibDataBat2$elev <- extract(
    ElevationRasterBig,
    cbind(CalibDataBat2$long, CalibDataBat2$lat))
  head(CalibDataBat2)
  example(AssignDataAlien)
  example(CalibDataAlien)
  
  foo <- tempfile()
  getelev(file = foo, z = 1)
  elev_raster <- terra::rast(foo)
  plot(elev_raster)
  rm(elev_raster)
  
  foo <- tempdir()
  #getprecip(path = foo) ## SLOW & BUG
  
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)
  GermanFit <- isofit(data = GNIPDataDEagg,
                      mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
  StrRaster <- prepraster(raster = ElevRasterDE,
                          isofit = GermanFit,
                          aggregation_factor = 0)
  getprecip(path = "~/Downloads/", overwrite = TRUE)  ## SLOW & BUG
  PrecipitationBrick <- prepcipitate(path = "~/Downloads/", raster = StrRaster)
  levelplot(PrecipitationBrick)
  
  # example(isosim) ## not activated for now
  
  example(prepraster)
  
  example(prepsources)
  
  example(isofit)
  example(isomultifit)
  
  example(isoscape)
  
  example(isomultiscape)
  
  example(calibfit)
  
  example(isofind)
}