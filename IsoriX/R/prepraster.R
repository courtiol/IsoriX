#' Prepare the structural raster
#' 
#' This function prepares the structural raster for the follow-up analyses. The
#' size and extent of the structural raster defines the resolution at which the
#' isoscapes and the assignments are defined.
#' 
#' This functions allows the user to crop a raster according to
#' either the extent of the isoscape or manually. If a fitted isoscape object
#' is provided (see \code{\link{isofit}}), the function extracts the observed
#' locations of isotopic sources from the model object and crops the structural
#' raster accordingly. Alternatively, \code{manual_crop} allows you to crop the
#' structural raster to a desired extent. If no model and no coordinates for
#' manual cropping are provided, no crop will be performed. Importantly,
#' cropping is recommended as it prevents extrapolations outside the
#' latitude/longitude range of the source data. Predicting outside the range of
#' the source data may lead to highly unreliable predictions.
#' 
#' Aggregation changes the spatial resolution of the raster, making computation
#' faster and using less memory (this can affect the assignment; see note
#' below). An aggregation factor of zero (or one) keeps the resolution constant
#' (default).
#' 
#' This function relies on calls to the functions
#' \code{\link[raster]{aggregate}} and \code{\link[raster]{crop}} from the
#' package  \pkg{\link[raster]{raster}}. It thus share the limitations of these
#' functions. In particular, \code{\link[raster]{crop}} expects extents with
#' increasing longitudes and latitudes. We have tried to partially relax this
#' constrains for longitude and you can use the argument \code{manual_crop} to
#' provide longitudes in decreasing order, which is useful to center a isoscape
#' around the pacific for instance. But this fix does not solve all the
#' limitations as plotting polygons or points on top of that remains problematic
#' (see example bellow). We will work on this on the future but we have other 
#' priorities for now (let us know if you really need this feature).
#' 
#' @param raster The structural raster (\var{RasterLayer})
#' @param isofit The fitted isoscape model returned by the function
#' \code{\link{isofit}}
#' @param margin_pct The percentage representing by how much the space should 
#' extend outside the range of the coordinates of the sources
#' (default = 5). 
#' @param aggregation_factor The number of neighbouring cells (\var{integer})
#' to merge during aggregation
#' @param aggregation_fn The \var{function} used to aggregate cells
#' @param manual_crop A vector of four coordinates (\var{numeric}) for manual
#' cropping, e.g. the spatial extent
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session, and \var{FALSE} otherwise.
#' @return The prepared structural raster of class \var{RasterLayer}
#' @note Aggregating the raster may lead to different results for the
#' assignment, because the values of raster cells changes depending on the
#' aggregation function (see example below), which in turn affects model
#' predictions.
#' @seealso \code{\link{ElevRasterDE}} for information on elevation rasters, which
#' can be used as structural rasters.
#' 
#' \code{\link{IsoriX}} for the complete workflow
#' @keywords utilities
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## We fit the models for Germany
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' ### Let's explore the difference between aggregation schemes
#' 
#' ## We aggregate and crop using different settings
#' ElevationRaster1 <- prepraster(
#'     raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 0,
#'     aggregation_factor = 0)
#' 
#' ElevationRaster2 <- prepraster(
#'     raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 5,
#'     aggregation_factor = 5)
#' 
#' ElevationRaster3 <- prepraster(
#'     raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 10,
#'     aggregation_factor = 5, aggregation_fn = max)
#' 
#' ## We build the plots of the outcome of the 3 different aggregation schemes
#' if(require(rasterVis)) {
#'     PlotAggregation1 <- levelplot(ElevationRaster1,
#'             margin = FALSE, main = "Original small raster") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))
#'     PlotAggregation2 <- levelplot(ElevationRaster2,
#'             margin = FALSE, main = "Small raster aggregated (by mean)") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))
#'     PlotAggregation3 <- levelplot(ElevationRaster3,
#'             margin = FALSE, main = "Small raster aggregated (by max)") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))  
#'     
#'     ## We plot as a panel using lattice syntax:
#'     print(PlotAggregation1, split = c(1, 1, 3, 1), more = TRUE)
#'     print(PlotAggregation2, split = c(2, 1, 3, 1), more = TRUE)
#'     print(PlotAggregation3, split = c(3, 1, 3, 1))
#' }
#' }
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 10) {
#' 
#' ### Let's create a raster centered around the pacific
#' 
#' ## We first create an empty raster
#' EmptyRaster <- raster(matrix(0, ncol = 360, nrow = 180))
#' extent(EmptyRaster) <- c(-180, 180, -90, 90)
#' projection(EmptyRaster) <- CRS("+proj=longlat +datum=WGS84")
#' 
#' ## We crop it around the pacific
#' PacificA <- prepraster(EmptyRaster, manual_crop = c(110, -70, -90, 90))
#' extent(PacificA) # note that the extent has changed!
#' 
#' ## We plot (note the use of the function shift()!)
#' if(require(rasterVis)) {
#'   levelplot(PacificA, margin = FALSE, colorkey = FALSE, col = "blue")+
#'     layer(sp.polygons(CountryBorders, fill = "black"))+
#'     layer(sp.polygons(shift(CountryBorders, x = 360), fill = "black"))
#'   }
#' 
#' }
#' 
#' @export
prepraster <- function(raster,
                       isofit = NULL,
                       margin_pct = 5,
                       aggregation_factor = 0L,
                       aggregation_fn = mean,
                       manual_crop = NULL,
                       verbose = interactive()
                       ) {

  time <- system.time({
    if (!is.null(isofit)) {  ## test if cropping is needed
      if (any(class(isofit) %in% "multiisofit")) {
        isofit <- isofit$multi.fits[[1]]
        }
      if (!is.null(manual_crop)) stop("cannot crop both according to sources and manually! Make up your choice.")
      if (## test if the raster is not smaller than the area covered by the sources.
          ## If yes crop will not proceed!
          raster::xmin(raster) > min(isofit$mean_fit$data$long) |
          raster::xmax(raster) < max(isofit$mean_fit$data$long) |
          raster::ymin(raster) > min(isofit$mean_fit$data$lat) |
          raster::ymax(raster) < max(isofit$mean_fit$data$lat)
          ) {
        warning("the cropping may not make sense (sources located outside structural raster)")
      }
      if (verbose) {
        print(paste("cropping..."))
      }
      
      ## crop is performed:
      margin_long <- (max(isofit$mean_fit$data$long) - min(isofit$mean_fit$data$long)) * margin_pct/100
      margin_lat <- (max(isofit$mean_fit$data$lat) - min(isofit$mean_fit$data$lat)) * margin_pct/100
      
      raster <- raster::crop(raster,
                               raster::extent(min(isofit$mean_fit$data$long) - margin_long,
                                              max(isofit$mean_fit$data$long) + margin_long,
                                              min(isofit$mean_fit$data$lat) - margin_lat,
                                              max(isofit$mean_fit$data$lat) + margin_lat))
    } else {
      if (length(manual_crop) == 4) {
        
        if ((manual_crop[1] > manual_crop[2]) && (manual_crop[3] < manual_crop[4])) {
          crop1 <- raster::crop(raster,
                                raster::extent(raster::xmin(raster::extent(raster)),
                                               manual_crop[2],
                                               manual_crop[3],
                                               manual_crop[4]))
          crop2 <- raster::crop(raster,
                                raster::extent(manual_crop[1],
                                               raster::xmax(raster::extent(raster)),
                                               manual_crop[3],
                                               manual_crop[4]))
          raster <- raster::shift(raster::merge(crop1, raster::shift(crop2,
                                                                     x = -360)),
                                  x = 360)
          warning("The first longitude is greater than the second one. You may want this to study something around the pacific. This feature is not fully supported... but... the function prepraster() tried to cope with this. That implies a change in the coordinate system (0:360 instead of -180:180). This should create no problem for ploting isoscapes but this can create troubles to add polygons or points on the maps. If that is the case, you need to add 360 degree to the longitudes... If all that sounds complicated, just stick to a first longitude SMALLER than the second one.")
        } else {
          raster <- raster::crop(raster, manual_crop)
        }
      }
    }
    if (aggregation_factor > 1) {  ## test if aggregation is needed
      if (interactive()) {
        print(paste("aggregating..."))
      }
      raster <- raster::aggregate(raster, fact = aggregation_factor, fun = aggregation_fn)  ## aggregation
    }
  })

  ## store the raster in memory if possible
  if (raster::canProcessInMemory(raster)) {
    raster_HD <- raster
    raster <- raster::raster(raster_HD)
    raster::values(raster) <- raster::values(raster_HD)
  }
  
  if (verbose) {
    print(paste("done!"))
    print(time)
  }

  print(raster)
  return(raster)
}

