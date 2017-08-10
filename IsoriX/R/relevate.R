#' @rdname IsoriX-defunct
#' @export
RElevate <- function(...) {
    .Defunct("relevate")
  }



#' Prepare the elevation raster
#' 
#' This function prepares the elevation raster for the follow-up analyses. The
#' size and extent of the elevation raster defines the resolution at which the
#' isoscape and the origin assignment are defined.
#' 
#' This functions allows the user to crop an elevation raster according to
#' either the extent of the isoscape or manually. If a fitted isoscape object
#' is provided (see \code{\link{isofit}}), the function extracts the observed
#' locations of isotopic sources from the model object and crops the elevation
#' raster accordingly. Alternatively, \code{manual.crop} allows you to crop the
#' elevation raster to a desired extent. If no model and no coordinates for
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
#' constrains for longitude and you can use the argument \code{manual.crop} to
#' provide longitudes in decreasing order, which is useful to center a isoscape
#' around the pacific for instance. But this fix does not solve all the
#' limitations as plotting polygons or points on top of that remains problematic
#' (see example bellow). We will work on this on the future but we have other 
#' priorities for now (let us know if you really need this feature).
#' 
#' @param elevation.raster The elevation raster (\var{RasterLayer})
#' @param isofit The fitted isoscape model returned by the function
#' \code{\link{isofit}}
#' @param margin_pct The percentage representing by how much the space should 
#' extend outside the range of the coordinates of the weather stations
#' (default = 5). 
#' @param aggregation.factor The number of neighbouring cells (\var{integer})
#' to merge during aggregation
#' @param aggregation.fun The \var{function} used to aggregate cells
#' @param manual.crop A vector of four coordinates (\var{numeric}) for manual
#' cropping, e.g. the spatial extent
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session, and \var{FALSE} otherwise.
#' @return The fine-tuned elevation raster of class \var{RasterLayer}
#' @note Aggregating the raster may lead to different results for the
#' assignment, because the elevation of raster cells changes depending on the
#' aggregation function (see example below), which in turn affects model
#' predictions.
#' @seealso \code{\link{ElevRasterDE}} for information on elevation rasters
#' 
#' \code{\link{IsoriX}} for the complete workflow
#' @keywords utilities
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. IsoriX.options(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(IsoriX.getOption("example_maxtime") > 30) {
#' 
#' ## We fit the models for Germany
#' GNIPDataDEagg <- prepdata(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(iso.data = GNIPDataDEagg,
#'                     mean.model.fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' ### Let's explore the difference between aggregation schemes
#' 
#' ## We aggregate and crop using different settings
#' elevation.raster1 <- relevate(
#'     elevation.raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 0,
#'     aggregation.factor = 0)
#' 
#' elevation.raster2 <- relevate(
#'     elevation.raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 5,
#'     aggregation.factor = 5)
#' 
#' elevation.raster3 <- relevate(
#'     elevation.raster = ElevRasterDE,
#'     isofit = GermanFit,
#'     margin_pct = 10,
#'     aggregation.factor = 5, aggregation.fun = max)
#' 
#' ## We build the plots of the outcome of the 3 different aggregation schemes
#' if(require(rasterVis)) {
#'     plot.aggregation1 <- levelplot(elevation.raster1,
#'             margin = FALSE, main = "Original small raster") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))
#'     plot.aggregation2 <- levelplot(elevation.raster2,
#'             margin = FALSE, main = "Small raster aggregated (by mean)") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))
#'     plot.aggregation3 <- levelplot(elevation.raster3,
#'             margin = FALSE, main = "Small raster aggregated (by max)") + 
#'         layer(sp.polygons(CountryBorders)) +
#'         layer(sp.polygons(OceanMask, fill = "blue"))  
#'     
#'     ## We plot as a panel using lattice syntax:
#'     print(plot.aggregation1, split = c(1, 1, 1, 3), more = TRUE)
#'     print(plot.aggregation2, split = c(1, 2, 1, 3), more = TRUE)
#'     print(plot.aggregation3, split = c(1, 3, 1, 3))
#' }
#' }
#' 
#' #' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. IsoriX.options(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(IsoriX.getOption("example_maxtime") > 10) {
#' 
#' ### Let's create a raster centered around the pacific
#' 
#' ## We first create an empty raster
#' empty.raster <- raster(matrix(0, ncol = 360, nrow = 180))
#' extent(empty.raster) <- c(-180, 180, -90, 90)
#' projection(empty.raster) <- CRS("+proj=longlat +datum=WGS84")
#' 
#' ## We crop it around the pacific
#' pacificA <- relevate(empty.raster, manual.crop = c(110, -70, -90, 90))
#' extent(pacificA) # note that the extent has changed!
#' 
#' ## We plot (note the use of the function shift()!)
#' if(require(rasterVis)) {
#'   levelplot(pacificA, margin = FALSE, colorkey = FALSE, col = "blue")+
#'     layer(sp.polygons(CountryBorders, fill = "black"))+
#'     layer(sp.polygons(shift(CountryBorders, x = 360), fill = "black"))
#'   }
#' 
#' }
#' 
#' @export
relevate <- function(elevation.raster,
                     isofit = NULL,
                     margin_pct = 5,
                     aggregation.factor = 0L,
                     aggregation.fun = mean,
                     manual.crop = NULL,
                     verbose = interactive()
                     ) {

  time <- system.time({
    if (!is.null(isofit)) {  ## test if cropping is needed
      if (any(class(isofit) %in% "multiisofit")) {
        isofit <- isofit$multi.fits[[1]]
        }
      if (!is.null(manual.crop)) stop("cannot crop both according to sources and manually! Make up your choice.")
      if (## test if the elevation raster is not smaller than the area covered by the weather sources.
          ## If yes crop will not proceed!
          raster::xmin(elevation.raster) > min(isofit$mean.fit$data$long) |
          raster::xmax(elevation.raster) < max(isofit$mean.fit$data$long) |
          raster::ymin(elevation.raster) > min(isofit$mean.fit$data$lat) |
          raster::ymax(elevation.raster) < max(isofit$mean.fit$data$lat)
          ) {
        warning("the cropping may not make sense (sources located outside elevation raster)")
      }
      if (verbose) {
        print(paste("cropping..."))
      }
      
      ## crop is performed:
      margin_long <- (max(isofit$mean.fit$data$long) - min(isofit$mean.fit$data$long)) * margin_pct/100
      margin_lat <- (max(isofit$mean.fit$data$lat) - min(isofit$mean.fit$data$lat)) * margin_pct/100
      
      elevation.raster <- raster::crop(elevation.raster,
                               raster::extent(min(isofit$mean.fit$data$long) - margin_long,
                                              max(isofit$mean.fit$data$long) + margin_long,
                                              min(isofit$mean.fit$data$lat) - margin_lat,
                                              max(isofit$mean.fit$data$lat) + margin_lat))
    } else {
      if (length(manual.crop) == 4) {
        
        if ((manual.crop[1] > manual.crop[2]) && (manual.crop[3] < manual.crop[4])) {
          crop1 <- raster::crop(elevation.raster,
                                raster::extent(elevation.raster@extent@xmin,
                                               manual.crop[2],
                                               manual.crop[3],
                                               manual.crop[4]))
          crop2 <- raster::crop(elevation.raster,
                                raster::extent(manual.crop[1],
                                               elevation.raster@extent@xmax,
                                               manual.crop[3],
                                               manual.crop[4]))
          elevation.raster <- raster::shift(raster::merge(crop1,
                                                          raster::shift(crop2,
                                                                        x = -360)),
                                            x = 360)
          warning("The first longitude is greater than the second one. You may want this to study something around the pacific. This feature is not fully supported... but... the function relevate() tried to cope with this. That implies a change in the coordinate system (0:360 instead of -180:180). This should create problems for ploting isoscapes but this can create troubles to add polygons or points on the maps. If that is the case, you need to add 360 degree to the longitudes... If all that sounds complicated, just stick to a first longitude SMALLER than the second one.")
        } else {
          elevation.raster <- raster::crop(elevation.raster, manual.crop)
        }
      }
    }
    if (aggregation.factor > 1) {  ## test if aggregation is needed
      if (interactive()) {
        print(paste("aggregating..."))
      }
      elevation.raster <- raster::aggregate(elevation.raster, fact = aggregation.factor, fun = aggregation.fun)  ## aggregation
    }
  })

  if (verbose) {
    print(paste("done!"))
    print(time)
  }

  print(elevation.raster)
  return(elevation.raster)
}
