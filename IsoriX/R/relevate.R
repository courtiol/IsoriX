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
#' @param elevation.raster The elevation raster (\var{RasterLayer})
#' @param isofit The fitted isoscape model returned by the function
#' \code{\link{isofit}}
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
#' @seealso \code{\link{elevraster}} for information on elevation rasters
#' 
#' \code{\link{IsoriX}} for the complete workflow
#' @keywords utilities
#' @examples
#' 
#' 
#' ### DIFFERENCES IN AGGREGATION
#' 
#' ## Loading objects
#' data(elevraster)
#' data(GermanFit)
#' data(countries)
#' data(oceanmask)
#' 
#' ## We aggregate and crop using different settings
#' elevation.raster1 <- relevate(
#'     elevation.raster = elevraster,
#'     isofit = GermanFit,
#'     aggregation.factor = 0)
#' 
#' elevation.raster2 <- relevate(
#'     elevation.raster = elevraster,
#'     isofit = GermanFit,
#'     aggregation.factor = 5)
#' 
#' elevation.raster3 <- relevate(
#'     elevation.raster = elevraster,
#'     isofit = GermanFit,
#'     aggregation.factor = 5, aggregation.fun = max)
#' 
#' 
#' ## The following plot takes a few second too many and will 
#' ## therefore not be run unless you type: 
#' ## example(relevate, run.dontrun = TRUE)
#' 
#' \dontrun{
#'     ## We plot the outcome of the 3 different aggregation schemes
#' if(require(rasterVis)) {
#'     data(countries)
#'     data(oceanmask)
#'     plot.aggregation1 <- levelplot(elevation.raster1,
#'             margin = FALSE, main = "Original small raster") + 
#'         layer(sp.polygons(countries)) +
#'         layer(sp.polygons(oceanmask, fill = "blue"))
#'     plot.aggregation2 <- levelplot(elevation.raster2,
#'             margin = FALSE, main = "Small raster aggregated (by mean)") + 
#'         layer(sp.polygons(countries)) +
#'         layer(sp.polygons(oceanmask, fill = "blue"))
#'     plot.aggregation3 <- levelplot(elevation.raster3,
#'             margin = FALSE, main = "Small raster aggregated (by max)") + 
#'         layer(sp.polygons(countries)) +
#'         layer(sp.polygons(oceanmask, fill = "blue"))  
#'     ## panel using lattice syntax:
#'     print(plot.aggregation1, split = c(1, 1, 1, 3), more = TRUE)
#'     print(plot.aggregation2, split = c(1, 2, 1, 3), more = TRUE)
#'     print(plot.aggregation3, split = c(1, 3, 1, 3))
#' }
#' }
#' 
#' @export
relevate <- function(elevation.raster,
                     isofit = NULL,
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
        stop("cropping not possible (sources located outside elevation raster)")
      }
      if (verbose) {
        print(paste("cropping..."))
      }
      ## crop is performed:
      elevation.raster <- raster::crop(elevation.raster,
                               raster::extent(min(isofit$mean.fit$data$long),
                                              max(isofit$mean.fit$data$long),
                                              min(isofit$mean.fit$data$lat),
                                              max(isofit$mean.fit$data$lat)
                                              )
                               )
    } else {
      if (length(manual.crop) == 4) {
        elevation.raster <- raster::crop(elevation.raster, manual.crop)
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
