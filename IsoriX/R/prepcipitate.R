#' Prepare the raster brick containing the precipitation data
#' 
#' This functions turns the WorldClim data downloaded using the function
#' \code{\link{getprecip}} into a \var{RasterBrick} of same resolution and
#' extent as the structural raster. This function is designed to be used with
#' \code{\link{isomultiscape}}.
#' 
#' @param path A \var{string} indicating the path where the WorldClim data have
#'   been downloaded. If the path is null (the default) the function will assume
#'   that the folder containing the precipitation data is in the current 
#'   directory
#' @param raster A \var{raster} containing the structural raster
#' @param verbose A \var{logical} indicating whether information about the 
#'   progress of the procedure should be displayed or not while the function is 
#'   running. By default verbose is \var{TRUE} if users use an interactive R 
#'   session, and \var{FALSE} otherwise.
#'   
#' @seealso
#' 
#' \code{\link{getprecip}} to download the relevant precipitation data
#' 
#' \code{\link{PrecipBrickDE}} for the stored precipitation data for Germany
#' 
#' \code{\link{prepelev}} to prepare an elevation raster
#' 
#' @examples
#' 
#' ## The following example takes some time and download a large amount of data (~ 1 Gb).
#' ## It will therefore not be run unless you uncomment it
#' 
#' ### We fit the models for Germany:
#' #GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' #
#' #GermanFit <- isofit(data = GNIPDataDEagg,
#' #                     mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
#' #
#' ### We prepare the structural raster:
#' #StrRaster <- prepraster(raster = ElevRasterDE,
#' #                        isofit = GermanFit,
#' #                        aggregation_factor = 0)
#' #
#' ### We download the precipitation data:
#' #getprecip(path = "~/Downloads/")
#' #
#' ### We prepare the raster brick with all the precipitation data:
#' #PrecipitationBrick <- prepcipitate(path = "~/Downloads/",
#' #                                   raster = StrRaster)
#' #
#' ### We plot the precipitation data:
#' #levelplot(PrecipitationBrick)
#'
#' @export
prepcipitate <- function(path = NULL,
                         raster,
                         verbose = interactive()
                        ) {
  
  ## Prepare path
  if (is.null(path)) {
    path <- paste0(getwd(), "/wc2.1_30s_prec")
  } else {
    path <- paste0(path, "/wc2.1_30s_prec")
  }
  
  path <- normalizePath(path, mustWork = FALSE)
  
  ## List the tif files
  list_tif <- list.files(path = path, pattern = "\\.tif$")
  
  ## Checks if the tif files are there
  if (length(list_tif) == 0) {
    stop("There is no *.tif file in path... you may have the path wrong or you may not have downloaded the file using 'getprecip()'.")
  }
  
  ## Checks if the tif files are the good ones
  if (!all(paste0("wc2.1_30s_prec_", formatC(1:12, digits = 0, width = 2, format = "f", flag = 0), ".tif") %in% list_tif)) {
    stop("The '.tif' files do not have expected names: 'wc2.1_30s_prec_01.tif', 'wc2.1_30s_prec_02.tif', ...")
  }
  
  ## Small function to get the name of a given file
  getfilename <- function(month) {
    paste0(path, "/wc2.1_30s_prec_", formatC(month, digits = 0, width = 2, format = "f", flag = 0), ".tif")
  }
  
  ## Import and resize rasters one by one
  for (month in 1:12) {
    if (verbose) {
      print(paste("Preparing precipitation raster for month", month, "..."), quote = FALSE)
    }
    tmp.raster <- raster::raster(getfilename(month))
    ## crop before resampling to save a lot of time
    tmp.raster <- .crop_withmargin(tmp.raster,
                                   xmin = raster::xmin(raster),
                                   xmax = raster::xmax(raster),
                                   ymin = raster::ymin(raster),
                                   ymax = raster::ymax(raster),
                                   margin_pct = 10) # 10% hardcoded, probably fine for most case
    assign(paste0("month_", month), raster::resample(x = tmp.raster, y = raster))
    rm(tmp.raster)
  }
  
  ## Put all rasters in a RasterBrick
  precip <- raster::brick(mget(paste0("month_", 1:12)))
  return(precip)
}
