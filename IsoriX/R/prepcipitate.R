#' Prepare the raster brick containing the precipitation data
#' 
#' This functions turns the WorldClim data downloaded using the function
#' \code{\link{getprecip}} into a \var{RasterBrick} of same resolution and
#' extent as the elevation raster. This function is designed to be used with
#' \code{\link{isomultiscape}}.
#' 
#' @param path A \var{string} indicating the path where the WorldClim data have
#'   been downloaded. If the path is null (the default) the function will assume
#'   that the folder containing the precipitation data is in the current 
#'   directory
#' @param elevation.raster A \var{raster} containing the elevation raster
#' @param verbose A \var{logical} indicating whether information about the 
#'   progress of the procedure should be displayed or not while the function is 
#'   running. By default verbose is \var{TRUE} if users use an interactive R 
#'   session, and \var{FALSE} otherwise.
#'   
#' @seealso
#' 
#' \code{\link{getprecip}} to download the relevant precipitation data
#' 
#' \code{\link{relevate}} to prepare the elevation raster
#' 
#' @examples
#' 
#' ## The following example takes some time and download heavy data.
#' ## It will therefore not be run unless you type: 
#' ## example(prepcipitate, run.dontrun = TRUE)
#' 
#' \dontrun{
#' data(elevraster)
#' data(Europefit)
#' 
#' elevation.raster <- relevate(
#'     elevation.raster = elevraster,
#'     isofit = Europefit,
#'     aggregation.factor = 0)
#' 
#' getprecip(path = "~/Desktop/")
#' 
#' precipitation.brick <- prepcipitate(path = "~/Desktop/",
#'                                     elevation.raster = elevraster
#'                                     )
#'  
#'  if (require(rasterVis)) {
#'    levelplot(precipitation.brick)
#'  }
#' 
#' }
#' 
prepcipitate <- function(path = NULL,
                         elevation.raster,
                         verbose = interactive()
                        ) {
  
  ## Prepare path
  if (is.null(path)) {
    path <- paste0(getwd(), "/wc2.0_30s_prec")
  }
  
  path <- normalizePath(path, mustWork = FALSE)
  
  ## List the tif files
  list.tif <- list.files(path = path, pattern = "\\.tif$")
  
  ## Checks if the tif files are there
  if (length(list.tif) == 0) {
    stop("no *.tif file in path... you may have the path wrong or you may not have downloaded the file using 'getprecip()'")
  }
  
  ## Checks if the tif files are the good ones
  if (!all(paste0("wc2.0_30s_prec_", formatC(1:12, digits = 0, width = 2, format = "f", flag = 0), ".tif") %in% list.tif)) {
    stop("the '.tif' files do not have expected names: 'wc2.0_30s_prec_01.tif', 'wc2.0_30s_prec_02.tif', ...")
  }
  
  ## Small function to get the name of a given file
  getfilename <- function(month) {
    paste0(path, "/wc2.0_30s_prec_", formatC(month, digits = 0, width = 2, format = "f", flag = 0), ".tif")
  }
  
  ## Import and resize rasters one by one
  for (month in 1:12) {
    if (verbose) {
      print(paste("Preparing precipitation raster for month", month, "..."), quote = FALSE)
    }
    tmp.raster <- raster::raster(getfilename(month))
    assign(paste0("month_", month), raster::resample(x = tmp.raster, y = elevation.raster))
    rm(tmp.raster)
  }
  
  ## Put all rasters in a RasterBrick
  precip <- raster::brick(mget(paste0("month_", 1:12)))
  return(precip)
}
