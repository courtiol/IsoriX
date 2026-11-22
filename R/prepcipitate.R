#' Prepare the raster brick containing the precipitation data
#'
#' This functions turns the WorldClim data downloaded using the function
#' [`getprecip`] into a *SpatRaster* of same resolution and
#' extent as the structural raster. This function is designed to be used with
#' [`isomultiscape`].
#'
#' @param path A *string* indicating the path where the WorldClim data have
#'   been downloaded. If the path is null (the default) the function will assume
#'   that the folder containing the precipitation data is in the current
#'   directory
#' @param raster A *raster* containing the structural raster
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session, and `FALSE` otherwise.
#'
#' @seealso
#'
#' [`getprecip`] to download the relevant precipitation data
#'
#' [`PrecipBrickDE`] for the stored precipitation data for Germany
#'
#' [`prepelev`] to prepare an elevation raster
#'
#' @examples
#'
#' ## The following example takes some time and download a large amount of data (~ 1 Gb).
#' ## It will therefore not be run unless you uncomment it
#'
#' ### We fit the models for Germany:
#' # GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' #
#' # GermanFit <- isofit(data = GNIPDataDEagg,
#' #                     mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
#' #
#' ### We prepare the structural raster:
#' # StrRaster <- prepraster(raster = ElevRasterDE,
#' #                         isofit = GermanFit,
#' #                         aggregation_factor = 0)
#' #
#' ### We download the precipitation data:
#' # temp_folder <- tempdir()
#' # getprecip(path = temp_folder)
#' #
#' ### We prepare the raster brick with all the precipitation data:
#' # PrecipitationBrick <- prepcipitate(path = temp_folder,
#' #                                    raster = StrRaster)
#' #
#' ### We plot the precipitation data:
#' # levelplot(PrecipitationBrick)
#'
#' @export
prepcipitate <- function(path = NULL,
                         raster,
                         verbose = interactive()) {
  ## Prepare path
  if (!is.null(path)) {
    path <- normalizePath(path, mustWork = FALSE)
  }

  if (is.null(path)) {
    path <- paste0(getwd(), "/wc2.1_30s_prec")
  } else {
    if (!grepl(pattern = "wc2.1_30s_prec", x = path)) { ## add subfolder if missing
      path <- paste0(path, "/wc2.1_30s_prec")
    }
  }

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
    tmp.raster <- terra::rast(getfilename(month))
    ## crop before resampling to save a lot of time
    tmp.raster <- .crop_withmargin(tmp.raster,
      xmin = terra::xmin(raster),
      xmax = terra::xmax(raster),
      ymin = terra::ymin(raster),
      ymax = terra::ymax(raster),
      margin_pct = 10
    ) # 10% hardcoded, probably fine for most case
    assign(paste0("month_", month), terra::resample(x = tmp.raster, y = raster))
    rm(tmp.raster)
  }

  ## Put all rasters in a RasterBrick
  precip <- terra::rast(mget(paste0("month_", 1:12)))
  return(precip)
}
