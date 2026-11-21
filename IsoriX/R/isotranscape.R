#' Converts isoscapes of sources into isoscapes of samples (e.g. tissues)
#'
#' This function uses a calibration fit to convert the mean isoscape of
#' source isotopic values (e.g. precipitation) into the mean isoscape of the
#' isotopic values expected in the samples (e.g. animal tissues).
#'
#' For assignments, IsoriX converts the isotopic values of the samples to
#' assign (e.g. animal tissues) into isotopic values of the sources (e.g.
#' precipitation), and not the other way around (see [`isofind`]). Yet, it can
#' be useful to visualise or study what the isoscape would look like in terms of
#' sample values. This is what `isotranscape()` does.
#'
#' The calibration fit (see [`calibfit`]) models the relationship
#' `sample_value = intercept + slope * source_value`, whatever the calibration
#' method used (for the method `"desk_inverse"`, the inversion of the
#' regression is already performed by [`calibfit`]). The mean isoscape is thus
#' simply converted into the isoscape of samples by computing, in each raster
#' cell, `intercept + slope * mean`, using the estimates of the intercept and
#' of the slope stored in the calibration fit.
#'
#' Only the mean isoscape is converted. The variances (prediction, residual
#' and response variances) are not converted since their conversion would
#' require to account for the uncertainty of the calibration fit, which depends
#' on the calibration method being used. The isoscapes related to the residual
#' dispersion are not returned either.
#'
#' The output can be plotted using [`plot.ISOSCAPE`] (with `which = "mean"`
#' only), but it cannot be used for assignments ([`isofind`]) or to simulate
#' data ([`create_aliens`]), since these functions require isoscapes of source
#' isotopic values.
#'
#' @param isoscape The output of the function [`isoscape`] or
#'   [`isomultiscape`] (i.e. an object of class *ISOSCAPE*)
#' @param calibfit The output of the function [`calibfit`] (i.e. an object of
#'   class *CALIBFIT*)
#' @return This function returns a *list* of class *ISOSCAPE* and
#'   *ISORESCALED* containing a *SpatRaster* with a single layer (`mean`)
#'   storing the predicted mean isotopic values of the samples, and the location
#'   of the sources as spatial points.
#' @seealso [`isoscape`] and [`isomultiscape`] for the functions building the
#'   isoscapes of sources
#'
#'   [`calibfit`] for the function fitting the calibration
#'
#'   [`plot.ISOSCAPE`] for the function plotting isoscapes
#'
#' @keywords models prediction
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## We fit the models
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'   GermanFit <- isofit(
#'     data = GNIPDataDEagg,
#'     mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
#'   )
#'
#'   ## We build the isoscapes of sources (precipitation)
#'   GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#'   ## We define a calibration fit from published estimates
#'   CalibDataDesk <- data.frame(intercept = -10, slope = 0.8)
#'   CalibDesk <- calibfit(data = CalibDataDesk, method = "desk")
#'
#'   ## We convert the isoscape of sources into an isoscape of tissues
#'   GermanTissueScape <- isotranscape(isoscape = GermanScape, calibfit = CalibDesk)
#'
#'   GermanTissueScape
#'   plot(GermanTissueScape)
#' }
#'
#' @export
isotranscape <- function(isoscape, calibfit) {
  if (!inherits(isoscape, "ISOSCAPE")) {
    stop("The argument `isoscape` must be an object of class ISOSCAPE (i.e. created by `isoscape()` or `isomultiscape()`).")
  }

  if (inherits(isoscape, "ISORESCALED")) {
    stop("The argument `isoscape` has already been converted by `isotranscape()`; it cannot be converted again.")
  }

  if (!inherits(calibfit, "CALIBFIT")) {
    stop("The argument `calibfit` must be an object of class CALIBFIT (i.e. created by `calibfit()`).")
  }

  intercept <- calibfit$param[["intercept"]]
  slope <- calibfit$param[["slope"]]

  mean_rescaled <- intercept + slope * isoscape$isoscapes[["mean"]]
  names(mean_rescaled) <- "mean"

  out <- list(
    isoscapes = mean_rescaled,
    sp_points = isoscape$sp_points
  )

  class(out) <- c("ISOSCAPE", "ISORESCALED", "list")

  return(out)
}


.stop_if_rescaled <- function(isoscape, fn) {
  ## This function should not be called by the user.
  ## It prevents the use of isoscapes produced by isotranscape() in functions
  ## requiring isoscapes of source isotopic values.
  if (inherits(isoscape, "ISORESCALED")) {
    stop(paste0(
      "The isoscape provided has been converted into sample values by `isotranscape()`, so it cannot be used by `",
      fn, "()`, which requires an isoscape of source values (as produced by `isoscape()` or `isomultiscape()`)."
    ), call. = FALSE)
  }
  return(invisible(NULL))
}
