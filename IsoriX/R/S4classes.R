# This S4 definitions of classes are required for saveRDS to work and interact properly with the package terra.

#' Class ISOSCAPE
#'
#' @slot isoscapes a SpatRaster storing the isoscapes
#' @slot sp_points a list of spatial points
#'
#' @export
#' @rdname ISOSCAPE-class
#' 
setClass("ISOSCAPE", slots = c(isoscapes = "SpatRaster",
                               sp_points = "list"))


#' Class CALIBFIT
#'
#' @slot method a character string indicating the method used for the calibration
#' @slot species_rand a logical indicating whether the species random effect is included in the model
#' @slot site_rand a logical indicating whether the site random effect is included in the model
#' @slot param the fixed-effect estimates of the calibration function
#' @slot fixefCov the covariance matrix of the fixed effects
#' @slot phi the residual variance of the calibration fit
#' @slot calib_fit the fitted calibration model (if applicable)
#' @slot iso_fit the fitted calibration model (if applicable)
#' @slot data the calibration data
#' @slot sp_points a list of spatial points used for calibration
#'
#' @export
#' @rdname CALIBFIT-class
#' 
setClass("CALIBFIT", slots = c(method = "character",
                               species_rand = "logical",
                               site_rand = "logical",
                               param = "numeric",
                               fixefCov = "matrix",
                               phi = "numeric",
                               calib_fit = "list",
                               iso_fit = "list",
                               data = "data.frame",
                               sp_points = "list"))


#' Class ISOFIND
#'
#' @slot sample a list of SpatRaster objects storing the assignment info for each sample
#' @slot group a SpatRaster storing the group assignment info
#' @slot sp_points a list of SpatVector storing the spatial points for sources, calibration and assignment samples
#'
#' @export
#' @rdname ISOFIND-class
#' 
setClass("ISOFIND", slots = c(sample = "list",
                              group = "SpatRaster",
                              sp_points = "list"))
