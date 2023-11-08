# This S4 definitions of classes are required for saveRDS to work and interact properly with the package terra.

#' Class ISOSCAPE
#'
#' @slot isoscapes a SpatRaster storing the isoscapes. 
#' @slot sp_points a list of spatial points. 
#'
#' @export
#' @rdname ISOSCAPE-class
#' 
setClass("ISOSCAPE", slots = c(isoscapes = "SpatRaster", sp_points = "list"))