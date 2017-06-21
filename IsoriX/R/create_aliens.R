#' Simulate datasets for calibrations or assignments
#' 
#' This function allows to simulate data so to provide examples for the
#' calibration and for the assignment procedure. We names the simulated
#' individuals 'Aliens' so to make it clear that the data we use to illustrate 
#' our package are not real data.
#' 
#' The isostopic values for the organisms are assumed to be linearly related to
#' the one from the environement. The linear function can be parameterized using
#' the first argument of the function (\code{calib_fn}). With this function the
#' user can simulate data for different sites. The number of sites is controled
#' by means of the argument \code{n_sites}. The sites will be selected randomly
#' among the locations present in the isoscape (argument \code{isoscape})
#' provided to this function. At each site, a random number of observation will
#' be drawn according to a uniform distribution bounded by the values of the 
#' argument \code{min_n_samples} and \code{max_n_samples}.
#' 
#' From the selected coordinates, the isotope values for the environement are
#' directly extracted from the corresponding point predictions stored in the
#' isoscape object. No uncertainty is considered during this process. Then the
#' linear calibration defines the means of the isotope values for the simulated
#' organims. The actual values is then drawn from a Gaussian distribution
#' centered around such mean and a variance defined by the residual variance
#' (\code{resid_var}) input within the list \code{calib_fn}.
#' 
#' @param calib_fn A \var{list} containing the parameter values describing the 
#'   relationship between the isotope values in the environment and those in the
#'   simulated organisms. This list must contain three parameters: the
#'   intercept, the slope, and the residual variance.
#'   
#' @param isoscape The output of the function \code{\link{isoscape}}
#'   
#' @param elevation_raster A \var{RasterLayer} containing an elevation raster
#'   
#' @param n_sites The number of sites from which the simulated organisms
#'   originate (\var{integer})
#'   
#' @param min_n_samples The minimal number of observations (\var{integer}) per
#'   site
#'   
#' @param max_n_samples The maximal number of observations (\var{integer}) per
#'   site
#'   
#' @return This functions returns a \var{data.frame} (see example for column
#'   names)
#'   
#' @seealso \pkg{\link{calibfit}} for a calibration based on simulated data
#'   
#'   \pkg{\link{isofind}} for an assignment based on simulated data
#'   
#'   \code{\link{IsoriX}} for the complete work-flow of our package
#' @keywords simulate simulation
#' @examples
#' 
#' ## We fit the models for Germany:
#' GNIPDataDEagg <- queryGNIP(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(iso.data = GNIPDataDEagg,
#'                     mean.model.fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' ## We build the isoscapes:
#' isoscapes <- isoscape(elevation.raster = ElevRasterDE, isofit = GermanFit)
#' 
#' ## We create a simulated dataset with 25 sites and 5 observations per site:
#' Aliens <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'                         isoscape = isoscapes,
#'                         elevation_raster = ElevRasterDE,
#'                         n_sites = 25,
#'                         min_n_samples = 5,
#'                         max_n_samples = 5)
#' 
#' ## We display the simulated dataset:
#' Aliens
#' 
#' ## We plot the relationship between the environmental isotope values
#' ## and those from the simulated organisms:
#' plot(tissue.value ~ env.value, data = Aliens, ylab = "Tissue", xlab = "Environment")
#' abline(3, 0.5, col = "blue") ## the true relationship
#' 
#' 
#' @export
create_aliens <- function(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
                          isoscape = NULL,
                          elevation_raster = NULL,
                          n_sites = 1,
                          min_n_samples = 1,
                          max_n_samples = 10) {
  
  ## Complete the arguments
  .CompleteArgs(create_aliens)  
  
  ## Choose location for the aliens
  LocationData <- data.frame(siteID = sample(1:raster::ncell(isoscape$isoscape$mean), n_sites, replace = FALSE))
  xy <- raster::xyFromCell(isoscape$isoscape$mean, LocationData$siteID)
  LocationData$long <- xy[, "x"]
  LocationData$lat  <- xy[, "y"]
  LocationData$elev = raster::extract(x = elevation_raster, y = xy)
  LocationData$n.samples <- round(stats::runif(n = n_sites, min = min_n_samples, max = max_n_samples))
  
  ## Predict environmental values at the locations
  LocationData$env.value <- raster::extract(isoscape$isoscape$mean, xy)
 
  ## Replicate the dataset per animal
  AlienData <- LocationData[rep(1:nrow(LocationData), times = LocationData$n.samples), ]
  AlienData$animalID <- paste("Alien", 1:nrow(AlienData), sep = "_")
  
  ## Predict the tissue value for each animal
  AlienData$tissue.value <- stats::rnorm(n = nrow(AlienData),
                                         mean = rep(calib_fn$intercept + LocationData$env.value * calib_fn$slope,
                                                    times = LocationData$n.samples),
                                         sd = sqrt(calib_fn$resid_var))
  ## Cleanup and return
  rownames(AlienData) <- NULL
  return(AlienData[, c("siteID", "long", "lat", "elev", "animalID", "env.value", "tissue.value")])
}

