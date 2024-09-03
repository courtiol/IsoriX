#' Simulate datasets for calibrations or assignments
#'
#' This function allows to simulate data so to provide examples for the
#' calibration and for the assignment procedure. We name the simulated
#' individuals 'Aliens' so to make it clear that the data we use to illustrate
#' our package are not real data.
#'
#' The isostopic values for the organisms are assumed to be linearly related to
#' the one from the environment. The linear function can be parametrized using
#' the first argument of the function (`calib_fn`). With this function the user
#' can simulate data for different sites.
#'
#' The number and locations of sites can be controlled in two ways. A first
#' possibility is to use the argument `n_sites`. The sites will then be selected
#' randomly among the locations present in the isoscape (argument `isoscape`)
#' provided to this function. An alternative possibility is to provide a data
#' frame containing three columns (`site_ID`, `long` and `lat`) to input the
#' coordinate of the sampling site manually.
#'
#' Irrespective of how locations are chosen, a random number of observations
#' will be drawn, at each site, according to a uniform distribution bounded by
#' the values of the argument `min_n_samples` and `max_n_samples`.
#'
#' From the selected coordinates, the isotope values for the environment are
#' directly extracted from the corresponding point predictions stored in the
#' isoscape object. No uncertainty is considered during this process. Then the
#' linear calibration defines the means of the isotope values for the simulated
#' organisms. The actual values is then drawn from a Gaussian distribution
#' centred around such mean and a variance defined by the residual variance
#' (`resid_var`) input within the list `calib_fn`.
#'
#' @param calib_fn A *list* containing the parameter values describing the
#'   relationship between the isotope values in the environment and those in the
#'   simulated organisms. This list must contain three parameters: the
#'   intercept, the slope, and the residual variance.
#'
#' @param isoscape The output of the function [`isoscape`]
#'
#' @param coordinates An optional *data.frame* with columns `site_ID`,
#' `long` and `lat`
#'
#' @param raster A *SpatRaster* containing an elevation raster
#'
#' @param n_sites The number of sites from which the simulated organisms
#'   originate (*integer*)
#'
#' @param min_n_samples The minimal number of observations (*integer*) per
#'   site
#'
#' @param max_n_samples The maximal number of observations (*integer*) per
#'   site
#'
#' @return This functions returns a *data.frame* (see example for column
#'   names)
#'
#' @seealso [`calibfit`] for a calibration based on simulated data
#'
#'   [`isofind`] for an assignment based on simulated data
#'
#'   [`IsoriX`] for the complete work-flow of our package
#' @keywords simulate simulation
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## We fit the models for Germany
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   GermanFit <- isofit(data = GNIPDataDEagg)
#'
#'   ## We build the isoscapes
#'   GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#'   ## We create a simulated dataset with 25 sites and 5 observations per site
#'   Aliens <- create_aliens(
#'     calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'     isoscape = GermanScape,
#'     raster = ElevRasterDE,
#'     n_sites = 25,
#'     min_n_samples = 5,
#'     max_n_samples = 5
#'   )
#'
#'   ## We display the simulated dataset
#'   Aliens
#'
#'   ## We plot the relationship between the environmental isotope values
#'   ## and those from the simulated organisms
#'   plot(sample_value ~ source_value, data = Aliens, ylab = "Tissue", xlab = "Environment")
#'   abline(3, 0.5, col = "blue") ## the true relationship
#'
#'   ## We create a simulated dataset with 2 sites imputing coordinates manually
#'   Aliens2 <- create_aliens(
#'     calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'     isoscape = GermanScape,
#'     coordinates = data.frame(
#'       site_ID = c("Berlin", "Bielefeld"),
#'       long = c(13.52134, 8.49914),
#'       lat = c(52.50598, 52.03485)
#'     ),
#'     raster = ElevRasterDE,
#'     min_n_samples = 5,
#'     max_n_samples = 5
#'   )
#'
#'   Aliens2
#' }
#'
#' @export
create_aliens <- function(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
                          isoscape = NULL,
                          coordinates = NA,
                          raster = NULL,
                          n_sites = NA,
                          min_n_samples = 1,
                          max_n_samples = 10) {
  ## Complete the arguments
  .complete_args(create_aliens)

  ## Choose location for the aliens
  if (length(coordinates) == 1 && is.na(coordinates)) {
    LocationData <- data.frame(site_ID = sample(1:terra::ncell(isoscape$isoscapes$mean), n_sites, replace = FALSE))
    xy <- terra::xyFromCell(isoscape$isoscapes$mean, LocationData$site_ID)
    LocationData$long <- xy[, "x"]
    LocationData$lat <- xy[, "y"]
  } else {
    if (!all(c("site_ID", "long", "lat") %in% colnames(coordinates))) {
      stop("the argument coordinates must contain the columns 'site_ID', 'long' and 'lat'")
    }
    if (!is.na(n_sites)) {
      warnings("the argument coordinates has been used so the argument 'n_sites' has been ignored")
    }
    LocationData <- coordinates
    xy <- coordinates[, c("long", "lat")]
    n_sites <- nrow(coordinates)
  }
  LocationData$elev <- terra::extract(x = raster, y = xy)[[1]]
  LocationData$n_samples <- round(stats::runif(n = n_sites, min = min_n_samples, max = max_n_samples))

  ## Predict environmental values at the locations
  LocationData$source_value <- terra::extract(isoscape$isoscapes$mean, xy)[[1]]

  ## Replicate the dataset per animal
  AlienData <- LocationData[rep(seq_len(nrow(LocationData)), times = LocationData$n_samples), ]
  AlienData$sample_ID <- factor(paste("Alien", seq_len(nrow(AlienData)), sep = "_"))

  ## Turning site_ID into a factor
  AlienData$site_ID <- factor(AlienData$site_ID)

  ## Predict the tissue value for each animal
  AlienData$sample_value <- stats::rnorm(
    n = nrow(AlienData),
    mean = calib_fn$intercept + AlienData$source_value * calib_fn$slope,
    sd = sqrt(calib_fn$resid_var)
  )
  ## Cleanup and return
  rownames(AlienData) <- NULL
  return(AlienData[, c("site_ID", "long", "lat", "elev", "sample_ID", "source_value", "sample_value")])
}
