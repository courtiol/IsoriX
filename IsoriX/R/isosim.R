#' Simulate isotopic values
#' 
#' This function allows for the simulation of isoscapes. Both partial or
#' complete (i.e. maps) isoscapes can be simulated.
#' 
#' This function takes as inputs the values for all covariates matching a
#' series of locations (which can be provided as an structural raster or as a
#' *data.frame*), as well as the parameters of the isoscape model. The
#' function is not required to fit an isoscape, nor to perform assignments. It
#' is an additional function that can be useful to test the method, and to
#' study the effect of different parameters on isoscapes. We chose default
#' values for parameters inspired to model fits in order to simulate a
#' relatively realistic isoscape. We say 'relatively' because, in the
#' simulations, the Matérn realizations for the mean and the dispersion are
#' drawn independently, which is not the case in nature. Note that extra
#' parameters present in the input lists will not make the function crash but
#' won't be considered during computations either.
#' 
#' @param data A *data.frame* containing the covariates needed for the
#' simulation, or alternatively a structural raster of class *SpatRaster*
#' @param mean_model_fix_coef A *vector* of coefficients for fixed-effects
#' @param disp_model_fix_coef A *vector* of coefficients for fixed-effects
#' @param mean_model_matern_coef A *vector* of coefficients for the spatial
#' random effect
#' @param disp_model_matern_coef A *vector* of coefficients for the spatial
#' random effect
#' @param mean_model_uncorr_coef A *vector* of coefficients for the
#' uncorrelated random effect
#' @param disp_model_uncorr_coef A *vector* of coefficients for the
#' uncorrelated random effect
#' @param dist_method A *string* indicating the distance method
#' @param seed An *integer* used to set the seed of the random generator
#' @param save_dataframe A *logical* indicating whether the detailed
#' *data.frame* containing the simulated data should be saved
#' @param verbose A *logical* indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is `TRUE` if users use an interactive R
#' session and `FALSE` otherwise.
#' @return This function returns a *list* of class *ISOSCAPE*
#' containing a set of raster and an optional *data.frame*. The set
#' contains the raster `mean_raster` storing the mean isotopic value, and
#' the raster \cr `disp_raster` storing the residual dispersion variance.
#' The optional *data.frame* contains the simulated data and details of the
#' computation in an object called `data`.
#' @note The spatial autocorrelation and the Nugget are computed by the
#' functions `RandomFields::RMwhittle` and
#' `RandomFields::RMnugget`, respectively. These two functions are
#' part of the powerful package \pkg{RandomFields} (currently retired from CRAN).
#' @seealso [isofit] for the function fitting the isoscape model
#' 
#' [IsoriX] for the complete work-flow
#' @keywords simulate simulation
#' @examples
#' 
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 60) {
#' 
#' ## We simulate data under default settings
#' Simu <- isosim(data = ElevRasterDE,
#'                save_dataframe = TRUE,
#'                seed = 1)
#' 
#' Simu
#' 
#' ## We build the plots of the outcome using IsoriX
#' PlotMeanSimu <- plot(x = Simu, which = "mean")
#' 
#' PlotDispSimu <- plot(x = Simu, which = "disp")
#' 
#' 
#' ## We fit the simulated data by sampling 50 locations
#' 
#' set.seed(123)
#' Newdat <- Simu$data[sample(1:nrow(Simu$data), 50), ]
#' 
#' NewdatFit <- isofit(data = Newdat,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' Isoscape <- isoscape(ElevRasterDE, NewdatFit)
#' 
#' PlotMeanFitted <- plot(x = Isoscape, which = "mean", sources = list(draw = FALSE))
#' 
#' PlotDispFitted <- plot(x = Isoscape, which = "disp", sources = list(draw = FALSE))
#' 
#' ## We compare simulated and fitted data visually
#' print(PlotMeanSimu, split = c(1, 1, 2, 2), more = TRUE)
#' print(PlotDispSimu, split = c(2, 1, 2, 2), more = TRUE)
#' print(PlotMeanFitted, split = c(1, 2, 2, 2), more = TRUE)
#' print(PlotDispFitted, split = c(2, 2, 2, 2), more = FALSE)
#' 
#' ## We compare simulated and fitted data numerically
#' ## Note that differences are expected, as the geographic
#' ##   area is small compared to the scale at which
#' ##   spatial auto-correlation occurs
#' round(cbind(
#'     simulated = c(
#'         intercept = 64,
#'         lat_abs = -2.3, 
#'         elev = -0.01,
#'         nu = 0.35,
#'         rho = 5e-5,
#'         rho_div_nu  =  5e-5/0.35,
#'         lambda_ID = 0,
#'         lambda_matern = 899,
#'         intercept_disp = 5.8,
#'         nu_disp = 3.2e-01,
#'         rho_disp = 1.5e-05,
#'         lambda_matern_source_ID = 0,
#'         lambda_matern_disp = 5),
#'     fitted = c(
#'         intercept = NewdatFit$mean_fit$fixef[1],
#'         lat.abs = NewdatFit$mean_fit$fixef[2], 
#'         elev = NewdatFit$mean_fit$fixef[3],
#'         nu = get_ranPars(NewdatFit$mean_fit, which = "corrPars")[[1]]$nu,
#'         rho = get_ranPars(NewdatFit$mean_fit, which = "corrPars")[[1]]$rho,
#'         rho_div_nu = with(get_ranPars(NewdatFit$mean_fit, which = "corrPars")[[1]],rho/nu),
#'         lambda.matern = NewdatFit$mean_fit$lambda,
#'         intercept.disp = NewdatFit$disp_fit$fixef[1],
#'         nu.disp = get_ranPars(NewdatFit$disp_fit, which = "corrPars")[[1]]$nu,
#'         rho.disp = get_ranPars(NewdatFit$disp_fit, which = "corrPars")[[1]]$rho,
#'         lambda.matern.disp = NewdatFit$disp_fit$lambda)), 4)
#' 
#' }
#' 
#' @export
isosim <- function(data,
                   mean_model_fix_coef = c(intercept = 64, elev = -0.01, lat_abs = -2.3, lat_2 = 0, long = 0, long_2 = 0),
                   disp_model_fix_coef = c(intercept = 5.8, elev = 0, lat_abs = 0, lat_2 = 0, long = 0, long_2 = 0),
                   mean_model_matern_coef = c(nu = 0.35, rho = 5e-5, lambda = 899),
                   disp_model_matern_coef = c(nu = 3.2e-01, rho = 1.5e-05, lambda = 5),
                   mean_model_uncorr_coef = c(nugget = 0, lambda = 0),
                   disp_model_uncorr_coef = c(nugget = 0, lambda = 0),
                   dist_method = "Earth",
                   seed = NULL,
                   save_dataframe = FALSE,
                   verbose = interactive()) {

  if (!requireNamespace("RandomFields", quietly = TRUE)) {
    stop("The package 'RandomFields' is needed for this function,
    you could install it by typing install.packages('RandomFields'),
    but the package is unfortunately currently retired...")
  }
  
  ## if data is a raster, we convert it as data.frame
  if (inherits(data, "SpatRaster")) {
    raster <- data
    coord <- terra::crds(raster, na.rm = FALSE)
    data <- data.frame(long = coord[, "x"],
                       long_2 = coord[, "x"]^2,
                       lat = coord[, "y"],
                       lat_2 = coord[, "y"]^2,
                       lat_abs = abs(coord[, "y"]),
                       elev = terra::extract(raster, coord),
                       n_source_value = rep(1e6, nrow(coord)),
                       source_ID = as.factor(paste("simu", 1:nrow(coord), sep = "_")))
    rm(coord); gc() ## remove coord as it can be a large object
  }

  ## test if inputs are correct
  if (!any(dist_method %in% c("Earth", "Euclidean"))) {
    stop("The argument you chose for 'dist_method' is unknown.")
  }

  if (sum(mean_model_uncorr_coef > 0) > 1 | sum(disp_model_uncorr_coef > 0) > 1) {
    stop("The arguments 'mean_model_uncorr_coef' and 'disp_model_uncorr_coef' must each have at least one coefficient equals to zero (considering two parameterizations of uncorrelated random effects does not make any sense.)")
  }

  if (any(c(mean_model_uncorr_coef, disp_model_uncorr_coef) < 0)) {
    stop("All the nugget or lambda coefficients must be null or positive.")
  }

  if (any(c(mean_model_matern_coef, disp_model_matern_coef) < 0)) {
    stop("All spatial coefficients must be null or positive.")
  }

  data_needed <- names(c(mean_model_fix_coef, disp_model_fix_coef))
  data_needed <- data_needed[data_needed != "intercept"]

  if (!all(data_needed %in% names(data))) {
    stop(paste(c("You need to provide all the following covariates in data (even if the coefficients associated to the missing ones are null):", data_needed, "."), collapse = " "))
  }

  ## define the seeds (one for RandomFields, one for other R functions) and set other options for RandomFields
  set.seed(seed)
  RandomFields::RFoptions(seed = ifelse(is.null(seed), NA, seed),
                          spConform = FALSE,  ##so that RFsimulte returns vector directly
                          cPrintlevel = 1)  ##cPrintlevel = 3 for more details

  if (dist_method == "Earth") {
    RandomFields::RFoptions(new_coord_sys = "earth")
    }
  if (dist_method == "Euclidean") {
    RandomFields::RFoptions(new_coord_sys = "cartesian")
  }
  ## see ?"coordinate system" for info about RF coordinate system


  ### Simulate the dispersion

  if (verbose) {
    print("Simulating the dispersion...")
  }

  ## compute the linear predictor
  LinpredDisp <- .linear_predictor(fix_coef = disp_model_fix_coef,
                           matern_coef = disp_model_matern_coef,
                           uncorr_coef = disp_model_uncorr_coef,
                           data = data)

  data$disp_logvar_fix <- LinpredDisp$fix
  data$disp_logvar_matern <- LinpredDisp$matern
  data$disp_logvar_uncorr <- LinpredDisp$uncorr
  data$disp_mean <- exp(LinpredDisp$eta_sum)

  ## add residual variance
  data$var_source_value <- stats::rgamma(nrow(data),
                                                shape = (data$disp_mean^2)/2,
                                                scale = 2/data$disp_mean)

  ### Simulate the mean
  if (verbose) {
    print("Simulating the mean...")
  }

  ## compute the linear predictor
  MeanLinpred <- .linear_predictor(fix_coef = mean_model_fix_coef,
                           matern_coef = mean_model_matern_coef,
                           uncorr_coef = mean_model_uncorr_coef,
                           data = data)

  data$mean_var_fix <- MeanLinpred$fix
  data$mean_var_matern <- MeanLinpred$matern
  data$mean_var_uncorr <- MeanLinpred$uncorr
  data$mean_source_value <- MeanLinpred$eta_sum


  ### Building rasters
  if (verbose) {
    print("Building rasters...")
  }

  save_raster <- function(x){
    with(data, {
      .create_raster(long = long,
                    lat = lat,
                    values = get(x),
                    proj = "+proj=longlat +datum=WGS84")
    })
  }

  mean_raster <- save_raster("mean_source_value")
  disp_raster <- save_raster("var_source_value")

  ### Buidling return object
  out <- list()

  out$isoscapes <- terra::rast(list("mean" = mean_raster,
                                    "disp" = disp_raster))

  if (!save_dataframe & interactive()) {
    message(paste("Note: simulated data not saved as data.frame (save_dataframe is set to FALSE). Saving the simulated data as data.frame would require", format(utils::object.size(data), units = "MB")))
  } else {
    out$data <- data
  }
    
  class(out) <- c("ISOSCAPE", "ISOSIM", "list")

  return(out)
}


.linear_predictor <- function(fix_coef, matern_coef, uncorr_coef, data) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It builds the linear predictor for the simulations
  
  if (!requireNamespace("RandomFields", quietly = TRUE)) {
    stop("the package 'RandomFields' is needed for this function,
    you can install it by typing install.packages('RandomFields')")
  }
  
  ## fixed effects
  fix <- with(as.list(fix_coef), intercept +
                elev*data$elev + lat_abs*data$lat_abs + lat_2*data$lat_2 +
                long*data$long + long_2*data$long_2)

  ## spatial random effects
  matern <- 0
  if (matern_coef["lambda"] > 0) {
    model_matern <- with(as.list(matern_coef),
                         RandomFields::RMwhittle(nu = nu, var = lambda, scale = 1/rho))
    matern <- RandomFields::RFsimulate(model_matern,
                                       x = data$long, y = data$lat)
  }

  ## uncorr random effects
  uncorr <- stats::rnorm(nrow(data), mean = 0, sd = sqrt(uncorr_coef["lambda"]))
  if (uncorr_coef["nugget"] > 0) {
    uncorr <- uncorr + RandomFields::RFsimulate(RandomFields::RMnugget(var = uncorr_coef["nugget"]),
                                                x = data$long, y = data$lat)
  }
  eta_sum <- fix + matern + uncorr
  return(list("fix" = fix, "matern" = matern, "uncorr" = uncorr, "eta_sum" = eta_sum))
}
