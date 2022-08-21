#' Fit the calibration model (or load parameters from calibration done outside IsoriX)
#'
#' This function establishes the relationship between the isotopic values of
#' organisms (e.g. tissues such as hair, horn, ivory or feathers; referred in
#' code as \var{sample_value}) and the isotopic values of their environment
#' (e.g. precipitation water; referred in code as \var{source_value}). This
#' function is only needed when the assignment of organisms has to be performed
#' within an isoscape that was not built using the organisms themselves, but
#' that was instead built using another source of isotopic values (e.g.,
#' precipitation). If the isoscape had been fitted using isotopic ratios from
#' sedentary animals directly, this calibration step is not needed (e.g.
#' isoscape fitted using sedentary butterflies and migratory butterflies to
#' assign). In other cases, this calibration step is usually needed since
#' organism may not directly reflect the isotopic values of their environment.
#' Depending on the calibration data to be used (provided via the argument
#' \code{data}), one of three possible calibration method must be selected (via
#' the argument \code{method}). Each method considers a different statistical
#' model and requires particular data that are organised in a specific way (see
#' **Details** for explanations and **Examples** for use cases).
#'
#' Calibration can be performed according to one of three different methods and
#' it is crucial for the users to select the method that is most appropriate for
#' their workflow. The methods are labelled "wild", "lab" and "desk". One of
#' this term must thus be used to define the argument \code{method}. If no term
#' is used, the default that is selected is the method referred to as "wild".
#' Importantly, the choice of the method can impact the most likely
#' assignment locations during the assignment test performed in 
#' \code{\link{isofind}}.
#'
#' ## Method "wild"
#'
#' This calibration method is the one to be used when the calibration data to be
#' used correspond to isotopic values measured on sedentary organisms and when
#' no direct measurement of isotopic values in the environment are available at
#' the locations where sedentary organisms have been collected. In such a case,
#' the isotopic values in the environment of sedentary organisms are predicted
#' internally using an isoscape fitted with \code{\link{isofit}}. This
#' calibration method thus aim at estimating and accounting for the uncertainty
#' associated with these predicted values. Such uncertainty is accounted for
#' when fitting the calibration fit so as to produce an unbiased estimation of
#' the calibration relationship and it is also then accounted for by
#' \code{\link{isofind}} when inferring the possible locations of origin. Before
#' we added the argument \code{method} in \code{calibfit} (i.e. before releasing
#' the version 0.9), this method was the only one available in IsoriX.
#'
#' - **Statistical model**: in this case, the calibration model to be fitted is
#' a linear mixed-effects model (LMM) that fits the isotopic values of sedentary
#' organisms as a linear function of the isotopic values in their environment
#' (e.g. precipitation). The function considers that the isotopic values from
#' the environment (e.g. from precipitation) at the locations at which organisms
#' were sampled are not known. The function therefore predicts these isotopic
#' values from the geostatistical model fitted by the function
#' \code{\link{isofit}}, which is provided to \code{calibfit} using the argument
#' \code{isofit}. The LMM used to fit the calibration function has a simple
#' fixed-effect structure: an intercept and a slope. The random effect is more
#' complex: it is normally distributed with mean zero, a certain variance
#' between locations proportional to the squared (fixed) slope, and a covariance
#' structure defined by the prediction covariance matrix of the isoscape model
#' between the calibration locations. See appendix in Courtiol et al. 2019 for
#' more details.

#' - **Required calibration data**: the calibration data to be used here must be
#' a dataframe (or a tibble) containing at least the following columns:
#'    - \code{sample_value}: the isotopic value of the calibration sample
#'    - \code{long}: the longitude coordinate (decimal degrees)
#'    - \code{lat}: the latitude coordinate (decimal degrees)
#'    - \code{site_ID}: the sample site
#'
#'    The column name must be identical to those indicated here. Other columns
#'    can be present in the data but won't be used. Each row must correspond to
#'    a different calibration sample (i.e. a single isotopic measurement). See
#'    \code{\link{CalibDataAlien}}, \code{\link{CalibDataBat}}, or
#'    \code{\link{CalibDataBat2}} for examples of such a dataset.
#' 
#' ## Method "lab"
#' 
#' This calibration method is the one to be used when the calibration data to be
#' used correspond to isotopic values recorded for both organisms and their
#' environment. This is the right method to be used when the data are generated
#' by growing organisms in a controlled environment where they are fed and/or
#' given water with a specific (known) isotopic value. This is also the method
#' to be used if sedentary organisms are sampled in the wild together with a
#' sample from their environment and that isotopic values have been measured for
#' both.
#' 
#' - **Statistical model**: in this case, the calibration model to be fitted is
#' a simple linear model (LM) or a simple linear mixed-effects model (LMM) that
#' fits the isotopic values of sedentary organisms as a linear function of the
#' isotopic values in their environment (e.g. precipitation). Whether it is a LM
#' or a LMM depends on the presence of a column \code{site_ID} in the dataset as
#' well as on the number of unique values for such a column. If the column is
#' present and the number of unique values is larger than 4, a LMM is fitted.
#' Otherwise, a LM is fitted. In both cases, the function considers that the
#' isotopic values from the environment (e.g. from precipitation) at the
#' locations at which organisms were sampled are known. Contrary to the method
#' "wild", the environment values are thus observed and not predicted from an
#' isoscape. The argument \code{isofit} should thus remain \code{NULL} in this
#' case (since no isoscape is used, no isoscape fit is required to perform the
#' calibration). The model used to fit the calibration function has a simple
#' fixed effect structure: an intercept and a slope.
#' 
#' - **Required calibration data**: the calibration data to be used here must be
#' a dataframe (or a tibble) containing at least the following columns:
#'    - \code{sample_value}: the isotopic value of the calibration sample
#'    - \code{source_value}: the isotopic value of the environment
#'    - \code{site_ID} (optional): the sample site
#'
#'    The column name must be identical to those indicated here. Other columns
#'    can be present in the data but won't be used. Each row must correspond to
#'    a different calibration sample (i.e. a single sample-environment pair of
#'    isotopic measurements).
#'
#' ## Method "desk"
#'
#' This calibration method is the one to be used when no calibration data is
#' available and that users want to rely on the slope and intercept (and, if
#' available, on the standard errors associated to them, and/or the residual
#' variance) of a calibration function they found elsewhere (e.g. in a
#' publication). Note that if the slope is set to 0 and an intercept is
#' considered, the calibration methods actually corresponds to the simple
#' consideration of a fractionation factor.
#'
#' This calibration method must only be used as a last resource! It is unlikely
#' to yield to robust inference during the assignment step. The more argument
#' provided the better, but even if standard errors and the residual variance
#' are defined, the function won't achieve anything close to a good job. This is
#' because relying solely on the simple metrics reported in publications or by
#' software implies to neglect various sources of uncertainty. In particular,
#' even if 5 parameters listed below are provided, the assignment will still be
#' forced to assume that documented uncertainties are independent from each
#' others. Making these simplifying assumptions comes with a cost: it can bias
#' the assignment or pretend that they are more accurate than they really are.
#' For these reasons, we were tempted to use \code{method = "dirty"} instead of
#' \code{method = "desk"}... but we chickened out since we predicted that users
#' would then refrain from mentioning the method they used in publications...
#' 
#' - **Statistical model**: none!
#' 
#' - **Required calibration data**: the calibration data to be used here must be
#' a dataframe (or a tibble) containing a single row with the following columns:
#'    - \code{intercept}: the estimated slope of a fit of the form
#'    \code{lm(sample_value ~ source_value)}
#'    - \code{slope}: the estimated slope of a fit of the form
#'    \code{lm(sample_value ~ source_value)}
#'    - \code{intercept_se} (optional): the standard error around the intercept
#'    - \code{slope_se} (optional): the standard error around the slope
#'    - \code{resid_var} (optional): the residual variance (not SD) of a fit of 
#'    the form \code{lm(sample_value ~ source_value)}
#' 
#' @aliases calibfit print.CALIBFIT summary.CALIBFIT
#' @inheritParams isoscape
#' @param data A \var{dataframe} containing the calibration data (see note
#'   below)
#' @param method A \var{string} indicating the method used to generate the data
#'   used for the calibration. By default method is \var{"wild"}, but the
#'   other possible values are \var{"lab"} and \var{"desk"}. See **Details** for
#'   the difference between these three methods. 
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \var{TRUE} if users run an interactive R
#'   session and \var{FALSE} otherwise.
#' @param control_optim A \var{list} to pass information to the argument control
#'   in the call to \code{\link{optim}} (only effective when \code{method =
#'   "wild"}; for advanced users only).
#' @return This function returns a \var{list} of class \var{CALIBFIT} containing
#'   the name of the calibration method used, whether a species_ID random effect
#'   was estimated, whether a site_ID random effect was estimated, the
#'   fixed-effect estimates of the calibration function, the covariance of the
#'   fixed effects, the residual variance of the calibration fit, the fitted
#'   calibration model (if applicable), the original calibration data set with
#'   additional information added during the fit, and the location of the
#'   calibration points as spatial points.
#' @seealso see \code{\link{plot}} for the help on how to plot the calibration
#' relationship.
#' @keywords models regression
#' @references Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC,
#'   Hobson KA, Wassenaar LI, Kramer-Schadt S (2019). Isoscape computation and
#'   inference of spatial origins with mixed models using the R package IsoriX.
#'   In Hobson KA, Wassenaar LI (eds.), Tracking Animal Migration with Stable
#'   Isotopes, second edition. Academic Press, London.
#'   
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if (getOption_IsoriX("example_maxtime") > 30) {
#' 
#' #####################################################
#' ## 1 Example of calibration using the method "wild" #
#' #####################################################
#' 
#' ## 1.1 We prepare the data to fit the isoscape:
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ## 1.2 We fit the isoscape models for Germany:
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' ## 1.3 We fit the calibration model using the method "wild" (the default):
#' CalibAlien <- calibfit(data = CalibDataAlien, isofit = GermanFit)
#' 
#' ## 1.4 We explore the outcome of the calibration:
#' CalibAlien
#' summary(CalibAlien)
#' plot(CalibAlien)
#' 
#' ## Note: you can plot several calibrations at once (using bats this time):
#' CalibBat1 <- calibfit(data = CalibDataBat, isofit = GermanFit)
#' CalibBat2 <- calibfit(data = CalibDataBat2, isofit = GermanFit)
#' plot(CalibBat1)
#' points(CalibBat2, pch = 3, col = "red", CI = list(col = "green"))
#'  
#'
#' ####################################################
#' ## 2 Example of calibration using the method "lab" #
#' ####################################################
#' 
#' ## 2.0 We create made up data here because we don't have yet a good dataset
#' ## for this case, but you should use your own data instead:
#' GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#' set.seed(123)
#' CalibDataAlien2 <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5,
#'                                                  resid_var = 5),
#'                                  isoscape = GermanScape,
#'                                  raster = ElevRasterDE,
#'                                  n_sites = 25,
#'                                  min_n_samples = 5,
#'                                  max_n_samples = 5)
#' CalibDataAlien2 <- CalibDataAlien2[, c("site_ID", "sample_ID", "source_value", 
#'                                        "sample_value")]
#' head(CalibDataAlien2) ## your data should have this structure
#' 
#' ## 2.1 We fit the calibration model using the method "lab": 
#' CalibAlien2 <- calibfit(data = CalibDataAlien2, method = "lab")
#'                        
#' ## 2.2 We explore the outcome of the calibration:
#' CalibAlien2
#' summary(CalibAlien2)
#' plot(CalibAlien2)
#' 
#' 
#' #####################################################
#' ## 3 Example of calibration using the method "desk" #
#' #####################################################
#' 
#' ## 3.1 We format the information about the calibration function to be used
#' ## as a dataframe:
#' CalibDataAlien3 <- data.frame(intercept = 3.69, slope = 0.8,
#'                               intercept_se = 0.4, slope_se = 0.05,
#'                               resid_var = 4.2)
#' CalibDataAlien3
#'                
#' ## 3.2 We fit the calibration model using the method "desk":
#' CalibAlien3 <- calibfit(data = CalibDataAlien3, method = "desk")
#' 
#' ## 3.3 We explore the outcome of the calibration:
#' CalibAlien3
#' summary(CalibAlien3)
#' plot(CalibAlien3, xlim = c(-100, 100), ylim = c(-50, 50))
#' 
#' ## Note: the desk function also work with just intercept and slope:
#' CalibDataAlien4 <- CalibDataAlien3[, c("intercept", "slope")]
#' CalibAlien4 <- calibfit(data = CalibDataAlien4, method = "desk")
#' CalibAlien4
#' summary(CalibAlien4)
#' plot(CalibAlien3, xlim = c(-100, 100), ylim = c(-50, 50))
#' points(CalibAlien4, line = list(col = "orange"))
#' ## Regression lines are the same, but the new calibration does not have a
#' ## confidence intervals since we provided no uncertainty measure in 
#' ## CalibDataAlien4, which will make a difference during assignments...
#' 
#' }
#' 
#' @export
calibfit <- function(data,
                     isofit = NULL,
                     method = c("wild", "lab", "desk"),
                     verbose = interactive(),
                     control_optim = list()
                     ) {

    ## checking inputs
    if (inherits(isofit, "MULTIISOFIT")) {
      stop("object 'isofit' of class MULTIISOFIT; calibration have not yet been implemented for this situation.")
    }
    
    method <- match.arg(method, c("wild", "lab", "desk"))
    
    ## Note: part of the code is prepared to use species_rand as an argument (with NULL = automatic selection)
    ## That would allow to fit species as a random effect in the model
    ## However, it is not obvious that it would make sense to do that as it may
    ## remove variance that should be captured during the assignment.
    ## We thus disregard this term for now.
    species_rand <- FALSE
    
    species_info <- "species_ID" %in% colnames(data)
    if (!is.null(species_rand)) {
      if (!species_info && species_rand) {
        stop("The random effect for species cannot be fit if data does not contain a column called species_ID")
      }
    }
    
    ## prepare the dataset
    data <- .prepare_data_calib(data, method = method)

    ## set species_rand (not used for now)
    if (!species_info) {
      species_rand <- FALSE
    } else {
      nb_species <- length(unique(data$species_ID))
      species_rand <- ifelse(is.null(species_rand) && nb_species > 4, TRUE, FALSE)
    }

  ## apply the calibration method
  result_calib <- switch(method,
                         wild = .calibfit_wild(data = data,
                                               isofit = isofit,
                                               species_rand = species_rand,
                                               verbose = verbose,
                                               control_optim = control_optim),
                         lab = .calibfit_lab(data = data,
                                             species_rand = species_rand,
                                             verbose = verbose),
                         desk = .calibfit_desk(data = data, verbose = verbose))
    
  class(result_calib) <- c("CALIBFIT", "list")

  return(invisible(result_calib))
}

.prepare_data_calib <- function(data, method, weighting = NULL) {
  ## This function should not be called by the user.
  ## It prepares data for the calibration procedure.
  
  if (method == "wild") {
    
    ## Checking the inputs
    if (!all(c("lat", "long") %in% colnames(data))) {
      stop("The dataset does not seem to contain the required variable(s) 'lat' and/or 'long'.")
    } 
    if (is.null(data$sample_value)) {
      stop("The dataset does not seem to contain the required variable 'sample_value'.")
    }
    if (is.null(data$site_ID)) {
      stop("The dataset does not seem to contain the required variable 'site_ID'.")
    }
    
    ## Preparing the inputs
    data$site_ID <- factor(data$site_ID)
    data$lat_abs <- abs(data$lat)
    data$lat_2 <- data$lat^2
    data$long_2 <- data$long^2
    data$source_ID <- as.factor(paste("new", data$site_ID, sep = "_"))
    }
  
  else if (method == "lab") {
    
    ## Checking the inputs
    if (is.null(data$sample_value)) {
      stop("The dataset does not seem to contain the required variable 'sample_value'.")
    }
    if (is.null(data$source_value)) {
      stop("The dataset does not seem to contain the required variable 'source_value'.")
    }
    
    ## Preparing the inputs
    if (!is.null(data$site_ID)) {
      data$site_ID <- factor(data$site_ID)
    }
    }
  
  else if (method == "desk") {
    
    ## Checking the inputs
    if (is.null(data$intercept)) {
      stop("The dataset does not seem to contain the required variable 'intercept'.")
    }
    if (is.null(data$slope)) {
      stop("The dataset does not seem to contain the required variable 'slope'.")
    }
    if (nrow(data) > 1) {
      stop("The selected calibration method requires that data contains only a single row.")
    }

    ## Preparing the inputs
    ### No preparation needed
    }
  
  else {
    stop("method argument not recognised")
  }
  
  data <- droplevels(data)
  
  if (!is.null(weighting)) {
    precipitations <- raster::extract(weighting, cbind(data$long, data$lat))
    if (anyNA(precipitations)) {
      message("NA values in precipitation cannot be handled and were thus replaced by 0.0001. If this does not make sense in your case, please contact the maintainer of this package.")
      precipitations[is.na(precipitations)] <- 0.0001 ## remove NA to prevent crashes --> dangerous?
    }
    data <- cbind(data, precipitations)
  }
  return(data)
}

.calibfit_wild <- function(data,
                           isofit,
                           species_rand,
                           verbose = interactive(),
                           control_optim = list()) {
  ## This function should not be called by the user.
  ## It fits the calibration model according to the "wild" method.
  
  if (verbose) {
    print("starting calibration method 'wild'")
  }
  
  time <- system.time({
    ## predict isoscape and associated prediction
    ##   covariance matrix at animal locations
    
    if (verbose) {
      print("predicting the isoscape value in each calibration site...")
    }
    
    mean_calib <- spaMM::predict.HLfit(isofit[["mean_fit"]],
                                       newdata = data,
                                       variances = list(predVar = TRUE, cov = TRUE))
    
    ## store the mean prediction
    data$mean_source_value <- c(mean_calib)
    
    ## warns if extrapolation occurs
    too_small <- sum(min(data$mean_source_value, na.rm = TRUE) < min(isofit$info_fit$data$mean_source_value, na.rm = TRUE))
    too_large <- sum(max(data$mean_source_value, na.rm = TRUE) > max(isofit$info_fit$data$mean_source_value, na.rm = TRUE))
    if (too_small + too_large > 0) {
      message(
        paste("Tip:", too_small + too_large, "out of your", nrow(data),
          "calibration samples are associated to predicted values more extreme than the ones present in the isoscape. This corresponds to extrapolation during the calibration step. If the proportion becomes too large, it could imped the reliability of your assignments. In such cases, you should perhaps rethink the design of your isoscape and/or collect more callibration data within the expected range to avoid any problem."
        )
      )
    }
    
    ## extract the prediction covariance matrix
    predcov_matrix_isofit_full <- attr(mean_calib, "predVar")
    
    ## extract the prediction variances
    data$mean_predVar_source <- diag(predcov_matrix_isofit_full)
    
    ## reshape the prediction covariance matrix to number of unique sites
    firstoccurences <- match(levels(data$site_ID), data$site_ID)
    predcov_isofit <- predcov_matrix_isofit_full[firstoccurences, firstoccurences]
    rownames(predcov_isofit) <- levels(data$site_ID)
    colnames(predcov_isofit) <- levels(data$site_ID)
    
    ### fitting the calibration function
    if (verbose) {
      print("fitting the calibration function...")
    }
    
    ## Defining the calibration function
    objective_fn_calib <- function(param,
                                   data,
                                   predcov,
                                   species_rand,
                                   return_fit = FALSE,
                                   lik_method = "REML") {
        ## This function computes the likelihood of a given calibration function
        data$intercept <- param[1]
        data$slope <- param[2]
        lambda_list <- list(lambda = c(1e-6 + unique(data$slope)^2, NA))
        calib_formula <-
          "sample_value ~ 0 + offset(intercept+slope*mean_source_value) + corrMatrix(1|site_ID) + (1|site_ID)"
        if (species_rand) {
          lambda_list$lambda <- c(lambda_list$lambda, NA)
          calib_formula <- paste(calib_formula, "+ (1|species_ID)")
        }
        calib_fit <-
          spaMM::HLCor(
            formula = stats::formula(calib_formula),
            corrMatrix = predcov,
            ranPars = lambda_list,
            data = data,
            method = lik_method
          )
        if (return_fit)
          return(calib_fit)
        return(calib_fit$APHLs$p_v)
      }
    
    ## estimation of intercept and slope of the calibration function
    opt_res <- stats::optim(
      par = c(0, 1),
      fn = objective_fn_calib,
      control = c(list(fnscale = -1), control_optim),
      data = data,
      predcov = predcov_isofit,
      species_rand = species_rand,
      lik_method = "REML"
    )
    
    param_calibfit <- opt_res$par
    names(param_calibfit) <- c("intercept", "slope")
    
    ## fit of the calibration function
    calib_fit <- objective_fn_calib(
      param = param_calibfit,
      data = data,
      predcov = predcov_isofit,
      species_rand = species_rand,
      lik_method = "REML",
      return_fit = TRUE
    )
    
    ## computing the covariance matrix of fixed effects
    if (verbose) {
      print("computing the covariance matrix of fixed effects...")
    }
    
    fixefCov_calibfit <- solve(
      -numDeriv::hessian(
        objective_fn_calib,
        param_calibfit,
        data = data,
        predcov = predcov_isofit,
        species_rand = species_rand,
        lik_method = "ML"
      )
    )
    
    rownames(fixefCov_calibfit) <- names(param_calibfit)
    colnames(fixefCov_calibfit) <- names(param_calibfit)
    
  }) ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("the calibration procedure based on", nrow(data), "calibration samples has been completed in", time, "sec."))
  }
  
  ## we create the spatial points for calibration points
  calib_points  <- .create_spatial_points(long = data$long,
                                          lat = data$lat,
                                          proj = "+proj=longlat +datum=WGS84")
  
  return(list("method" = "wild",
              "species_rand" = species_rand,
              "site_rand" = TRUE,
              "param" = param_calibfit,
              "fixefCov" = fixefCov_calibfit,
              "phi" = calib_fit$phi,
              "calib_fit" = calib_fit,
              "data" = data,
              "sp_points" = list(calibs = calib_points)))
}

.calibfit_lab <- function(data,
                          species_rand,
                          verbose = interactive()) {
  ## This function should not be called by the user.
  ## It fits the calibration model according to the "lab" method.
  
  if (verbose) {
    print("starting calibration method 'lab'")
  }
  
  time <- system.time({

    ## determine if site_ID random term needed
    if (!"site_ID" %in% colnames(data)) {
      site_rand <- FALSE
    } else {
      nb_sites <- length(unique(data$site_ID))
      site_rand <- ifelse(nb_sites > 4, TRUE, FALSE)
    }
    
    ### fitting the calibration function
    calib_formula <- "sample_value ~ source_value"
    
    if (species_rand) {
      calib_formula <- paste(calib_formula, "+ (1|species_ID)")
    }
    
    if (site_rand) {
      calib_formula <- paste(calib_formula, "+ (1|site_ID)")
    }
    
    calib_fit <- spaMM::fitme(
          formula = stats::formula(calib_formula),
          data = data,
          method = "REML")

    param_calibfit <- spaMM::fixef(calib_fit)
    names(param_calibfit) <- c("intercept", "slope")

    ## extracting the covariance matrix of fixed effects

    fixefCov_calibfit <- stats::vcov(calib_fit)
    
    rownames(fixefCov_calibfit) <- names(param_calibfit)
    colnames(fixefCov_calibfit) <- names(param_calibfit)
    
  }) ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("the calibration procedure based on", nrow(data), "calibration samples has been completed in", time, "sec."))
  }
  
  ## we create the spatial points for calibration points
  if (all(c("long", "lat") %in% colnames(data))) {
    calib_points  <- .create_spatial_points(long = data$long,
                                            lat = data$lat,
                                            proj = "+proj=longlat +datum=WGS84")
  } else {
    calib_points <- list()
  }
  
  return(list("method" = "lab",
              "species_rand" = species_rand,
              "site_rand" = site_rand,
              "param" = param_calibfit,
              "fixefCov" = fixefCov_calibfit,
              "phi" = calib_fit$phi,
              "calib_fit" = calib_fit,
              "data" = data,
              "sp_points" = list(calibs = calib_points)))
}

.calibfit_desk <- function(data,
                           verbose = interactive()) {
  ## This function should not be called by the user.
  ## It fits the calibration model according to the "desk" method (aka "dirty").
  
  message("Note: this calibration method is not recommended since it does not account for the required covariance terms needed to perform reliable assignments. See ?calibfit for details.")
  
  phi_calibfit <- ifelse(!is.null(data$resid_var), data$resid_var, 0)
  
  fixefCov_calibfit <- matrix(0, nrow = 2, ncol = 2)
  rownames(fixefCov_calibfit) <- c("intercept", "slope")
  colnames(fixefCov_calibfit) <- c("intercept", "slope")
  
  if (!is.null(data$intercept_se)) {
    fixefCov_calibfit[1, 1] <- data$intercept_se^2
  }
  
  if (!is.null(data$slope_se)) {
    fixefCov_calibfit[2, 2] <- data$slope_se^2
  }
  
  if (verbose) {
    print(paste("the calibration data have been loaded."))
  }
  
  return(list("method" = "desk",
              "species_rand" = FALSE,
              "site_rand" = FALSE,
              "param" = c(intercept = data$intercept, slope = data$slope),
              "fixefCov" = fixefCov_calibfit,
              "phi" = phi_calibfit,
              "calib_fit" = list(),
              "data" = data,
              "sp_points" = list()))
  
}

#' @export
#' @method print CALIBFIT
print.CALIBFIT <- function(x, ...) {
  cat("\n")
  cat("Fixed effect estimates of the calibration fit", "\n")
  if (x$method == "wild") {
    if (x$species_rand) {
      cat("sample_value = intercept + slope * mean_source_value + corrMatrix(1|site_ID) + slope^2 * (1|site_ID) + (1|species_ID) + Error", "\n")
    } else {
      cat("sample_value = intercept + slope * mean_source_value + corrMatrix(1|site_ID) + slope^2 * (1|site_ID) + Error", "\n")
    }
  } else if (x$method == "lab") {
    if (x$species_rand && x$site_rand) {
      cat("sample_value = intercept + slope * source_value + (1|species_ID) + (1|site_ID) + Error", "\n")
    } else if (!x$species_rand && x$site_rand) {
      cat("sample_value = intercept + slope * source_value + (1|site_ID) + Error", "\n")
    }  else if (x$species_rand && !x$site_rand) {
      cat("sample_value = intercept + slope * source_value + (1|species_ID) + Error", "\n")
    } else if (!x$species_rand && !x$site_rand) {
      cat("sample_value = intercept + slope * source_value + Error", "\n")
    }
  } else if (x$method == "desk") {
    cat("Estimates of the loaded calibration fit", "\n")
  } else {
    stop("method unknown")
  }
  cat("\n")
  cat(paste("           intercept (+/- SE) =", .print_nice_and_round(x$param["intercept"], 2),
            "+/-",  .print_nice_and_round(sqrt(x$fixefCov["intercept", "intercept"]), 2)), "\n")
  cat(paste("           slope     (+/- SE) =", .print_nice_and_round(x$param["slope"], 2),
            "+/-",  .print_nice_and_round(sqrt(x$fixefCov["slope", "slope"]), 2)), "\n")
  cat("\n")
  cat("[for more information, use summary()]", "\n")
  cat("\n")

  return(invisible(NULL))
}

#' @export
#' @method summary CALIBFIT
summary.CALIBFIT <- function(object, ...) {
  cat("\n")
  cat("Fixed effect estimates of the calibration fit", "\n")
  print(.print_nice_and_round(object$param, 3), quote = FALSE)
  cat("\n")
  cat("Covariance matrix of fixed effect estimates:", "\n")
  print(.print_nice_and_round(object$fixefCov, 3), quote = FALSE)
  cat("\n")
  cat("Residual variance of the calibration fit:", "\n")
  print(.print_nice_and_round(c(phi = object$phi), 3), quote = FALSE)
  cat("\n")
  if (object$method != "desk") {
    cat("#########################################################", "\n")
    cat("### spaMM summary of the fit of the calibration model ###", "\n")
    cat("#########################################################", "\n")
    cat("\n")
    print(spaMM::summary.HLfit(object$calib_fit)$summ)
    cat("\n")
    cat(paste("[model fitted with spaMM version ", object$calib_fit$spaMM.version, "]", sep = ""), "\n")
    cat("\n")
  }
  return(invisible(NULL))
}

