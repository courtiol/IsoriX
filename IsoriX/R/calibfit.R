#' Fit the calibration model
#' 
#' This function fits a model that establishes the relationship between the 
#' isotopic values of organisms (e.g. tissues such as hair, horn, ivory or 
#' feathers) and the isotopic values of their environment (e.g. precipitation).
#' 
#' The calibration model is a linear mixed-effects model (LMM) that fits the 
#' isotopic values of sedentary organisms as a linear function of the isotopic 
#' values in their environment (e.g. precipitation).
#' 
#' This function considers that the isotopic values from the environment (e.g.
#' from precipitation) at the locations at which organisms were sampled are not
#' known. The function therefore predicts these isotopic values from the
#' geostatistical model fitted by the function \code{\link{isofit}}, which is
#' provided to \code{\link{calibfit}} using the argument \code{isofit}.
#'
#' The LMM used to fit the calibration function has a simple fixed-effect
#' structure: an intercept and a slope. The random effect is more complex: it is
#' normally distributed with mean zero, a certain variance between locations
#' proportional to the squared fixed slope, and a covariance structure defined
#' by the prediction covariance matrix of the isoscape model between the
#' calibration locations. See appendix in Courtiol et al. 2019 for more details.
#'
#' This function is only needed in the case for which the assignment of
#' organisms has to be performed within an isoscape that was built using another
#' source of isotopic values (e.g., precipitation). This implies that if the
#' isoscape had been fitted using isotopic ratios from sedentary animals, then
#' this calibration step is not needed.
#'
#' If source isotopic values are known at the locations where sedentary
#' organisms were sampled, users should calibrate their data directly using the
#' function \code{\link[stats]{lm}} by fitting tissue isotopic values as a
#' function of source isotopic values.
#'
#' @aliases calibfit print.CALIBFIT summary.CALIBFIT
#' @inheritParams isoscape
#' @param data A \var{dataframe} containing the calibration data (see note
#'   below)
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \var{TRUE} if users use an interactive R
#'   session and \var{FALSE} otherwise.
#' @param control_optim A \var{list} to pass information to the argument control
#'   of the optim call (for advanced users only).
#' @return This function returns a \var{list} of class \var{CALIBFIT} containing
#'   the fixed-effect estimates of the calibration function, the covariance of
#'   the fixed effects, the residual variance of the calibration fit, the fitted
#'   calibration model, the original calibration data set with additional
#'   information added during the fit, and the location of the calibration
#'   points as spatial points.
#' @note See \code{\link{CalibDataAlien}} to know which variables are needed to
#'   perform the calibration fit and their names.
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
#' if(getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## We prepare the data:
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ## We fit the models for Germany:
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' ## We fit the calibration model:
#' CalibAlien <- calibfit(data = CalibDataAlien, isofit = GermanFit)
#' 
#' ## We display minimal information:
#' CalibAlien
#' 
#' ## We display more information:
#' summary(CalibAlien)
#' 
#' ## We plot the calibration function:
#' plot(CalibAlien)
#' 
#' ## Add other calibrations on the same plot
#' CalibDataAlien3 <- CalibDataAlien2 <- CalibDataAlien
#' CalibDataAlien2$sample_value <- rnorm(nrow(CalibDataAlien2),
#'   mean = CalibDataAlien2$sample_value, sd = 10)
#' CalibDataAlien3$sample_value <- rnorm(nrow(CalibDataAlien3),
#'   mean = CalibDataAlien3$sample_value, sd = 10)
#' CalibAlien2 <- calibfit(data = CalibDataAlien2, isofit = GermanFit)
#' CalibAlien3 <- calibfit(data = CalibDataAlien3, isofit = GermanFit)
#' plot(CalibAlien3)
#' points(CalibAlien)
#' points(CalibAlien2, pch = 3, col = "green", CI = list(col = "green"))
#' 
#' 
#' }
#' 
#' @export
calibfit <- function(data,
                     isofit,
                     verbose = interactive(),
                     control_optim = list()
                     ) {

  time <- system.time({
    ## checking inputs
    if (any(class(isofit) %in% "MULTIISOFIT")) {
      stop("object 'isofit' of class MULTIISOFIT; calibration have not yet been implemented for this situation.")
    }
    
    ## Note: all is prepared to use species_rand as an argument (with NULL = automatic selection)
    ## That would allow to fit species as a random effect in the model
    ## However, it is not obvious that it would make sense to do that as it may
    ## remove variance that should be captured during the assignment...
    species_rand <- FALSE
    
    species_info <- any(colnames(data) %in% "species_ID")
    if (!is.null(species_rand)) {
      if (!species_info & species_rand) {
        stop("The random effect for species cannot be fit if data does not contain a column called species_ID")
      }
    }
    
    ## prepare the dataset
    data <- .prepare_data_calib(data)

    ## set species_rand
    if (!species_info) {
      species_rand <- FALSE
    } else {
      nb_species <- length(unique(data$species_ID))
      if (is.null(species_rand) & nb_species > 4) {
        species_rand <- TRUE
      } else {
        species_rand <- FALSE
      }
    }

    ## predict isoscape and associated prediction
    ##   covariance matrix at animal locations
    
    if (verbose) {
      print("predicting the isoscape value in each calibration site...")
    }
  
    mean_calib <- spaMM::predict.HLfit(isofit[["mean_fit"]],
                                       newdata = data,
                                       variances = list(predVar = TRUE, cov = TRUE)
                                      )

    ## store the mean prediction
    data$mean_source_value <- c(mean_calib)
    
    ## warns if extrapolation occurs
    too_small <- sum(min(data$mean_source_value, na.rm = TRUE) < min(isofit$info_fit$data$mean_source_value, na.rm = TRUE))
    too_large <- sum(max(data$mean_source_value, na.rm = TRUE) > max(isofit$info_fit$data$mean_source_value, na.rm = TRUE))
    if (too_small + too_large > 0) {
      warning(paste(too_small + too_large, "of your calibration data are associated to predicted values more extreme than the ones present in the isoscape. This corresponds to extrapolation during the calibration step and should thus to be avoided to obtain reliable assignments. Perhaps revise your isoscape to avoid this problem."))
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
    objective_fn_calib <- function(param, data, predcov, species_rand, return_fit = FALSE, lik_method = "REML") {
      ## This function computes the likelihood of a given calibration function
      data$intercept <- param[1]
      data$slope <- param[2]
      lambda_list <- list(lambda = c(1e-6 + unique(data$slope)^2, NA))
      calib_formula <- "sample_value ~ 0 + offset(intercept+slope*mean_source_value) +
        corrMatrix(1|site_ID) + (1|site_ID)"
      if (species_rand) {
        lambda_list$lambda <- c(lambda_list$lambda, NA)
        calib_formula <- paste(calib_formula, "+ (1|species_ID)")
      }
      calib_fit <- spaMM::HLCor(formula = stats::formula(calib_formula),
                               corrMatrix = predcov,
                               ranPars = lambda_list,
                               data = data,
                               method = lik_method
      )
      if (return_fit) return(calib_fit)
      return(calib_fit$APHLs$p_v)
    }
    
    ## estimation of intercept and slope of the calibration function
    opt_res <- stats::optim(par = c(0, 1),
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
    calib_fit <- objective_fn_calib(param = param_calibfit,
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
  
    fixefCov_calibfit <- solve(-numDeriv::hessian(objective_fn_calib,
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
    print(paste("the calibration procedure based on", nrow(data), "calibration samples have been computed in", time, "sec."))
  }
  
  ## we create the spatial points for calibration points
  calib_points  <- .create_spatial_points(long = data$long,
                                          lat = data$lat,
                                          proj = "+proj=longlat +datum=WGS84"
                                          )

  ## return
  out <- list("param" = param_calibfit,
              "fixefCov" = fixefCov_calibfit,
              "phi" = calib_fit$phi,
              "calib_fit" = calib_fit,
              "data" = data,
              "sp_points" = list(calibs = calib_points)
              )

  class(out) <- c("CALIBFIT", "list")

  return(invisible(out))
}



.prepare_data_calib <- function(data, weighting = NULL) {
  ## This function should not be called by the user.
  ## It prepares data for the calibration procedure.
  
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
  data$site_ID <- factor(data$site_ID)
  data$lat_abs <- abs(data$lat)
  data$lat_2 <- data$lat^2
  data$long_2 <- data$long^2
  data$source_ID <- as.factor(paste("new", data$site_ID, sep = "_"))
  data <- droplevels(data)
  if (!is.null(weighting)) {
    precipitations <- raster::extract(weighting, cbind(data$long, data$lat))
    precipitations[is.na(precipitations)] <- 0.0001 ## remove NA to prevent crashes --> dangerous?
    data <- cbind(data, precipitations)
  }
  return(data)
}

#' @export
#' @method print CALIBFIT
print.CALIBFIT <- function(x, ...) {
  cat("\n")
  cat("Fixed effect estimates of the calibration fit", "\n")
  if (length(x$calib_fit$lambda) == 2) {
    cat("sample_value = intercept + slope * mean_source_value + corrMatrix(1|site_ID) + slope^2 * (1|site_ID) + Error", "\n")
  } else {
    cat("sample_value = intercept + slope * mean_source_value + corrMatrix(1|site_ID) + slope^2 * (1|site_ID) + (1|species_ID) + Error", "\n")
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
  cat("#########################################################", "\n")
  cat("### spaMM summary of the fit of the calibration model ###", "\n")
  cat("#########################################################", "\n")
  cat("\n")
  print(spaMM::summary.HLfit(object$calib_fit)$summ)
  cat("\n")
  cat(paste("[model fitted with spaMM version ", object$calib_fit$spaMM.version, "]", sep = ""), "\n")
  cat("\n")
  return(invisible(NULL))
}

