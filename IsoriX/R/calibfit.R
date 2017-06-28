#' @rdname IsoriX-defunct
#' @export
Calibfit <- function(...) {
  .Defunct("calibfit")
}

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
#' provided to \code{calibfit} using the argument \code{isofit}.
#' 
#' The LMM used to fit the calibration function has a simple fixed-effect
#' structure: an intercept and a slope. The random effect is more complex: it is
#' normally distributed with mean zero, a certain variance between locations
#' proportional to the squared fixed slope, and a covariance structure defined
#' by the prediction covariance matrix of the isoscape model between the
#' calibration locations. All models used in \pkg{IsoriX} will be soon detailed
#' in an additional document.
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
#' @aliases calibfit print.calibfit summary.calibfit
#' @param calib.data A \var{dataframe} containing the calibration data (see
#' note below)
#' @param isofit The fitted isoscape model created by \code{\link{isofit}}
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session and \var{FALSE} otherwise.
#' @param control.optim A \var{list} to pass information to the argument
#' control of the optim call (for advanced users only).
#' @return This function returns a \var{list} of class \var{calibfit}
#' containing the fixed-effect estimates of the calibration function, the
#' covariance of the fixed effects, the fitted calibration model, the original
#' calibration data set with additional information added during the fit, and
#' the location of the calibration points as spatial points.
#' @note See \code{\link{CalibDataAlien}} to know which variables are needed to
#' perform the calibration fit and their names.
#' @seealso \code{\link{IsoriX}} for the complete workflow
#' @keywords models regression
#' @examples
#' ## The following example takes some time and will therefore not
#' ## be run unless you type: example(calibfit, run.dontrun = TRUE)
#' 
#' \dontrun{
#' ## We fit the models for Germany:
#' GNIPDataDEagg <- queryGNIP(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(iso.data = GNIPDataDEagg)
#' 
#' ## We fit the calibration model:
#' calib <- calibfit(calib.data = CalibDataAlien, isofit = GermanFit)
#' 
#' ## We display minimal information:
#' calib
#' 
#' ## We display more information:
#' summary(calib)
#' 
#' ## We plot the calibration function:
#' plot(calib)
#' }
#' 
#' @export
calibfit <- function(calib.data,
                     isofit,
                     verbose = interactive(),
                     control.optim = list()
                     ) {

  time <- system.time({
    
    ## prepare the dataset
    calib.data <- .PrepareDataCalib(calib.data)
  
    ## predict isoscape and associated prediction
    ##   covariance matrix at animal locations
  
    if (verbose) {
      print("predicting the isoscape value in each calibration site...")
    }
  
    calib.mean <- spaMM::predict.HLfit(isofit[["mean.fit"]],
                                       newdata = calib.data,
                                       variances = list(predVar = TRUE, cov = TRUE)
                                      )
    
    ## store the mean prediction
    calib.data$mean.iso <- c(calib.mean)
  
    ## extract the prediction covariance matrix
    predcov.isofit.full <- attr(calib.mean, "predVar")
  
    ## extract the prediction variances
    calib.data$mean.predVar.iso <- diag(predcov.isofit.full)
    
    ## reshape the prediction covariance matrix to number of unique sites
    firstoccurences <- match(levels(calib.data$siteID), calib.data$siteID)
    predcov.isofit <- predcov.isofit.full[firstoccurences, firstoccurences]
    rownames(predcov.isofit) <- levels(calib.data$siteID)
    colnames(predcov.isofit) <- levels(calib.data$siteID)
  
    ### fitting the calibration function
    if (verbose) {
      print("fitting the calibration function...")
    }
  
    ## estimation of intercept and slope of the calibration function
    opt.res <- stats::optim(par = c(0, 1),
                     fn = .ObjectiveFnCalib,
                     control = c(list(fnscale = -1), control.optim),
                     data = calib.data,
                     predcov = predcov.isofit,
                     lik.method = "REML"
                     )
  
    param.calibfit <- opt.res$par
    names(param.calibfit) <- c("intercept", "slope")
  
    ## fit of the calibration function
    calib.fit <- .ObjectiveFnCalib(param = param.calibfit,
                                   data = calib.data,
                                   predcov = predcov.isofit, 
                                   lik.method = "REML",
                                   return.fit = TRUE
                                   )
  
    ## computing the covariance matrix of fixed effects
    if (verbose) {
      print("computing the covariance matrix of fixed effects...")
    }
  
    fixefCov.calibfit <- solve(-numDeriv::hessian(.ObjectiveFnCalib,
                                                  param.calibfit,
                                                  data = calib.data,
                                                  predcov = predcov.isofit, 
                                                  lik.method = "ML"
                                                  )
                              )
  
    rownames(fixefCov.calibfit) <- names(param.calibfit)
    colnames(fixefCov.calibfit) <- names(param.calibfit)
    
  }) ## end of system.time
  
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("the calibration procedure based on", nrow(calib.data), "calibration samples have been computed in", time, "sec."))
  }
  
  ## we create the spatial points for calibration points
  calib.points  <- .CreateSpatialPoints(long = calib.data$long,
                                        lat = calib.data$lat,
                                        proj = "+proj=longlat +datum=WGS84"
                                        )

  ## return
  out <- list("param" = param.calibfit,
              "fixefCov" = fixefCov.calibfit,
              "calib.fit" = calib.fit,
              "calib.data" = calib.data,
              "sp.points" = list(calibs = calib.points)
              )

  class(out) <- c("calibfit", "list")

  return(invisible(out))
}


.PrepareDataCalib <- function(data) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It prepares data for the calibration procedure.
  if (!all(c("lat", "long") %in% colnames(data))) {
  stop("the dataset does not seem to contain the required variable(s) lat and/or long")
  }
  if (is.null(data$tissue.value)) {
    stop("the dataset does not seem to contain the required variable tissue.value")
  }
  if (is.null(data$siteID)) {
    stop("the dataset does not seem to contain the required variable siteID")
  }
  data$siteID <- factor(data$siteID)
  data$lat.abs <- abs(data$lat)
  data$lat.2 <- data$lat^2
  data$long.2 <- data$long^2
  data$stationID <- as.factor(paste("new", data$siteID, sep = "_"))
  data <- droplevels(data)
  return(data)
}


.ObjectiveFnCalib <- function(param, data, predcov, return.fit = FALSE, lik.method = "REML") {
  ## This function should not be called by the user but is itself called by other functions.
  ## It computes the likelihood of a given calibration function
  data$intercept <- param[1]
  data$slope <- param[2]
  calib.fit <- spaMM::HLCor(formula = tissue.value ~ 0 + offset(intercept+slope*mean.iso) +
                            corrMatrix(1|siteID) + (1|siteID),
                            corrMatrix = predcov,
                            ranPars = list(lambda = c(1e-6 + unique(data$slope)^2, NA)),
                            data = data,
                            method = lik.method
                            )
  if (return.fit) return(calib.fit)
  return(calib.fit$APHLs$p_v)
}

#' @export
#' @method print calibfit
print.calibfit <- function(x, ...) {
  cat("\n")
  cat("Fixed effect estimates of the calibration fit", "\n")
  cat("tissue.value = intercept + slope * mean.iso +   corrMatrix(1|siteID) + slope^2 * (1|siteID) + Error", "\n")
  cat("\n")
  cat(paste("           intercept (+/- SE) =", .NiceRound(x$param["intercept"], 2),
            "+/-",  .NiceRound(sqrt(x$fixefCov["intercept", "intercept"]), 2)), "\n")
  cat(paste("           slope     (+/- SE) =  ", .NiceRound(x$param["slope"], 2),
            "+/-",  .NiceRound(sqrt(x$fixefCov["slope", "slope"]), 2)), "\n")
  cat("\n")
  cat("[for more information, use summary()]", "\n")
  cat("\n")
  return(invisible(NULL))
}

#' @export
#' @method summary calibfit
summary.calibfit <- function(object, ...) {
  cat("\n")
  cat("Fixed effect estimates of the calibration fit", "\n")
  print(.NiceRound(object$param, 3), quote = FALSE)
  cat("\n")
  cat("Covariance matrix of fixed effect estimates:", "\n")
  print(.NiceRound(object$fixefCov, 3), quote = FALSE)
  cat("\n")
  cat("#########################################################", "\n")
  cat("### spaMM summary of the fit of the calibration model ###", "\n")
  cat("#########################################################", "\n")
  cat("\n")
  print(spaMM::summary.HLfit(object$calib.fit))
  cat("\n")
  cat(paste("[model fitted with spaMM version ", object$calib.fit$spaMM.version, "]", sep = ""), "\n")
  cat("\n")
  return(invisible(NULL))
}

