#' @rdname IsoriX-defunct
#' @export
Isofit <- function(...) {
  .Defunct("isofit")
}


#' Fit the isoscape model
#' 
#' This function fits the isocape as a mixed model. The fitting procedures are
#' done by the package \pkg{\link[spaMM]{spaMM}} which we use to jointly fit
#' the mean isotopic values and their associated residual dispersion variance
#' in a spatially explicit manner.
#' 
#' The detailed statistical definition of the isoscape model will be soon
#' available in another document. Briefly, the fitting procedure of the
#' isoscape model is divided into two fits: \code{mean.fit} and
#' \code{disp.fit}. \code{mean.fit} corresponds to the fit of the "mean model",
#' which we will use to predict the mean isotopic values at any location in
#' other functions of the package. \code{disp.fit} corresponds to the fit of
#' the "residual dispersion model", which we will use to predict the residual
#' dispersion variance associated to the mean predictions. \code{mean.fit} is a
#' linear mixed-effects model (LMM) with fixed effects, an optional spatial
#' random effect with a Matern correlation structure and an optional
#' uncorrelated random effect accounting for variation between weather station
#' unrelated to their location. \code{disp.fit} is a Gamma Generalized LMM
#' (Gamma GLMM) that also has fixed effects, an optional spatial random effect
#' with a Matern correlation structure and an optional uncorrelated random
#' effect. For the GLMM the residual variance is fixed to its theoretical
#' expectation.
#' 
#' The \var{dataframe} \code{iso.data} must contain a single row per source
#' location with the following columns: \code{isoscape.value} (the isotopic
#' value), \code{var.isoscape.value} (the unbiased variance estimate of the
#' isotopic value at the location), \code{n.isoscape.value} (the number of
#' measurements performed at the location, could be 1) and \code{stationID} (a
#' factor defining the identity of the sources at a given location).
#' 
#' The arguments \code{mean.model.fix} and \code{disp.model.fix} allow the user
#' to choose among different fixed-effect structures for each model. These
#' arguments are lists of booleans (\code{TRUE} or \code{FALSE}), which define
#' which of the following fixed effects must be considered: the elevation
#' (\code{elev}), the absolute value of the latitude (\code{lat.abs}), the
#' squared latitude (\code{lat.2}), the longitude (\code{long}) and the squared
#' longitude (\code{long.2}). An intercept is always considered in both models.
#' By default, only intercept are being fitted.
#' 
#' In the models, the mean (for the mean model) or the log residual variance
#' (for the residual dispersion model) follow a Gaussian distribution around a
#' constant value. The arguments \code{mean.model.rand} and
#' \code{disp.model.rand} allow to choose among different random effects for
#' each model influencing the realizations of these Gaussian random processes.
#' For each model one can choose not to include random effects or to include an
#' uncorrelated random effect, a spatial random effect, or both (default).
#' Setting \code{"uncorr" = TRUE} implies that the different realizations are
#' identical for a given weather station (e.g. some micro-climate or some
#' measurement errors trigger a shift in all measurements (mean model) or a
#' shift in the variance between measurements (residual dispersion model)
#' performed on a given weather station by the same amount). Setting
#' \code{"spatial" = TRUE} (default) implies that the random realizations of the
#' Gaussian process follow a Matern correlation structure. Put simply, this
#' implies that the closer two locations are, the more similar the means (or
#' the log residual variance) in isotopic values are (e.g. because they are
#' likely to be traversed by the same air masses).
#' 
#' The arguments \code{uncorr.terms} allow the choice between two alternative
#' parameterizations for the uncorrelated random effect in the fits:
#' \code{"lambda"} or \code{"nugget"} for each model. When using
#' \code{"lambda"}, the variance of the uncorrelated random terms is
#' classically modeled by a variance. When a spatial random effect is
#' considered, one can alternatively choose \code{"nugget"}, which modifies the
#' Matern correlation value when distance between location tends to zero. If no
#' random effect is considered, one should stick to the default setting and it
#' will be ignored by the function. The choice of the parametrization is a
#' matter of personal preferences and it does not change the underlying models,
#' so the estimations for all the other parameters of the models will not be
#' impacted by whether one chooses \code{lambda} or \code{nugget}. Depending on
#' the data one parametrization may lead to faster fit than the other.
#' 
#' The argument \code{spaMM.method} is also a list of two \var{strings} where
#' the first element defines the spaMM functions used for fitting the mean
#' model and the second element defines the spaMM method used for fitting the
#' residual dispersion model. The possible options are "HLfit", "corrHLfit" and
#' "fitme". Note that "HLfit" shall only be used in the absence of a Matern
#' correlation structure and "corrHLfit" shall only be used in the presence of
#' it. In contrast, "fitme" should work in all situations. Which method is best
#' remains to be determined and it is good practice to try different methods
#' (if applicable) to check for the robustness of the results. If all is well
#' one should obtain very similar results with the different methods. If this
#' is not the case, carefully check the model output to see if one model fit
#' did not get stuck at a local minimum during optimization (which would
#' translate in a lower likelihood).
#' 
#' The argument \code{dist.method} allows modifying how the distance between
#' locations is computed to estimate the spatial correlation structure. By
#' default, we consider the so-called "Earth" distances which are technically
#' called orthodromic distances. They account for earth curvature. The
#' alternative "Euclidean" distances do not. For studies performed on a small
#' geographic scale, both distance methods should lead to similar results.
#' 
#' The arguments \code{control.mean} and \code{control.dist} are lists that are
#' transmitted to the \pkg{\link[spaMM]{spaMM}} fitting functions (defined by
#' \code{spaMM.method}). These lists can be used to finely control the fitting
#' procedure, so advanced knowledge of the package \pkg{\link[spaMM]{spaMM}} is
#' required before messing around with these inputs.
#' 
#' We highly recommend users to examine the output produced by \code{isofit}.
#' Sometimes, poor fit may occur and such models should therefore not be used
#' for building isoscapes or performing assignments.
#' 
#' @aliases isofit print.isofit summary.isofit
#' @param iso.data The \var{dataframe} containing the data used for fitting the
#' isoscape model
#' @param mean.model.fix A \var{list} of \var{logical} indicating which fixed
#' effects to consider in mean.fit
#' @param disp.model.fix A \var{list} of \var{logical} indicating which fixed
#' effects to consider in disp.fit
#' @param mean.model.rand A \var{list} of \var{logical} indicating which random
#' effects to consider in mean.fit
#' @param disp.model.rand A \var{list} of \var{logical} indicating which random
#' effects to consider in disp.fit
#' @param uncorr.terms A \var{list} of two strings defining the parametrization
#' used to model the uncorrelated random effects for mean.fit and disp.fit
#' @param spaMM.method A \var{list} of two strings defining the spaMM functions
#' used for mean.fit and disp.fit
#' @param dist.method A \var{string} indicating the distance method
#' @param control.mean A \var{list} of additional arguments to be passed to the
#' call of mean.fit
#' @param control.disp A \var{list} of additional arguments to be passed to the
#' call of disp.fit
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \code{TRUE} if users use an interactive R
#' session and \code{FALSE} otherwise.
#' @return This function returns a \var{list} of class \code{isofit} containing
#' two inter-related fits: \code{mean.fit} and \code{disp.fit}. The returned
#' \var{list} also contains the object \code{info.fit} that contains all the
#' call arguments.
#' @note There is no reason to restrict \code{mean.fit} and \code{disp.fit} to
#' using the same parametrization for fixed and random effects.
#' 
#' Never use a mean.fit object to draw predictions without considering a
#' disp.fit object: mean.fit is not fitted independently from disp.fit.
#' 
#' For all methods, fixed effects are being estimated by Maximum Likelihood
#' (ML) and dispersion parameters (i.e. random effects and Matern correlation
#' parameters) are estimated by Restricted Maximum Likelihood (REML). Using
#' REML provides more accurate prediction intervals but impedes the accuracy of
#' Likelihood Ratio Tests (LRT). Our choice for REML was motivated by the fact
#' that our package is more likely to be used for drawing inferences than null
#' hypothesis testing. Users interested in model comparisons may rely on the
#' AIC values that can be extracted from fitted models using the function
#' \code{\link[spaMM:extractors]{AIC}} from the \pkg{\link[spaMM]{spaMM}}.
#' 
#' Variable names for \code{iso.data} must be respected to ensure a correct
#' utilization of this package. Alteration to the fixed effect structure is not
#' implemented so far (beyond the different options proposed) to avoid misuse
#' of the package. Users that would require more flexibility should consider
#' using spaMM directly (at their own risks). We will soon provide a document
#' explaining how to do so.
#' @seealso \pkg{\link[spaMM]{spaMM}} for an overview of the \pkg{spaMM}
#' package
#' 
#' \code{\link[spaMM]{fitme}} and \code{\link[spaMM]{corrHLfit}} for
#' information about the two possible fitting procedures that can be used here
#' 
#' \code{\link[spaMM]{Matern.corr}} for information about the Matern
#' correlation structure
#' 
#' \code{\link{IsoriX}} for the complete work-flow of our package
#' @references Rousset, F., Ferdy, J. B. (2014). Testing environmental and
#' genetic effects in the presence of spatial autocorrelation. Ecography,
#' 37(8):781-790.
#' 
#' Bowen, G. J., Wassenaar, L. I., Hobson, K. A. (2005). Global application of
#' stable hydrogen and oxygen isotopes to wildlife forensics. Oecologia,
#' 143(3):337-348.
#' @source \url{http://kimura.univ-montp2.fr/~rousset/spaMM.htm}
#' @keywords models regression
#' @examples
#' 
#' 
#' ## Fitting the models for Germany:
#' 
#' GNIPDataDEagg <- queryGNIP(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(iso.data = GNIPDataDEagg)
#' 
#' GermanFit
#' 
#' ## Diagnostics for the fits:
#' plot(GermanFit)
#' 
#' 
#' @export
isofit <- function(iso.data,
                   mean.model.fix = list(elev = TRUE, lat.abs = TRUE, lat.2 = FALSE, long = FALSE, long.2 = FALSE),
                   disp.model.fix = list(elev = FALSE, lat.abs = FALSE, lat.2 = FALSE, long = FALSE, long.2 = FALSE),
                   mean.model.rand = list(uncorr = TRUE, spatial = TRUE),
                   disp.model.rand = list(uncorr = TRUE, spatial = TRUE),
                   uncorr.terms = list(mean.model = "lambda", disp.model = "lambda"), ## or: "nugget"
                   spaMM.method = list(mean.model = "fitme", disp.model = "fitme"), ## or: "corrHLfit", "HLfit"
                   dist.method = "Earth", ## or: "Euclidean"
                   control.mean = list(),
                   control.disp = list(),
                   verbose = interactive()
                   ) {

  ## Complete the arguments
  .CompleteArgs(isofit)

  ## Save the call information
  info.fit <- mget(names(formals()))
  info.fit$IsoriX_version <- utils::packageDescription("IsoriX")$Version
  info.fit$verbose <- verbose

  ## Check that arguments are correct and to some extent test that they make sense
  if (!dist.method %in% c("Euclidean", "Earth")) {
    warnings("the argument you chose for dist.method may not be safe to use, please use 'Euclidean' or 'Earth' unless you really know how this package works")
  }
  if (!all(unlist(spaMM.method) %in% c("fitme", "corrHLfit", "HLfit"))) {
    stop("the argument(s) you chose for spaMM.method are unknown")
  }

  ## Partially check that the different arguments are compatible between each others
  if (sum(unlist(mean.model.rand)) == 0 & (spaMM.method$mean.model == "corrHLfit" | spaMM.method$disp.model == "corrHLfit")) {
    stop("your call does not make sense: the spaMM.method 'corrHLfit' should only be used when random effects are present")
  }
  if (!mean.model.rand$spatial & !disp.model.rand$spatial &
      all(unlist(uncorr.terms) != c("lambda", "lambda"))
      ) {
    stop("in the absence of spatial random effects, only lambda can be used as uncorr.terms")
  }


  ## Prepare the dataset
  iso.data <- .PrepareDataIso(iso.data)

  ## Define the formulas for each model   
  mean.formula <- .PrepareFormula("isoscape.value ~ 1",
                                  fix = mean.model.fix, rand = mean.model.rand,
                                  rand.p = uncorr.terms$mean.model
                                  )
  disp.formula <- .PrepareFormula("var.isoscape.value ~ 1",
                                  fix = disp.model.fix, rand = disp.model.rand,
                                  rand.p = uncorr.terms$disp.model
                                  )

  ## Define weights
  iso.data$weights.mean <- as.numeric(iso.data$n.isoscape.value)
  iso.data$weights.disp <- as.numeric(iso.data$n.isoscape.value) - 1
  if (any(iso.data$weights.disp < 1)) {
    warning("some prior weights seem to be null")
  }
    
  ## Define the baseline argument lists for the models irrespective of the spaMM.method       
  args.disp.fit <- list(formula = stats::formula(disp.formula),
                        family = stats::Gamma(log),
                        prior.weights = iso.data$weights.disp,
                        data = iso.data
                        )

  args.mean.fit <- list(formula = stats::formula(mean.formula),
                        prior.weights = iso.data$weights.mean,
                        resid.model = list(formula = ~ 0 + offset(pred.disp), family = stats::Gamma(identity)),
                        data = iso.data
                        )

  ## Inclusion of additional arguments for corrHLfit, if necessary
  if (spaMM.method[1] == "corrHLfit") {
    args.mean.fit[["control.corrHLfit"]] <- list(maxcorners = 0)
    if (mean.model.rand$spatial) {
      args.mean.fit[["control.dist"]] <- list(dist.method = dist.method)
      if (uncorr.terms$mean.model == "nugget") args.mean.fit[["init.corrHLfit"]] <- list(Nugget = 0.01)
    }
  }
  if (spaMM.method[2] == "corrHLfit") {
    if (disp.model.rand$spatial) {
      args.disp.fit[["control.dist"]] <- list(dist.method = dist.method)
      if (uncorr.terms$disp.model == "nugget") args.disp.fit[["init.corrHLfit"]] <- list(Nugget = 0.01)
    }
    args.disp.fit[["control.corrHLfit"]] <- list(maxcorners = 0)
    args.disp.fit[["ranFix"]] <- list(phi = 2)  
  }

  ## Inclusion of additional arguments for fitme, if necessary
  if (spaMM.method[1] == "fitme") {
    args.mean.fit[["method"]] <- "REML"
    if (mean.model.rand$spatial) {
      args.mean.fit[["control.dist"]] <- list(dist.method = dist.method)
      if (uncorr.terms$mean.model == "nugget") args.mean.fit[["init"]] <- list(Nugget = 0.01)
    }
  }
  if (spaMM.method[2] == "fitme") {
    args.disp.fit[["method"]] <- "REML"
    args.disp.fit[["fixed"]] <- list(phi = 2)
    if (disp.model.rand$spatial) {
      args.disp.fit[["control.dist"]] <- list(dist.method = dist.method)
      if (uncorr.terms$disp.model == "nugget") args.disp.fit[["init"]] <- list(Nugget = 0.01)
    }
  }

  ## Interactive display
  if (verbose) {
    nug.string <- ifelse(uncorr.terms$disp.model == "nugget", "with a Nugget", "")
    print(paste("Fitting the following residual dispersion model using spaMM", nug.string, ":"), quote = FALSE)
    print(disp.formula)
    if (sum(unlist(disp.model.rand)) > 0) print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Fit disp.fit
  time.disp <- system.time(disp.fit <- do.call(eval(parse(text = paste0("spaMM::", spaMM.method$disp.model))),
                                               c(args.disp.fit, control.disp)
                                               )
                          )

  ## Predict the values for the residual variance
  args.mean.fit$data$pred.disp <- spaMM::predict.HLfit(disp.fit, newdata = iso.data)[, 1]

  ## Interactive display
  if (verbose) {
    nug.string <- ifelse(uncorr.terms$mean.model == "nugget", "with a Nugget", "")
    print(paste("Fitting the following mean model using spaMM", nug.string, ":"), quote = FALSE)
    print(mean.formula)
    if (sum(unlist(mean.model.rand)) > 0) print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Fit mean.fit
  time.mean <- system.time(mean.fit <- do.call(eval(parse(text = paste0("spaMM::", spaMM.method$mean.model))),
                                               c(args.mean.fit, control.mean)
                                               )
                           )

  ## Interactive display of fit time duration
  total.time <- round(as.numeric((time.mean + time.disp)[3]))
  if (verbose) {
    print(paste("Done!"), quote = FALSE)
    print(paste("Models were fitted in", total.time, "sec."))
  }

  ## Store the time
  info.fit$time.fit <- total.time

  ## Create the return object
  out <- list("mean.fit" = mean.fit, "disp.fit" = disp.fit, "info.fit" = info.fit)
  
  class(out) <- c("isofit", "list")

  return(invisible(out))
}


#' Fit isoscape models per strata (typically time interval such as months)
#' 
#' This function fits several bunch of isocapes (e.g. one per strata), which we
#' call sub-isoscape. It can thus be used to predict annual averages
#' precipitation weighted isoscapes.
#' 
#' This function is a wrapper around the function \code{\link{isofit}}.
#' 
#' @return This function returns a \var{list} of class \code{multiisofit} containing
#' all pairs of inter-related fits: \code{multi.fits}. The returned
#' \var{list} also contains the object \code{info.fit} that contains all the
#' call arguments.
#' 
#' @inheritParams isofit
#' @param split.by A \var{string} indicating the name of the column of
#'   \code{iso.data} used to split the dataset. The function
#'   \code{\link{isofit}} will then be called on each of these sub-datasets. The
#'   default split the dataset per months (\code{split.by = "month"}).
#'   
#' @seealso \code{\link{isofit}} for information about the fitting procedure of 
#'   each sub-isoscape.
#' 
#' @examples
#' 
#' ## We prepare the GNIP monthly data between January and June for Germany
#' 
#' GNIPDataDEmonthly <- queryGNIP(data = GNIPDataDE,
#'                                month = 1:6,
#'                                split.by = "month")
#' 
#' dim(GNIPDataDEmonthly)
#' 
#' ## We fit the isoscapes
#' 
#' isoscapemodels <- isomultifit(iso.data = GNIPDataDEmonthly)
#' 
#' isoscapemodels
#' @export
isomultifit <- function(iso.data,
                        split.by = "month",
                        mean.model.fix = list(elev = TRUE, lat.abs = TRUE, lat.2 = FALSE, long = FALSE, long.2 = FALSE),
                        disp.model.fix = list(elev = FALSE, lat.abs = FALSE, lat.2 = FALSE, long = FALSE, long.2 = FALSE),
                        mean.model.rand = list(uncorr = TRUE, spatial = TRUE),
                        disp.model.rand = list(uncorr = TRUE, spatial = TRUE),
                        uncorr.terms = list(mean.model = "lambda", disp.model = "lambda"), ## or: "nugget"
                        spaMM.method = list(mean.model = "fitme", disp.model = "fitme"), ## or: "corrHLfit", "HLfit"
                        dist.method = "Earth", ## or: "Euclidean"
                        control.mean = list(),
                        control.disp = list(),
                        verbose = interactive()
) {
  
  ## Complete the arguments
  .CompleteArgs(isomultifit)
  
  ## Save the call information
  info.multifit <- info.fit <- mget(names(formals()))
  info.multifit$IsoriX_version <- utils::packageDescription("IsoriX")$Version
  info.multifit$verbose <- verbose
  
  if (is.null(iso.data[, split.by])) {
      stop(paste("you used 'split.by =", split.by, "' but no column called ',", split.by, "' is found in 'iso.data'..."))
  }
  
  ## Prepare arguments for call(s) to isofit
  info.fit$split.by <- info.fit$weighting <- NULL  ## removes arguments unknown to isofit
  
  ## Trivial case if no splitting is done
  if (is.null(split.by)) {
    return(do.call(isofit, info.fit))
  }

  ## Interactive display
  if (verbose) {
    print(paste("Fitting the all", length(unique(iso.data[, split.by])), "pairs of models using spaMM:"), quote = FALSE)
    print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Run all fits
  info.fit$verbose <- FALSE ## no display for each fit  
  total.time <- system.time({
    multi.fits <- lapply(unique(iso.data[, split.by]), function(s) {
      info.fit$iso.data <- iso.data[iso.data[, split.by] == s, ]
      fit <- do.call(isofit, info.fit)
      if (verbose) {
        print(paste("fit of the pair of models for", split.by, s, "done"), quote = FALSE)
      }
      return(fit)
      })
  })
  names(multi.fits) <- paste(split.by, unique(iso.data[, split.by]), sep = "_")
  
  ## Interactive display
  if (verbose) {
    print(paste("Done!"), quote = FALSE)
    print(paste("All models have been fitted in", round(as.numeric((total.time)[3])), "sec."), quote = FALSE)
  }
  
  ## Store the time
  info.multifit$time.fit <- total.time
  
  ## Create the return object
  out <- list("multi.fits" = multi.fits, "info.fit" = info.multifit)
  
  class(out) <- c("multiisofit", "isofit", "list")
  
  return(invisible(out))
}


.PrepareDataIso <- function(data) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It prepares data for the prediction procedures.
  if (!all(c("lat", "long") %in% colnames(data))) {
    stop("the dataset does not seem to contain the required variable(s) lat and/or long")
  }
  if (is.null(data$var.isoscape.value)) {
    stop("the dataset does not seem to contain the required variable var.isoscape.value")
  }
  if (is.null(data$n.isoscape.value)) {
    stop("the dataset does not seem to contain the required variable n.isoscape.value")
  }
  if (any(!is.na(data$var.isoscape.value) & data$var.isoscape.value <= 0)) {
    stop("the dataset seem to contain null or negative value for var.isoscape.value")
  }
  if (!is.null(data$stationID)) {
    data$stationID <- factor(data$stationID)
  } #else {
    #data$stationID <- factor(1:nrow(data))
  #}
    
  data$lat.abs <- abs(data$lat)
  data$lat.2 <- data$lat^2
  data$long.2 <- data$long^2
  return(data)
}


.PrepareFormula <- function(base.formula, fix, rand, rand.p){
  ## This function should not be called by the user but is itself called by other functions.
  ## It prepares formulas for the fitting procedures.
  if (fix$elev) {
    base.formula <- paste(base.formula, "+ elev")
  }
  if (fix$lat.abs) {
    base.formula <- paste(base.formula, "+ lat.abs")
  }
  if (fix$lat.2) {
    base.formula <- paste(base.formula, "+ lat.2")
  }
  if (fix$long) {
    base.formula <- paste(base.formula, "+ long")
  }
  if (fix$long.2) {
    base.formula <- paste(base.formula, "+ long.2")
  }
  if (rand$uncorr & rand.p == "lambda") {
    base.formula <- paste(base.formula, "+ (1|stationID)")
  }
  if (rand$spatial) {
    base.formula <- paste(base.formula, "+ Matern(1|long + lat)")
  }

  return(base.formula)
}

#' @export
#' @method print isofit
print.isofit <- function(x, ...) { ## we should recode this to make a table more succint than that from summary!
  print(summary(x))
  return(invisible(NULL))
}

#' @export
#' @method summary isofit
summary.isofit <- function(object, ...) {
  if (!any(class(object) %in% "multiisofit")) {
    cat("\n")
    cat("### spaMM summary of the fit of the mean model ###", "\n")
    cat("\n")
    print(spaMM::summary.HLfit(object$mean.fit))
    cat("\n")
    cat("\n")
    cat("### spaMM summary of the fit of the residual dispersion model ###", "\n")
    cat("\n")
    print(spaMM::summary.HLfit(object$disp.fit))
    cat("\n")
    cat(paste("[models fitted with spaMM version ", object$mean.fit$spaMM.version, "]", sep = ""), "\n")
    cat("\n")
  } else {
    for (fit in 1:length(object$multi.fits)) {
      cat("\n")
      cat(paste("##### Pair of models", names(object$multi.fits)[fit]), "#####")
      cat("\n")
      summary(object$multi.fits[[fit]])
      }
  }
  return(invisible(NULL))
}
