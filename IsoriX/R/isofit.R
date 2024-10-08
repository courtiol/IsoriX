#' Fit the isoscape models
#'
#' This function fits the aggregated source data using mixed models. The fitting
#' procedures are done by the package [`spaMM::spaMM`] which we use to jointly fit
#' the mean isotopic values and their associated residual dispersion variance in
#' a spatially explicit manner.
#'
#' The detailed statistical definition of the isoscape model is described in
#' Courtiol & Rousset 2017 and summarized in Courtiol et al. 2019.
#'
#' Briefly, the fitting procedure of the isoscape model is divided into two
#' fits: `mean_fit` and `disp_fit`. `mean_fit` corresponds to the fit of the
#' "mean model", which we will use to predict the mean isotopic values at any
#' location in other functions of the package. `disp_fit` corresponds to the fit
#' of the "residual dispersion model", which we will use to predict the residual
#' dispersion variance associated to the mean predictions. `mean_fit` is a
#' linear mixed-effects model (LMM) with fixed effects, an optional spatial
#' random effect with a Matérn correlation structure and an optional
#' uncorrelated random effect accounting for variation between sources unrelated
#' to their location. `disp_fit` is a Gamma Generalized LMM (Gamma GLMM) that
#' also has fixed effects, an optional spatial random effect with a Matérn
#' correlation structure and an optional uncorrelated random effect. For the
#' GLMM the residual variance is fixed to its theoretical expectation.
#'
#' The *dataframe* `data` must contain a single row per source location with the
#' following columns: `mean_source_value` (the mean isotopic value),
#' `var_source_value` (the unbiased variance estimate of the isotopic value at
#' the location), `n_source_value` (the number of measurements performed at the
#' location, could be 1) and `source_ID` (a factor defining the identity of the
#' sources at a given location).
#'
#' The arguments `mean_model_fix` and `disp_model_fix` allow the user to choose
#' among different fixed-effect structures for each model. These arguments are
#' lists of booleans (`TRUE` or `FALSE`), which define which of the following
#' fixed effects must be considered: the elevation (`elev`), the absolute value
#' of the latitude (`lat_abs`), the squared latitude (`lat_2`), the longitude
#' (`long`) and the squared longitude (`long_2`). An intercept is always
#' considered in both models.
#'
#' In the models, the mean (for the mean model) or the log residual variance
#' (for the residual dispersion model) follow a Gaussian distribution around a
#' constant value. The arguments `mean_model_rand` and `disp_model_rand` allow
#' to choose among different random effects for each model influencing the
#' realizations of these Gaussian random processes. For each model one can
#' choose not to include random effects or to include an uncorrelated random
#' effect, a spatial random effect, or both (default). Setting `"uncorr" = TRUE`
#' implies that the realizations of the random effect differ between sources for
#' reasons that have nothing to do with the relative geographic location (e.g.
#' some micro-climate or some measurement errors trigger a shift in all
#' measurements (mean model) or a shift in the variance between measurements
#' (residual dispersion model) performed at a given source by the same amount).
#' Setting `"spatial" = TRUE` (default) implies that the random realizations of
#' the Gaussian process follow a Matérn correlation structure. Put simply, this
#' implies that the closer two locations are, the more similar the means (or the
#' log residual variance) in isotopic values are (e.g. because they are likely
#' to be traversed by the same air masses).
#'
#' The arguments `uncorr_terms` allow the choice between two alternative
#' parametrizations for the uncorrelated random effect in the fits:
#' `"lambda"` or `"nugget"` for each model. When using
#' `"lambda"`, the variance of the uncorrelated random terms is classically
#' modelled by a variance. When a spatial random effect is considered, one can
#' alternatively choose `"nugget"`, which modifies the Matérn correlation
#' value when distance between location tends to zero. If no random effect is
#' considered, one should stick to the default setting and it will be ignored by
#' the function. The choice of the parametrization is a matter of personal
#' preferences and it does not change the underlying models, so the estimations
#' for all the other parameters of the models should not be impacted by whether
#' one chooses `"lambda"` or `"nugget"`. However, only uncertainty in
#' the estimation of `"lambda"` can be accounted for while computing
#' prediction variances, which is why we chose this alternative as the default.
#' Depending on the data one parametrization may lead to faster fit than the
#' other.
#'
#' The argument `spaMM_method` is also a list of two *strings* where the first
#' element defines the spaMM functions used for fitting the mean model and the
#' second element defines the spaMM method used for fitting the residual
#' dispersion model. The possible options are `"HLfit"`, `"corrHLfit"` and
#' `"fitme"`. Note that `"HLfit"` shall only be used in the absence of a Matérn
#' correlation structure and `"corrHLfit"` shall only be used in the presence of
#' it. In contrast, `"fitme"` should work in all situations. Which method is
#' best remains to be determined and it is good practice to try different
#' methods (if applicable) to check for the robustness of the results. If all is
#' well one should obtain very similar results with the different methods. If
#' this is not the case, carefully check the model output to see if one model
#' fit did not get stuck at a local minimum during optimization (which would
#' translate in a lower likelihood, or weird isoscapes looking flat with high
#' peaks at very localised locations).
#'
#' The argument `dist_method` allows modifying how the distance between
#' locations is computed to estimate the spatial correlation structure. By
#' default, we consider the so-called "Earth" distances which are technically
#' called orthodromic distances. They account for earth curvature. The
#' alternative "Euclidean" distances do not. For studies performed on a small
#' geographic scale, both distance methods should lead to similar results.
#'
#' The arguments `control_mean` and `control_dist` are lists that are
#' transmitted to the [`spaMM::spaMM`] fitting functions (defined by
#' `spaMM_method`). These lists can be used to finely control the fitting
#' procedure, so advanced knowledge of the package [`spaMM::spaMM`] is required
#' before messing around with these inputs.
#'
#' We highly recommend users to examine the output produced by isofit.
#' Sometimes, poor fit may occur and such models should therefore not be used
#' for building isoscapes or performing assignments.
#'
#' @aliases isofit print.ISOFIT summary.ISOFIT
#' @param data The *dataframe* containing the data used for fitting the
#'   isoscape model
#' @param mean_model_fix A *list* of *logical* indicating which fixed
#'   effects to consider in mean_fit
#' @param disp_model_fix A *list* of *logical* indicating which fixed
#'   effects to consider in disp_fit
#' @param mean_model_rand A *list* of *logical* indicating which random
#'   effects to consider in mean_fit
#' @param disp_model_rand A *list* of *logical* indicating which random
#'   effects to consider in disp_fit
#' @param uncorr_terms A *list* of two strings defining the parametrization
#'   used to model the uncorrelated random effects for mean_fit and disp_fit
#' @param spaMM_method A *list* of two strings defining the spaMM functions
#'   used for mean_fit and disp_fit
#' @param dist_method A *string* indicating the distance method
#' @param control_mean A *list* of additional arguments to be passed to the
#'   call of mean_fit
#' @param control_disp A *list* of additional arguments to be passed to the
#'   call of disp_fit
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session and `FALSE` otherwise.
#' @return This function returns a *list* of class *ISOFIT* containing
#'   two inter-related fits: `mean_fit` and `disp_fit`. The returned
#'   *list* also contains the object `info_fit` that contains all the
#'   call arguments.
#' @note There is no reason to restrict `mean_fit` and `disp_fit` to
#'   using the same parametrization for fixed and random effects.
#'
#'   Never use a mean_fit object to draw predictions without considering a
#'   disp_fit object: mean_fit is not fitted independently from disp_fit.
#'
#'   For all methods, fixed effects are being estimated by Maximum Likelihood
#'   (ML) and dispersion parameters (i.e. random effects and Matérn correlation
#'   parameters) are estimated by Restricted Maximum Likelihood (REML). Using
#'   REML provides more accurate prediction intervals but impedes the accuracy
#'   of Likelihood Ratio Tests (LRT). Our choice for REML was motivated by the
#'   fact that our package is more likely to be used for drawing inferences than
#'   null hypothesis testing. Users interested in model comparisons may rely on
#'   the conditional AIC values that can be extracted from fitted models using
#'   the function [`spaMM::AIC`] from  \pkg{spaMM}.
#'
#'   Variable names for `data` must be respected to ensure a correct utilization
#'   of this package. Alteration to the fixed effect structure is not
#'   implemented so far (beyond the different options proposed) to avoid misuse
#'   of the package. Users that would require more flexibility should consider
#'   using spaMM directly (see Courtiol & Rousset 2017) or let us know which
#'   other covariates would be useful to add in IsoriX.
#'
#' @seealso [`spaMM::spaMM`] for an overview of the \pkg{spaMM} package
#'
#'   [`spaMM::fitme`] and [`spaMM::corrHLfit`] for
#'   information about the two possible fitting procedures that can be used here
#'
#'   [`spaMM::MaternCorr`] for information about the Matérn
#'   correlation structure
#'
#'   [`prepsources`] for the function preparing the data for isofit
#'
#' @references Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
#'   models. \url{https://www.biorxiv.org/content/10.1101/207662v1}
#'
#' Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson KA, Wassenaar LI & Kramer-Schadt S (2019). Isoscape
#' computation and inference of spatial origins with mixed models using the R package IsoriX. In Hobson KA & Wassenaar LI (eds.),
#' Tracking Animal Migration with Stable Isotopes, second edition. Academic Press, London.
#'
#'  Rousset, F., Ferdy, J. B. (2014). Testing environmental and genetic effects
#'  in the presence of spatial autocorrelation. Ecography, 37(8):781-790.
#'
#'  Bowen, G. J., Wassenaar, L. I., Hobson, K. A. (2005). Global application of
#'  stable hydrogen and oxygen isotopes to wildlife forensics. Oecologia,
#'  143(3):337-348.
#' @source \url{https://kimura.univ-montp2.fr/~rousset/spaMM.htm}
#' @keywords models regression
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 10) {
#'   ## Fitting the models for Germany
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   GermanFit <- isofit(data = GNIPDataDEagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#'
#'   GermanFit
#'
#'   ## Diagnostics for the fits
#'   plot(GermanFit)
#'
#'   ## Exploration of the fitted models
#'   GermanFit$mean_fit
#'   GermanFit$disp_fit
#'   AIC(GermanFit$disp_fit)
#' }
#'
#' @export
isofit <- function(data,
                   mean_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
                   disp_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
                   mean_model_rand = list(uncorr = TRUE, spatial = TRUE),
                   disp_model_rand = list(uncorr = TRUE, spatial = TRUE),
                   uncorr_terms = list(mean_model = "lambda", disp_model = "lambda"), ## or: "nugget"
                   spaMM_method = list(mean_model = "fitme", disp_model = "fitme"), ## or: "corrHLfit", "HLfit"
                   dist_method = "Earth", ## or: "Euclidean"
                   control_mean = list(),
                   control_disp = list(),
                   verbose = interactive()) {
  ## Complete the arguments
  .complete_args(isofit)

  ## Save the call information
  info_fit <- mget(names(formals()))
  info_fit$IsoriX_version <- utils::packageDescription("IsoriX")$Version
  info_fit$verbose <- verbose

  ## Check that arguments are correct and to some extent test that they make sense
  if (!dist_method %in% c("Euclidean", "Earth")) {
    warnings("The argument you chose for dist_method may not be safe to use, please use 'Euclidean' or 'Earth' unless you really know what you are doing.")
  }
  if (!all(unlist(spaMM_method) %in% c("fitme", "corrHLfit", "HLfit"))) {
    stop("The argument(s) you chose for spaMM_method is(are) unknown.")
  }

  ## Partially check that the different arguments are compatible between each others
  if (sum(unlist(mean_model_rand)) == 0 && (spaMM_method$mean_model == "corrHLfit" || spaMM_method$disp_model == "corrHLfit")) {
    stop("Your call does not make sense: the spaMM_method 'corrHLfit' should only be used when random effects are present.")
  }
  if (mean_model_rand[[2]] && spaMM_method$mean_model == "HLfit") {
    stop("Your call does not make sense: the spaMM_method 'HLfit' should only be used when no spatial random effects are present.")
  }
  if (disp_model_rand[[2]] && spaMM_method$disp_model == "HLfit") {
    stop("Your call does not make sense: the spaMM_method 'HLfit' should only be used when no spatial random effects are present.")
  }
  if (!mean_model_rand[[2]] && spaMM_method$mean_model == "corrHLfit") {
    stop("Your call does not make sense: the spaMM_method 'corrHLfit' should only be used when a spatial random effects is present.")
  }
  if (!disp_model_rand[[2]] && spaMM_method$disp_model == "corrHLfit") {
    stop("Your call does not make sense: the spaMM_method 'corrHLfit' should only be used when a spatial random effects is present.")
  }
  if (!mean_model_rand$spatial && !disp_model_rand$spatial &&
    all(unlist(uncorr_terms) != c("lambda", "lambda"))
  ) {
    stop("In the absence of spatial random effects, only 'lambda' can be used as uncorr_terms.")
  }


  ## Prepare the dataset
  data <- .prepare_data_sources(data)

  ## Define the formulas for each model
  mean_formula <- .prepare_formula("mean_source_value ~ 1",
    fix = mean_model_fix, rand = mean_model_rand,
    rand_p = uncorr_terms$mean_model
  )
  disp_formula <- .prepare_formula("var_source_value ~ 1",
    fix = disp_model_fix, rand = disp_model_rand,
    rand_p = uncorr_terms$disp_model
  )

  ## Define weights
  data$weights_mean <- as.numeric(data$n_source_value)
  data$weights_disp <- as.numeric(data$n_source_value) - 1
  if (all(data$weights_disp < 1)) {
    stop("The variable 'n_source_value' must have some observations > 1 to fit the residual dispersion model.")
  }

  ## Define the baseline argument lists for the models irrespective of the spaMM_method
  args_dispfit <- list(
    formula = stats::formula(disp_formula),
    family = stats::Gamma(log),
    prior.weights = data$weights_disp,
    data = data
  )

  args_meanfit <- list(
    formula = stats::formula(mean_formula),
    prior.weights = data$weights_mean,
    resid.model = list(formula = ~ 0 + offset(pred_disp), family = stats::Gamma(identity)),
    data = data
  )

  ## Inclusion of additional arguments for corrHLfit, if necessary
  if (spaMM_method[1] == "corrHLfit") {
    args_meanfit[["control.corrHLfit"]] <- list(maxcorners = 0)
    if (mean_model_rand$spatial) {
      args_meanfit[["control.dist"]] <- list(dist.method = dist_method)
      if (uncorr_terms$mean_model == "nugget") args_meanfit[["init.corrHLfit"]] <- list(Nugget = 0.01)
    }
  }
  if (spaMM_method[2] == "corrHLfit") {
    if (disp_model_rand$spatial) {
      args_dispfit[["control.dist"]] <- list(dist.method = dist_method)
      if (uncorr_terms$disp_model == "nugget") args_dispfit[["init.corrHLfit"]] <- list(Nugget = 0.01)
    }
    args_dispfit[["control.corrHLfit"]] <- list(maxcorners = 0)
    args_dispfit[["ranFix"]] <- list(phi = 2)
  }

  ## Inclusion of additional arguments for fitme, if necessary
  if (spaMM_method[1] == "fitme") {
    args_meanfit[["method"]] <- "REML"
    if (mean_model_rand$spatial) {
      args_meanfit[["control.dist"]] <- list(dist.method = dist_method)
      if (uncorr_terms$mean_model == "nugget") args_meanfit[["init"]] <- list(Nugget = 0.01)
    }
  }
  if (spaMM_method[2] == "fitme") {
    args_dispfit[["method"]] <- "REML"
    args_dispfit[["fixed"]] <- list(phi = 2)
    if (disp_model_rand$spatial) {
      args_dispfit[["control.dist"]] <- list(dist.method = dist_method)
      if (uncorr_terms$disp_model == "nugget") args_dispfit[["init"]] <- list(Nugget = 0.01)
    }
  }

  ## Interactive display
  if (verbose) {
    nug_string <- ifelse(uncorr_terms$disp_model == "nugget", "with a Nugget", "")
    print(paste("Fitting the following residual dispersion model using spaMM", nug_string, ":"), quote = FALSE)
    print(disp_formula)
    if (sum(unlist(disp_model_rand)) > 0) print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Fit disp_fit
  time_disp <- system.time(disp_fit <- do.call(
    eval(parse(text = paste0("spaMM::", spaMM_method$disp_model))),
    c(args_dispfit, control_disp)
  ))

  ## Predict the values for the residual variance
  pred_disp_obj <- .safe_and_quiet_predictions(disp_fit, newdata = data)
  args_meanfit$data$pred_disp <- pred_disp_obj$result[, 1]

  if (length(pred_disp_obj$messages) > 0) {
    message("The following messages were produced by the predictions of residual dispersion: ")
    message(pred_disp_obj$messages)
  }

  if (length(pred_disp_obj$warnings) > 0) {
    message("The following warnings were produced by the predictions of mean isotopic values: ")
    message(pred_disp_obj$warnings)
  }

  ## Interactive display
  if (verbose) {
    nug_string <- ifelse(uncorr_terms$mean_model == "nugget", "with a Nugget", "")
    print(paste("Fitting the following mean model using spaMM", nug_string, ":"), quote = FALSE)
    print(mean_formula)
    if (sum(unlist(mean_model_rand)) > 0) print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Fit mean_fit
  time_mean <- system.time(mean_fit <- do.call(
    eval(parse(text = paste0("spaMM::", spaMM_method$mean_model))),
    c(args_meanfit, control_mean)
  ))

  ## Interactive display of fit time duration
  total_time <- round(as.numeric((time_mean + time_disp)[3]))
  if (verbose) {
    print(paste("Done!"), quote = FALSE)
    print(paste0("Models were fitted in ", total_time, "s."))
  }

  ## Store the time
  info_fit$time_fit <- total_time

  ## Create the return object
  out <- list("mean_fit" = mean_fit, "disp_fit" = disp_fit, "info_fit" = info_fit)

  class(out) <- c("ISOFIT", "list")

  return(invisible(out))
}


#' Fit isoscape models per strata (typically time interval such as months)
#'
#' This function fits several set of isoscapes (e.g. one per strata). It can thus be
#' used to predict annual averages precipitation weighted isoscapes.
#'
#' This function is a wrapper around the function [`isofit`].
#'
#' @return This function returns a *list* of class *MULTIISOFIT*
#'   containing all pairs of inter-related fits (stored under
#'   \code{multi_fits}). The returned *list* also contains the object
#'   `info_fit` that contains all the call arguments.
#'
#' @inheritParams isofit
#' @param split_by A *string* indicating the name of the column of
#'   `data` used to split the dataset. The function
#'   [`isofit`] will then be called on each of these sub-datasets. The
#'   default behaviour is to consider that the dataset should be split per
#'   months (`split_by = "month"`).
#'
#' @seealso [`isofit`] for information about the fitting procedure of
#'   each isoscape.
#'
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## We prepare the GNIP monthly data between January and June for Germany
#'
#'   GNIPDataDEmonthly <- prepsources(
#'     data = GNIPDataDE,
#'     month = 1:6,
#'     split_by = "month"
#'   )
#'
#'   head(GNIPDataDEmonthly)
#'
#'   ## We fit the isoscapes
#'
#'   GermanMonthlyFit <- isomultifit(data = GNIPDataDEmonthly)
#'
#'   GermanMonthlyFit
#'
#'   plot(GermanMonthlyFit)
#' }
#' @export
isomultifit <- function(data,
                        split_by = "month",
                        mean_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
                        disp_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
                        mean_model_rand = list(uncorr = TRUE, spatial = TRUE),
                        disp_model_rand = list(uncorr = TRUE, spatial = TRUE),
                        uncorr_terms = list(mean_model = "lambda", disp_model = "lambda"), ## or: "nugget"
                        spaMM_method = list(mean_model = "fitme", disp_model = "fitme"), ## or: "corrHLfit", "HLfit"
                        dist_method = "Earth", ## or: "Euclidean"
                        control_mean = list(),
                        control_disp = list(),
                        verbose = interactive()) {
  ## Complete the arguments
  .complete_args(isomultifit)

  ## Save the call information
  info_multifit <- info_fit <- mget(names(formals()))
  info_multifit$IsoriX_version <- utils::packageDescription("IsoriX")$Version
  info_multifit$verbose <- verbose

  if (is.null(data[, split_by])) {
    stop(paste("You used 'split_by =", split_by, "' but no column called ',", split_by, "' is found in 'data'..."))
  }

  ## Prepare arguments for call(s) to isofit
  info_fit$split_by <- info_fit$weighting <- NULL ## removes arguments unknown to isofit

  ## Trivial case if no splitting is done
  if (is.null(split_by)) {
    return(do.call(isofit, info_fit))
  }

  ## Interactive display
  if (verbose) {
    print(paste("Fitting the all", length(unique(data[, split_by])), "pairs of models using spaMM:"), quote = FALSE)
    print(paste("(it may take a while...)"), quote = FALSE)
  }

  ## Run all fits
  info_fit$verbose <- FALSE ## no display for each fit
  total_time <- system.time({
    multi_fits <- lapply(unique(data[, split_by]), function(s) {
      info_fit$data <- data[data[, split_by] == s, ]
      fit <- do.call(isofit, info_fit)
      if (verbose) {
        print(paste("fit of the pair of models for", split_by, s, "done"), quote = FALSE)
      }
      return(fit)
    })
  })
  names(multi_fits) <- paste(split_by, unique(data[, split_by]), sep = "_")

  ## Interactive display
  if (verbose) {
    print(paste("Done!"), quote = FALSE)
    print(paste0("All models have been fitted in ", round(as.numeric((total_time)[3])), "s."), quote = FALSE)
  }

  ## Store the time
  info_multifit$time_fit <- total_time

  ## Create the return object
  out <- list("multi_fits" = multi_fits, "info_fit" = info_multifit)

  class(out) <- c("MULTIISOFIT", "ISOFIT", "list")

  return(invisible(out))
}


.prepare_data_sources <- function(data) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It prepares data for the prediction procedures.
  if (!all(c("lat", "long") %in% colnames(data))) {
    stop("The dataset does not seem to contain the required variable(s) 'lat' and/or 'long'.")
  }
  if (is.null(data$var_source_value)) {
    stop("The dataset does not seem to contain the required variable 'var_source_value'.")
  }
  if (is.null(data$n_source_value)) {
    stop("The dataset does not seem to contain the required variable 'n_source_value'.")
  }
  if (any(!is.na(data$var_source_value) & data$var_source_value <= 0)) {
    stop("The dataset seem to contain null or negative value for 'var_source_value'.")
  }
  if (!is.null(data$source_ID)) {
    data$source_ID <- factor(data$source_ID)
  }

  data$lat_abs <- abs(data$lat)
  data$lat_2 <- data$lat^2
  data$long_2 <- data$long^2
  return(data)
}


.prepare_formula <- function(base_formula, fix, rand, rand_p) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It prepares formulas for the fitting procedures.
  if (fix$elev) {
    base_formula <- paste(base_formula, "+ elev")
  }
  if (fix$lat_abs) {
    base_formula <- paste(base_formula, "+ lat_abs")
  }
  if (fix$lat_2) {
    base_formula <- paste(base_formula, "+ lat_2")
  }
  if (fix$long) {
    base_formula <- paste(base_formula, "+ long")
  }
  if (fix$long_2) {
    base_formula <- paste(base_formula, "+ long_2")
  }
  if (rand$uncorr && rand_p == "lambda") {
    base_formula <- paste(base_formula, "+ (1|source_ID)")
  }
  if (rand$spatial) {
    base_formula <- paste(base_formula, "+ Matern(1|long + lat)")
  }

  return(base_formula)
}

#' @export
#' @method print ISOFIT
print.ISOFIT <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}

#' @export
#' @method summary ISOFIT
summary.ISOFIT <- function(object, ...) {
  if (!inherits(object, "MULTIISOFIT")) {
    cat("\n")
    cat("### spaMM summary of the fit of the mean model ###", "\n")
    cat("\n")
    print(spaMM::summary.HLfit(object$mean_fit)$summ)
    cat("\n")
    cat("\n")
    cat("### spaMM summary of the fit of the residual dispersion model ###", "\n")
    cat("\n")
    print(spaMM::summary.HLfit(object$disp_fit)$summ)
    cat("\n")
    cat(paste("[models fitted with spaMM version ", object$mean_fit$spaMM.version, "]", sep = ""), "\n")
    cat("\n")
  } else {
    for (fit in seq_along(object$multi_fits)) {
      cat("\n")
      cat(paste("##### Pair of models", names(object$multi_fits)[fit]), "#####")
      cat("\n")
      Recall(object$multi_fits[[fit]])
    }
  }
  return(invisible(NULL))
}
