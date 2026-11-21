#' Compute the conditional AIC of fitted isoscapes
#'
#' This function computes the conditional AIC (cAIC) of the isoscape models
#' fitted with [`isofit`] or [`isomultifit`], corrected for the number of
#' parameters estimated in the residual dispersion model. This corrected cAIC
#' measures the ability of the joint fit of the mean model and of the residual
#' dispersion model to predict the isotopic values observed at the source
#' locations. It thus allows for the comparison of isoscape models differing
#' not only in the structure of their mean model but also in the structure of
#' their residual dispersion model.
#'
#' The cAIC computed by \pkg{spaMM} for the mean model (`mean_fit`) treats the
#' residual dispersion variance (`pred_disp`) as known, while it has been
#' predicted from the fit of the residual dispersion model (`disp_fit`). This
#' cAIC thus does not account for the uncertainty stemming from the estimation
#' of the parameters of the residual dispersion model. Following Courtiol &
#' Rousset (2017), we correct for that by adding twice the number of parameters
#' estimated in the residual dispersion model (`K_disp`) to the cAIC of the mean
#' model (see equation 6 in Overholser & Xu 2014):
#'
#' \deqn{cAIC = cAIC_{mean\_fit} + 2 \times K_{disp}}
#'
#' `K_disp` is the number of fixed-effect coefficients, plus the number of
#' random-effect variances, plus the number of parameters of the Matérn
#' correlation function (including the nugget, if any), plus the number of
#' residual dispersion parameters that have been estimated in `disp_fit` (see
#' [`spaMM::DoF`]). Parameters kept fixed during the fit (e.g. the residual
#' variance of `disp_fit`, which is fixed to its theoretical expectation) are not
#' counted.
#'
#' For a fit produced by [`isomultifit`], the cAIC is the sum of the corrected
#' cAIC of all the pairs of models (one per strata), since all pairs are fitted
#' independently on non-overlapping data. Similarly, `K_mean` and `K_disp` are
#' summed over all pairs.
#'
#' As for any information criterion, only models fitted on the same data can be
#' compared. The lower the cAIC, the better the predictive power of the fitted
#' isoscape. Since the goal of an isoscape is prediction, there is no need to
#' favour simpler models beyond what the cAIC already accounts for. Note,
#' however, that the cAIC only reflects the quality of the predictions at the
#' source locations, so always check the isoscapes produced by the selected
#' model (e.g. by plotting the prediction variance; see [`isoscape`]).
#'
#' @param object The output of a call to [`isofit`] or [`isomultifit`].
#' @param ... Optionally, more fitted objects created by [`isofit`] or
#'   [`isomultifit`].
#' @param k The penalty per parameter, which must be equal to 2 (the default)
#'   since the cAIC computed by \pkg{spaMM} does not allow for other values.
#'   This argument only exists for consistency with [`stats::AIC`].
#'
#' @return If a single fitted object is provided, the corrected cAIC (a
#'   *numeric* value). If several fitted objects are provided, a *data.frame*
#'   with one row per object and the following columns: the number of
#'   parameters estimated in the mean model (`K_mean`) and in the residual
#'   dispersion model (`K_disp`), the cAIC of the mean model before correction
#'   (`cAIC_uncorrected`) and the corrected cAIC (`cAIC`).
#'
#' @seealso [`isofit`] for fitting the isoscape models
#'
#'   [`spaMM::AIC`] for the different information criteria computed by
#'   \pkg{spaMM} on each of the two fits
#'
#'   [`spaMM::DoF`] for the extraction of the number of estimated parameters
#'
#' @references Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
#'   models. \url{https://www.biorxiv.org/content/10.1101/207662v1}
#'
#'   Overholser, R., Xu, R. (2014). Effective degrees of freedom and its
#'   application to conditional AIC for linear mixed-effects models with
#'   correlated error structures. Journal of Multivariate Analysis, 132:160-170.
#'   \doi{10.1016/j.jmva.2014.08.004}.
#'
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 20) {
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   ## Fitting two isoscape models differing in their residual dispersion model
#'   GermanFit1 <- isofit(
#'     data = GNIPDataDEagg,
#'     mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
#'   )
#'
#'   GermanFit2 <- isofit(
#'     data = GNIPDataDEagg,
#'     mean_model_fix = list(elev = TRUE, lat_abs = TRUE),
#'     disp_model_fix = list(elev = TRUE)
#'   )
#'
#'   ## Corrected cAIC of a single fit
#'   AIC(GermanFit1)
#'
#'   ## Comparison of the two fits
#'   AIC(GermanFit1, GermanFit2)
#' }
#'
#' @export
#' @method AIC ISOFIT
#' @importFrom stats AIC
AIC.ISOFIT <- function(object, ..., k = 2) {
  if (!isTRUE(all.equal(k, 2))) {
    stop("The argument `k` must be equal to 2 since the conditional AIC computed by spaMM does not allow for other values.")
  }

  fits <- c(list(object), list(...))

  if (!all(vapply(fits, function(fit) inherits(fit, "ISOFIT"), logical(1)))) {
    stop("All objects must have been created by isofit() or isomultifit().")
  }

  if (length(fits) == 1L) {
    return(.cAIC_isofit(object)[["cAIC"]])
  }

  out <- as.data.frame(do.call(rbind, lapply(fits, .cAIC_isofit)))
  call <- match.call()
  call$k <- NULL
  rownames(out) <- make.unique(vapply(as.list(call)[-1L], function(arg) paste(deparse(arg), collapse = ""), character(1)))

  out
}


.cAIC_isofit <- function(object) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It computes the conditional AIC of the mean model, corrected for the
  ## number of parameters estimated in the residual dispersion model (#101).
  if (inherits(object, "MULTIISOFIT")) {
    return(Reduce(`+`, lapply(object$multi_fits, .cAIC_isofit)))
  }

  K_mean <- sum(spaMM::DoF(object$mean_fit))
  K_disp <- sum(spaMM::DoF(object$disp_fit))
  cAIC_uncorrected <- spaMM::get_any_IC(object$mean_fit, verbose = FALSE, short.names = TRUE)[["cAIC"]]

  c(
    K_mean = K_mean,
    K_disp = K_disp,
    cAIC_uncorrected = unname(cAIC_uncorrected),
    cAIC = unname(cAIC_uncorrected) + 2 * K_disp
  )
}
