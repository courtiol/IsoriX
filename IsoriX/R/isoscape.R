#' Predicts the spatial distribution of source isotopic values
#'
#' This function produces the set of isoscapes, i.e. the spatial prediction
#' (i.e. maps) of the distribution of source isotopic values, as well as several
#' variances around such predictions. The predictions are computed using the
#' fitted geostatistical models for each raster cell of a structural raster.
#' All shape files can be exported and loaded into any Geographic Information
#' System (GIS) if needed (see online tutorials).
#'
#' This function computes the predictions (`mean`), prediction variances
#' (`mean_predVar`), residual variances (`mean_residVar`) and response
#' variances (`mean_respVar`) for the isotopic values at a resolution equal
#' to the one of the structural raster. It also computes the same information
#' for the residual dispersion variance (`disp_pred`, `disp_predVar`,
#' `disp_residVar`, or `disp_respVar`).
#'
#' The predictions of isotopic values across the landscape are performed by
#' calling the function [`spaMM::predict`] from the package
#' \pkg{spaMM} on the fitted isoscape produced by
#' [`isofit`].
#'
#' Let us summarize the meaning of `mean`, `mean_predVar`,
#' `mean_residVar` and `mean_respVar` (see Courtiol & Rousset 2017 and
#' Courtiol et al. 2019 for more details):
#'
#' Our model assumes that that there is a single true unknown isoscape, which is
#' fixed but which is represented by the mixed-effect model as a random draw
#' from possible realizations of isoscapes (random draws of the
#' Matérn-correlated process and of the uncorrelated random effects if
#' considered). We infer this realized isoscape by fitting the model to a
#' limited amount of data, with some uncertainty since different random draws of
#' the unknown isoscape may give the same observed data. There is thus a
#' conditional distribution of possible true isoscapes given the data. For
#' linear mixed-effects models, the mean prediction is the mean of this
#' conditional distribution. The prediction variance is ideally the mean square
#' difference between the true unknown value of the linear predictor and the
#' mean prediction at a given location. The residual variance is simply the
#' prediction of the variance in isotopic value at a given location. Its exact
#' meaning depends on the aggregation scheme used in [`prepsources`],
#' but by default, it would correspond to the temporal variation between months
#' and across years. The response variance estimates the variance of new
#' observations drawn from the true unknown isoscape at a given location. The
#' response variance is simply equal to the sum of the prediction variance and
#' the residual variance (note that the residual variance considered assume that
#' a single observation is being observed per location).
#'
#' The isoscape can be plotted using the function [`plot.ISOSCAPE`]
#' (see examples).
#'
#' @aliases isoscape print.isoscape summary.isoscape
#' @param raster The structural raster (*SpatRaster*) such as an elevation
#'   raster created using [`prepelev`]
#' @param isofit The fitted isoscape created by [`isofit`]
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session and `FALSE` otherwise.
#' @return This function returns a *list* of class *ISOSCAPE* containing
#'   a set of all 8 raster layers mentioned above (all being of class
#'   *SpatRaster*), and the location of the sources as spatial points.
#' @seealso [`isofit`] for the function fitting the isoscape
#'
#'   [`plot.ISOSCAPE`] for the function plotting the isoscape model
#'
#' @references Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
#'   models. \url{https://www.biorxiv.org/content/10.1101/207662v1}
#'
#' Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson KA, Wassenaar LI, Kramer-Schadt S (2019). Isoscape
#' computation and inference of spatial origins with mixed models using the R package IsoriX. In Hobson KA, Wassenaar LI (eds.),
#' Tracking Animal Migration with Stable Isotopes, second edition. Academic Press, London.
#'
#' @keywords models regression prediction predict
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## We prepare the data
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   ## We fit the models
#'   GermanFit <- isofit(
#'     data = GNIPDataDEagg,
#'     mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
#'   )
#'
#'   ## We build the isoscapes
#'   GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#'   GermanScape
#'   plot(GermanScape)
#'
#'   ## We build more plots
#'   PlotMean <- plot(x = GermanScape, which = "mean", plot = FALSE)
#'
#'   PlotMeanPredVar <- plot(x = GermanScape, which = "mean_predVar", plot = FALSE)
#'
#'   PlotMeanResidVar <- plot(x = GermanScape, which = "mean_residVar", plot = FALSE)
#'
#'   PlotMeanRespVar <- plot(x = GermanScape, which = "mean_respVar", plot = FALSE)
#'
#'   ## We display the plots
#'   print(PlotMean, split = c(1, 1, 2, 2), more = TRUE)
#'   print(PlotMeanPredVar, split = c(2, 1, 2, 2), more = TRUE)
#'   print(PlotMeanResidVar, split = c(1, 2, 2, 2), more = TRUE)
#'   print(PlotMeanRespVar, split = c(2, 2, 2, 2), more = FALSE)
#'
#'   ## We build a sphere with our isoscape
#'   plot(x = GermanScape, which = "mean", plot = FALSE, sphere = list(build = TRUE))
#'
#'   ## We can save a rotating sphere with the isoscape as a .gif-file.
#'   ## This file will be located inside your working directory.
#'   ## Make sure your current rgl device (from the previous step) is still open
#'   ## and that you have both the packages 'rgl' and 'magick' installed.
#'   ## The building of the .gif implies to create temporarily many .png
#'   ## but those will be removed automatically once the .gif is done.
#'   ## Uncomment to proceed (after making sure you have rgl, magick & webshot2 installed)
#'   # if(require("rgl") && require("magick") && require("webshot2")) {
#'   #   movie3d(spin3d(axis = c(0, 0, 1), rpm = 2), duration = 30, dir = getwd())
#'   # }
#' }
#'
#' @export

isoscape <- function(raster,
                     isofit,
                     verbose = interactive()) {
  if (inherits(isofit, "MULTIISOFIT")) {
    stop("Object 'isofit' of class MULTIISOFIT; use isomultiscape instead!")
  }

  if (verbose) {
    print("Building the isoscapes... ", quote = FALSE)
    print("(this may take a while)", quote = FALSE)
  }

  if (isofit$mean_fit$spaMM.version != utils::packageVersion(pkg = "spaMM")) {
    warning("The isofit has been fitted on a different version of spaMM than the one called by IsoriX. This may create troubles in paradize...")
  }

  time <- system.time({
    ## we extract lat/long from all cells of the raster
    coord <- terra::crds(raster, na.rm = FALSE)
    long_to_do <- coord[, "x"] # extract the longitude
    lat_to_do <- coord[, "y"] # extract the lattitude
    rm(coord)
    gc() ## remove coord as it can be a large object

    ## size of chunks to split the job into smaller ones
    chunk_size_for_predict <- 1000L

    ## indexes of beginning of each chunk (- 1) and of last position are being computed
    steps <- unique(c(
      seq(from = 0L, to = length(long_to_do), by = chunk_size_for_predict),
      length(long_to_do)
    ))

    ## a logical indicating if a progression bar must be used
    draw_pb <- interactive() & (length(steps) - 1) > 2

    ## create empty vectors to store predictions
    mean_pred <- disp_pred <- rep(NA, length(long_to_do))
    mean_predVar <- mean_residVar <- mean_respVar <- mean_pred
    disp_predVar <- disp_residVar <- disp_respVar <- disp_pred

    ## initiate the progress bar
    if (draw_pb) {
      pb <- utils::txtProgressBar(style = 3)
    }

    ## we build xs non-specifically using most complex model definition
    ## (it may look ugly but it should not increase much the computation time
    ## and it avoids a lot of uglier code)
    xs <- data.frame(
      long = long_to_do,
      long_2 = long_to_do^2,
      lat = lat_to_do,
      lat_abs = abs(lat_to_do),
      lat_2 = lat_to_do^2,
      elev = terra::extract(raster, cbind(long_to_do, lat_to_do))[[1]], ## ToDo: check that it is elev and not something else
      source_ID = as.factor(paste("new", seq_len(length(long_to_do)), sep = "_"))
    )


    ## we loop on each chunk
    messages_meanpred <- character()
    messages_disppred <- character()
    warnings_meanpred <- character()
    warnings_disppred <- character()

    for (i in 1:(length(steps) - 1)) {
      ## compute indexes for covariate values matching the current chunk
      within_steps <- (steps[i] + 1L):steps[i + 1L]

      ## we select the chunk of the design matrix required for the loop
      xs_small <- xs[within_steps, ]

      ## predictions from disp_fit
      pred_dispfit <- .safe_and_quiet_predictions(
        object = isofit$disp_fit,
        newdata = xs_small,
        variances = list(respVar = TRUE)
      )

      messages_disppred <- c(messages_disppred, pred_dispfit$messages)
      warnings_disppred <- c(warnings_disppred, pred_dispfit$warnings)
      pred_dispfit <- pred_dispfit$result

      index_disp <- as.integer(rownames(pred_dispfit)) ## need since predictions drop values if NA in predictors

      ## transmission of phi to mean_fit
      xs_small[rownames(pred_dispfit), "pred_disp"] <- pred_dispfit[, 1]

      ## predictions from mean_fit
      pred_meanfit <- .safe_and_quiet_predictions(
        object = isofit$mean_fit,
        newdata = xs_small,
        variances = list(respVar = TRUE)
      )

      messages_meanpred <- c(messages_meanpred, pred_meanfit$messages)
      warnings_meanpred <- c(warnings_meanpred, pred_meanfit$warnings)
      pred_meanfit <- pred_meanfit$result

      index_mean <- as.integer(rownames(pred_meanfit)) ## need since predictions drop values if NA in predictors

      ## we save the predictions
      mean_pred[index_mean] <- pred_meanfit[, 1]
      mean_predVar[index_mean] <- attr(pred_meanfit, "predVar")
      mean_residVar[index_mean] <- attr(pred_meanfit, "residVar") ## same as disp_pred (as it should be)
      mean_respVar[index_mean] <- attr(pred_meanfit, "respVar")

      disp_pred[index_disp] <- pred_dispfit[, 1]
      disp_predVar[index_disp] <- attr(pred_dispfit, "predVar") ## same as mean_residVar (as it should be)
      disp_residVar[index_disp] <- attr(pred_dispfit, "residVar")
      disp_respVar[index_disp] <- attr(pred_dispfit, "respVar")

      if (draw_pb) {
        utils::setTxtProgressBar(pb, steps[i + 1L] / length(lat_to_do)) ## update progress bar
      }
    } ## we leave the loop on chunks

    ## the progress bar is being closed
    if (draw_pb) close(pb)
  }) ## end of system.time

  messages_meanpred <- unique(messages_meanpred)
  messages_disppred <- unique(messages_disppred)
  warnings_meanpred <- unique(warnings_meanpred)
  warnings_disppred <- unique(warnings_disppred)

  if (length(messages_meanpred) > 0) {
    message("The following messages were produced by the predictions of mean isotopic values: ")
    message(messages_meanpred)
  }

  if (length(messages_disppred) > 0) {
    message("The following messages were produced by the predictions of residual dispersion: ")
    message(messages_disppred)
  }

  if (length(warnings_meanpred) > 0) {
    message("The following warnings were produced by the predictions of mean isotopic values: ")
    message(warnings_meanpred)
  }

  if (length(warnings_disppred) > 0) {
    message("The following warnings were produced by the predictions of residual dispersion: ")
    message(warnings_disppred)
  }

  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste0(
      "predictions for all ", length(long_to_do),
      " locations have been computed in ", time, "s."
    ), quote = FALSE)
  }

  ## we store the predictions for mean isotopic values into a raster
  save_raster <- function(x) {
    .create_raster(
      long = long_to_do,
      lat = lat_to_do,
      values = x,
      proj = "+proj=longlat +datum=WGS84"
    )
  }

  mean_raster <- save_raster(mean_pred)
  mean_predVar_raster <- save_raster(mean_predVar)
  mean_residVar_raster <- save_raster(mean_residVar)
  mean_respVar_raster <- save_raster(mean_respVar)

  disp_raster <- save_raster(disp_pred)
  disp_predVar_raster <- save_raster(disp_predVar)
  disp_residVar_raster <- save_raster(disp_residVar)
  disp_respVar_raster <- save_raster(disp_respVar)


  ## we create the spatial points for sources
  source_points <- .create_spatial_points(
    long = isofit$mean_fit$data$long,
    lat = isofit$mean_fit$data$lat,
    proj = "+proj=longlat +datum=WGS84"
  )

  ## we put all rasters in a brick
  isoscapes <- terra::rast(list(
    "mean" = mean_raster,
    "mean_predVar" = mean_predVar_raster,
    "mean_residVar" = mean_residVar_raster,
    "mean_respVar" = mean_respVar_raster,
    "disp" = disp_raster,
    "disp_predVar" = disp_predVar_raster,
    "disp_residVar" = disp_residVar_raster,
    "disp_respVar" = disp_respVar_raster
  ))

  ## we put the brick in a list that also contains
  ## the spatial points for the sources
  out <- list(
    isoscapes = isoscapes,
    sp_points = list(sources = source_points)
  )

  ## store design matrix as attribute
  attr(out, "xs") <- xs

  ## we define a new class
  class(out) <- c("ISOSCAPE", "list")

  return(out)
}

.futureisoscape <- function(raster, ## change as method?
                            isofit,
                            verbose = interactive()) {
  # Predicts the spatial distribution of isotopic values
  #
  # This function is an alternative implementation of isoscape().
  # It is not exported but may be put in use in a future version of IsoriX.
  # It does not compute the predictions into chunks.

  if (inherits(isofit, "MULTIISOFIT")) {
    stop("object 'isofit' of class MULTIISOFIT; use isomultiscape instead.")
  }

  if (verbose) {
    print("Building the isoscapes... ", quote = FALSE)
    print("(this may take a while)", quote = FALSE)
  }

  if (isofit$mean_fit$spaMM.version != utils::packageVersion(pkg = "spaMM")) {
    warning("The isofit has been fitted on a different version of spaMM than the one called by IsoriX. This may create troubles in paradize...")
  }

  time <- system.time({
    ## we extract lat/long from all cells of the raster
    coord <- terra::crds(raster, na.rm = FALSE)

    ## we create the object for newdata
    xs <- data.frame(
      long = coord[, 1],
      long_2 = coord[, 1]^2,
      lat = coord[, 2],
      lat_abs = abs(coord[, 2]),
      lat_2 = coord[, 2]^2,
      elev = terra::extract(raster, coord)[[1]], ## ToDo: check that it is elev
      source_ID = as.factor(paste("new", seq_len(nrow(coord)), sep = "_"))
    )

    rm(coord)
    gc() ## remove coord as it can be a large object

    pred_dispfit <- .safe_and_quiet_predictions(
      object = isofit$disp_fit,
      newdata = xs,
      variances = list(respVar = TRUE),
      blockSize = 16000L
    )

    ## transmission of phi to mean_fit
    xs$pred_disp <- pred_dispfit[, 1]

    ## predictions from mean_fit
    pred_meanfit <- .safe_and_quiet_predictions(
      object = isofit$mean_fit,
      newdata = xs,
      variances = list(respVar = TRUE),
      blockSize = 16000L
    )

    mean_pred <- pred_meanfit[, 1]
    mean_predVar <- attr(pred_meanfit, "predVar")
    mean_residVar <- attr(pred_meanfit, "residVar") ## same as disp_pred (as it should be)
    mean_respVar <- attr(pred_meanfit, "respVar")

    disp_pred <- pred_dispfit[, 1]
    disp_predVar <- attr(pred_dispfit, "predVar") ## same as mean_residVar (as it should be)
    disp_residVar <- attr(pred_dispfit, "residVar")
    disp_respVar <- attr(pred_dispfit, "respVar")
  }) ## end of system.time

  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste0(
      "predictions for all ", nrow(xs),
      " locations have been computed in ", time, "s."
    ), quote = FALSE)
  }

  ## we store the predictions for mean isotopic values into a raster
  save_raster <- function(x) {
    .create_raster(
      long = xs$long,
      lat = xs$lat,
      values = x,
      proj = "+proj=longlat +datum=WGS84"
    )
  }

  mean_raster <- save_raster(mean_pred)
  mean_predVar_raster <- save_raster(mean_predVar)
  mean_residVar_raster <- save_raster(mean_residVar)
  mean_respVar_raster <- save_raster(mean_respVar)

  disp_raster <- save_raster(disp_pred)
  disp_predVar_raster <- save_raster(disp_predVar)
  disp_residVar_raster <- save_raster(disp_residVar)
  disp_respVar_raster <- save_raster(disp_respVar)


  ## we create the spatial points for sources
  source_points <- .create_spatial_points(
    long = isofit$mean_fit$data$long,
    lat = isofit$mean_fit$data$lat,
    proj = "+proj=longlat +datum=WGS84"
  )

  ## we put all rasters in a brick
  isoscapes <- terra::rast(list(
    "mean" = mean_raster,
    "mean_predVar" = mean_predVar_raster,
    "mean_residVar" = mean_residVar_raster,
    "mean_respVar" = mean_respVar_raster,
    "disp" = disp_raster,
    "disp_predVar" = disp_predVar_raster,
    "disp_residVar" = disp_residVar_raster,
    "disp_respVar" = disp_respVar_raster
  ))

  ## we put the brick in a list that also contains
  ## the spatial points for the sources
  out <- list(
    isoscapes = isoscapes,
    sp_points = list(sources = source_points)
  )

  ## we define a new class
  class(out) <- c("ISOCAPE", "list")
  return(out)
}



#' Predicts the average spatial distribution of isotopic values over months,
#' years...
#'
#' This function is the counterpart of [`isoscape`] for the objects
#' created with [`isomultifit`]. It creates the isoscapes for each
#' strata (e.g. month) defined by `split_by` during the call to
#' [`isomultifit`] and the aggregate them. The function can handle
#' weighting for the aggregation process and can thus be used to predict annual
#' averages precipitation weighted isoscapes.
#'
#' @inheritParams isoscape
#' @param weighting An optional RasterBrick containing the weights
#' @return This function returns a *list* of class *ISOSCAPE*
#' containing a set of all 8 raster layers mentioned above (all being of
#' class *SpatRaster*), and the location of the sources as spatial points.
#' @seealso
#'
#' [`isoscape`] for details on the function used to compute the isoscapes for each strata

#' [`isomultifit`] for the function fitting the isoscape
#'
#' [`plot.ISOSCAPE`] for the function plotting the isoscape model
#'
#' [`IsoriX`] for the complete work-flow
#'
#' @keywords models regression prediction predict
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 180) {
#'   ## We prepare the data and split them by month:
#'
#'   GNIPDataDEmonthly <- prepsources(
#'     data = GNIPDataDE,
#'     split_by = "month"
#'   )
#'
#'   dim(GNIPDataDEmonthly)
#'
#'   ## We fit the isoscapes:#'
#'   GermanMultiFit <- isomultifit(
#'     data = GNIPDataDEmonthly,
#'     mean_model_fix = list(elev = TRUE, lat.abs = TRUE)
#'   )
#'
#'   ## We build the annual isoscapes by simple averaging (equal weighting):
#'   GermanMultiscape <- isomultiscape(
#'     raster = ElevRasterDE,
#'     isofit = GermanMultiFit
#'   )
#'
#'   ## We build the annual isoscapes with a weighing based on precipitation amount:
#'   GermanMultiscapeWeighted <- isomultiscape(
#'     raster = ElevRasterDE,
#'     isofit = GermanMultiFit,
#'     weighting = PrecipBrickDE
#'   )
#'
#'   ## We plot the mean isoscape of the averaging with equal weighting:
#'   plot(x = GermanMultiscape, which = "mean")
#'
#'   ## We plot the mean isoscape of the averaging with precipitation weighting:
#'   plot(x = GermanMultiscapeWeighted, which = "mean")
#'
#'   ## We build the isoscapes for a given month (here January):
#'   GermanScapeJan <- isoscape(
#'     raster = ElevRasterDE,
#'     isofit = GermanMultiFit$multi_fits[["month_1"]]
#'   )
#'
#'   ## We plot the mean isoscape for January:
#'   plot(x = GermanScapeJan, which = "mean")
#' }
#' @export

isomultiscape <- function(raster, ## change as method?
                          isofit,
                          weighting = NULL,
                          verbose = interactive()) {
  ## In case the function is called on the output of isofit by mistake
  if (!inherits(isofit, "MULTIISOFIT")) {
    message("Your input for `isofit =` is not of class MULTIISOFIT, so your call to `isomultiscape()` has been automatically converted into a call to `isoscape()`.")
    return(isoscape(
      raster = raster,
      isofit = isofit,
      verbose = verbose
    ))
  }

  ## Checking the inputs
  if (!is.null(weighting)) {
    if (!inherits(weighting, "SpatRaster")) {
      stop("the argument 'weighting' should be a SpatRaster")
    }
    if (!all(names(isofit$multi.fits) %in% names(weighting))) {
      stop("the names of the layer in the object 'weighting' do not match those of your pairs of fits...")
    }
    if (terra::ext(weighting) != terra::ext(raster)) {
      stop("the extent of the object 'weighting' and 'raster' differ")
    }
    if (terra::ncell(weighting) != terra::ncell(raster)) {
      stop("the resolution of the object 'weighting' and 'raster' differ")
    }
  }

  isoscapes <- lapply(
    names(isofit$multi_fits),
    function(fit) {
      if (verbose) {
        print(paste("#### building isoscapes for", fit, " in progress ####"), quote = FALSE)
      }
      iso <- isoscape(
        raster = raster,
        isofit = isofit$multi_fits[[fit]],
        verbose = verbose
      )
      iso$sp_points$sources$values <- fit ## set values for sp.points
      return(iso)
    }
  )

  names(isoscapes) <- names(isofit$multi_fits)

  ## Combining mean isoscapes into RasterBricks
  brick_mean <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$mean))
  brick_mean_predVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$mean_predVar))
  brick_mean_residVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$mean_residVar))
  brick_mean_respVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$mean_respVar))

  ## Combining disp isoscapes into RasterBricks
  brick_disp <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$disp))
  brick_disp_predVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$disp_predVar))
  brick_disp_residVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$disp_residVar))
  brick_disp_respVar <- terra::rast(lapply(isoscapes, function(iso) iso$isoscapes$disp_respVar))

  ## Compute the weights
  if (is.null(weighting)) {
    weights <- terra::rast(raster)
    weights <- terra::setValues(weights, 1 / length(isoscapes))
  } else {
    weights <- weighting / sum(weighting)
  }

  ## Compute the weighted averages and store then in a list of RasterBricks
  multiscape <- terra::rast(list(
    "mean" = sum(brick_mean * weights),
    "mean_predVar" = sum(brick_mean_predVar * weights^2),
    "mean_residVar" = sum(brick_mean_residVar * weights^2),
    "mean_respVar" = sum(brick_mean_respVar * weights^2),
    "disp" = sum(brick_disp * weights),
    "disp_predVar" = sum(brick_disp_predVar * weights^2),
    "disp_residVar" = sum(brick_disp_residVar * weights^2),
    "disp_respVar" = sum(brick_disp_respVar * weights^2)
  ))

  ## Agglomerate the sources spatial points
  # source_points <- Reduce("+", lapply(isoscapes, function(iso) iso$sp_points$sources)) ## https://github.com/rspatial/terra/issues/1337
  source_points <- Reduce("rbind", lapply(isoscapes, function(iso) iso$sp_points$sources))
  terra::values(source_points) <- NULL
  source_points <- terra::unique(source_points)

  ## we put the brick in a list that also contains
  ## the spatial points for the sources
  out <- list(
    isoscapes = multiscape,
    sp_points = list(sources = source_points)
  )

  ## we define a new class
  class(out) <- c("ISOSCAPE", "list")

  return(out)
}

#' @export
#' @method print ISOSCAPE
print.ISOSCAPE <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}

#' @export
#' @method summary ISOSCAPE
summary.ISOSCAPE <- function(object, ...) {
  if (inherits(object, "ISOSIM")) {
    cat("\n")
    cat("### Note: this isoscape has been simulated ###", "\n")
    cat("\n")
  }
  cat("### Multilayered raster containing the isoscapes", "\n")
  print(object[[1]])
  cat("\n")
  if (length(object) > 1) {
    cat("### Points of the sampled sources", "\n")
    print(methods::show(object[[2]][[1]]))
  }
  return(invisible(NULL))
}
