#' @rdname IsoriX-defunct
#' @export
Isoscape <- function(...) {
  .Defunct("isoscape")
}


#' Predicts the spatial distribution of source isotopic values
#'
#' This function produces the set of isoscapes, i.e. the spatial prediction
#' (i.e. maps) of the distribution of source isotopic values, as well as several
#' variances around such predictions. The predictions are computed using the
#' fittedm geostatistical models for each raster cell of a structural raster.
#' All shape files can be exported and loaded into any Geographic Information
#' System (GIS) if needed (see online tutorials).
#'
#' This function computes the predictions (\code{mean}), prediction variances
#' (\code{mean_predVar}), residual variances (\code{mean_residVar}) and response
#' variances (\code{mean_respVar}) for the isotopic values at a resolution equal
#' to the one of the structural raster. It also computes the same information
#' for the residual dispersion variance (\code{disp_pred}, \code{disp_predVar},
#' \code{disp_residVar}, or \code{disp_respVar}).
#'
#' The predictions of isotopic values across the landscape are performed by
#' calling the function \code{\link[spaMM]{predict}} from the package
#' \pkg{\link[spaMM]{spaMM}} on the fitted isoscape produced by
#' \code{\link{isofit}}.
#'
#' Let us summarize the meaning of \code{mean}, \code{mean_predVar} and
#' \code{mean_respVar} (see Courtiol & Rousset 2017 for more details):
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
#' mean prediction at a given location. The response variance has a different
#' meaning. It estimates the variance of new observations drawn from the true
#' unknown isoscape at a given location. The response variance is simply equal
#' to the sum of the prediction variance and the residual variance (note that
#' the residual variance considered assume that a single observation is being
#' observed per location).
#'
#' The isoscape can be plotted using the function \code{\link{plot.isoscape}}
#' (see examples).
#'
#' @aliases isoscape print.isoscape summary.isoscape
#' @param raster The structural raster (\var{RasterLayer}) such as an elevation
#'   raster created using \code{\link{prepelev}}
#' @param isofit The fitted isoscape created by \code{\link{isofit}}
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \var{TRUE} if users use an interactive R
#'   session and \var{FALSE} otherwise.
#' @return This function returns a \var{list} of class \var{ISOSCAPE} containing
#'   a stack of all 8 raster layers mentioned above (all being of class
#'   \var{RasterLayer}), and the location of the sources as spatial points.
#' @seealso \code{\link{isofit}} for the function fitting the isoscape
#'
#'   \code{\link{plot.ISOSCAPE}} for the function plotting the isoscape model
#'
#'   \code{\link{IsoriX}} for the complete workflow
#'
#' @references Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
#'   models. \url{https://www.biorxiv.org/content/early/2017/10/23/207662}
#'
#' @keywords models regression prediction predict
#' @examples
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## We prepare the data
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ## We fit the models
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' ## We build the isoscapes
#' GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#' 
#' GermanScape
#' plot(GermanScape)
#' 
#' ## We build more plots
#' PlotMean <- plot(x = GermanScape, which = "mean", plot = FALSE)
#' 
#' PlotMeanPredVar <- plot(x = GermanScape, which = "mean_predVar", plot = FALSE)
#' 
#' PlotMeanResidVar <- plot(x = GermanScape, which = "mean_residVar", plot = FALSE)
#' 
#' PlotMeanRespVar <- plot(x = GermanScape, which = "mean_respVar", plot = FALSE)
#' 
#' ## We display the plots
#' if(require(lattice)) {
#'  print(PlotMean, split = c(1, 1, 2, 2), more = TRUE)
#'  print(PlotMeanPredVar,   split = c(2, 1, 2, 2), more = TRUE)
#'  print(PlotMeanResidVar,  split = c(1, 2, 2, 2), more = TRUE)
#'  print(PlotMeanRespVar,   split = c(2, 2, 2, 2), more = FALSE)
#'  }
#'  
#' ## We build a sphere with our isoscape
#' plot(x = GermanScape, which = "mean", plot = FALSE, sphere = list(build = TRUE))
#'  
#' ## We can save a rotating sphere with the isoscape as a .gif-file.
#' ## This file will be located inside your working directory.
#' ## Make sure your current rgl device (from the previous step) is still open
#' ## and that you have both the packages 'rgl' and 'magick' installed.
#' ## The building of the .gif implies to create temporarily many .png
#' ## but those will be removed automatically once the .gif is done.
#' if(require("rgl") & require("magick")) {
#'   movie3d(spin3d(axis = c(0, 0, 1), rpm = 2), duration = 30, dir = getwd())
#' }
#' 
#' }
#' 
#' @export

isoscape <- function(raster, ## change as method?
                     isofit,
                     verbose = interactive()) {
  
  if (any(class(isofit) %in% "MULTIISOFIT")) {
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
    coord <- sp::coordinates(raster)
    long_to_do <- coord[, 1]  # extract the longitude
    lat_to_do <-  coord[, 2]  # extract the lattitude
    rm(coord); gc()  ## remove coord as it can be a large object
    
    ## size of chunks to split the job into smaller ones
    chunk_size_for_predict <- 1000L
    
    ## indexes of beginning of each chunk (- 1) and of last position are being computed
    steps <- unique(c(seq(from = 0L, to = length(long_to_do), by = chunk_size_for_predict),
                      length(long_to_do)))
    
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
    
    ## we loop on each chunk
    for (i in 1:(length(steps) - 1)) {

      ## compute indexes for covariate values matching the current chunk
      within_steps <-  (steps[i] + 1L):steps[i + 1L] 
      
      ## select coordinates for prediction within chunk
      long <- long_to_do[within_steps]
      lat <- lat_to_do[within_steps]
      
      ## we build xs non-specifically using most complex model definition
      ## (it may look ugly but it should not increase much the computation time
      ## and it avoids a lot of uglier code)
      xs <- data.frame(long = long,
                       long_2 = long^2,
                       lat = lat,
                       lat_abs = abs(lat),
                       lat_2 = lat^2,
                       elev = raster::extract(raster, cbind(long, lat)),  ## ToDo: check that it is elev and not something else
                       source_ID = as.factor(paste("new", within_steps, sep = "_"))
      )
      
      ## predictions from disp_fit
      pred_dispfit <- spaMM::predict.HLfit(object = isofit$disp_fit,
                                            newdata = xs,
                                            variances = list(respVar = TRUE)
      )
      
      ## transmission of phi to mean_fit
      xs$pred_disp <- pred_dispfit[, 1]
      
      ## predictions from mean_fit
      pred_meanfit <- spaMM::predict.HLfit(object = isofit$mean_fit,
                                           newdata = xs,
                                           variances = list(respVar = TRUE)
      )
      
      ## we save the predictions
      mean_pred[within_steps] <- pred_meanfit[, 1]
      mean_predVar[within_steps]  <- attr(pred_meanfit, "predVar")
      mean_residVar[within_steps] <- attr(pred_meanfit, "residVar") ## same as disp_pred (as it should be)
      mean_respVar[within_steps]  <- attr(pred_meanfit, "respVar")
      
      disp_pred[within_steps] <- pred_dispfit[, 1]
      disp_predVar[within_steps]  <- attr(pred_dispfit, "predVar")  ## same as mean_residVar (as it should be)
      disp_residVar[within_steps] <- attr(pred_dispfit, "residVar")  
      disp_respVar[within_steps]  <- attr(pred_dispfit, "respVar")
      
      if (draw_pb) {
        utils::setTxtProgressBar(pb, steps[i + 1L]/length(lat_to_do)) ## update progress bar
      }
      
    }  ## we leave the loop on chunks
    
    ## the progress bar is being closed
    if (draw_pb) close(pb)
  })  ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("predictions for all", length(long_to_do),
                "locations have been computed in", time, "sec."), quote = FALSE)
  }
  
  ## we store the predictions for mean isotopic values into a raster
  save_raster <- function(x){
    .create_raster(long = long_to_do,
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
  source_points  <- .create_spatial_points(long = isofit$mean_fit$data$long,
                                           lat = isofit$mean_fit$data$lat,
                                           proj = "+proj=longlat +datum=WGS84"
  )
  
  ## we put all rasters in a stack
  isoscapes <- raster::stack(list("mean" = mean_raster,
                                 "mean_predVar" = mean_predVar_raster,
                                 "mean_residVar" = mean_residVar_raster,
                                 "mean_respVar" = mean_respVar_raster,
                                 "disp" = disp_raster,
                                 "disp_predVar" = disp_predVar_raster,
                                 "disp_residVar" = disp_residVar_raster,
                                 "disp_respVar" = disp_respVar_raster
  )
  )
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscapes = isoscapes,
              sp_points = list(sources = source_points)
  )
  
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
    
  if (any(class(isofit) %in% "MULTIISOFIT")) {
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
    coord <- sp::coordinates(raster)
    
    ## we create the object for newdata
    xs <- data.frame(long = coord[, 1],
                     long_2 = coord[, 1]^2,
                     lat = coord[, 2],
                     lat_abs = abs(coord[, 2]),
                     lat_2 = coord[, 2]^2,
                     elev = raster::extract(raster, coord), ## ToDo: check that it is elev
                     source_ID = as.factor(paste("new", 1:nrow(coord), sep = "_"))
    )

    rm(coord); gc()  ## remove coord as it can be a large object
  
    pred_dispfit <- spaMM::predict.HLfit(object = isofit$disp_fit,
                                         newdata = xs,
                                         variances = list(respVar = TRUE),
                                         blockSize = 16000L
    )
    
    ## transmission of phi to mean_fit
    xs$pred_disp <- pred_dispfit[, 1]
    
    ## predictions from mean_fit
    pred_meanfit <- spaMM::predict.HLfit(object = isofit$mean_fit,
                                         newdata = xs,
                                         variances = list(respVar = TRUE),
                                         blockSize = 16000L
    )
    
    mean_pred <- pred_meanfit[, 1]
    mean_predVar  <- attr(pred_meanfit, "predVar")
    mean_residVar <- attr(pred_meanfit, "residVar") ## same as disp_pred (as it should be)
    mean_respVar  <- attr(pred_meanfit, "respVar")
    
    disp_pred <- pred_dispfit[, 1]
    disp_predVar  <- attr(pred_dispfit, "predVar")  ## same as mean_residVar (as it should be)
    disp_residVar <- attr(pred_dispfit, "residVar")  
    disp_respVar  <- attr(pred_dispfit, "respVar")
  })  ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("predictions for all", nrow(xs),
                "locations have been computed in", time, "sec."), quote = FALSE)
  }
  
  ## we store the predictions for mean isotopic values into a raster
  save_raster <- function(x){
    .create_raster(long = xs$long,
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
  source_points  <- .create_spatial_points(long = isofit$mean_fit$data$long,
                                           lat = isofit$mean_fit$data$lat,
                                           proj = "+proj=longlat +datum=WGS84"
  )
  
  ## we put all rasters in a stack
  isoscapes <- raster::stack(list("mean" = mean_raster,
                                 "mean_predVar" = mean_predVar_raster,
                                 "mean_residVar" = mean_residVar_raster,
                                 "mean_respVar" = mean_respVar_raster,
                                 "disp" = disp_raster,
                                 "disp_predVar" = disp_predVar_raster,
                                 "disp_residVar" = disp_residVar_raster,
                                 "disp_respVar" = disp_respVar_raster
  )
  )
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscapes = isoscapes,
              sp_points = list(sources = source_points)
  )
  
  ## we define a new class
  class(out) <- c("ISOCAPE", "list")
  return(out)
}



#' Predicts the average spatial distribution of isotopic values over months,
#' years...
#' 
#' This function is the counterpart of \code{\link{isoscape}} for the objects
#' created with \code{\link{isomultifit}}. It creates the isoscapes for each
#' strata (e.g. month) defined by \code{split_by} during the call to
#' \code{\link{isomultifit}} and the aggregate them. The function can handle
#' weighting for the aggregation process and can thus be used to predict annual
#' averages precipitation weighted isoscapes.
#' 
#' @inheritParams isoscape
#' @param weighting An optional RasterBrick containing the weights
#' @return This function returns a \var{list} of class \var{isoscape}
#' containing a stack of all 8 raster layers mentioned above (all being of
#' class \var{RasterLayer}), and the location of the sources as spatial points.
#' @seealso
#' 
#' \code{\link{isoscape}} for details on the function used to compute the isoscapes for each strata

#' \code{\link{isomultifit}} for the function fitting the isoscape
#' 
#' \code{\link{plot.ISOSCAPE}} for the function plotting the isoscape model
#' 
#' \code{\link{IsoriX}} for the complete work-flow
#' 
#' @keywords models regression prediction predict
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 180) {
#' 
#' ## We prepare the data and split them by month:
#' 
#' GNIPDataDEmonthly <- prepsources(data = GNIPDataDE,
#'                                  split_by = "month")
#' 
#' dim(GNIPDataDEmonthly)
#' 
#' ## We fit the isoscapes:#' 
#' GermanMultiFit <- isomultifit(data = GNIPDataDEmonthly,
#'                               mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' ## We build the annual isoscapes by simple averaging (equal weighting):
#' GermanMultiscape <- isomultiscape(raster = ElevRasterDE,
#'                                   isofit = GermanMultiFit)
#' 
#' ## We build the annual isoscapes with a weighing based on precipitation amount:
#' GermanMultiscapeWeighted <- isomultiscape(raster = ElevRasterDE,
#'                                           isofit = GermanMultiFit,
#'                                           weighting = PrecipBrickDE)
#' 
#' ## We plot the mean isoscape of the averaging with equal weighting:
#' plot(x = GermanMultiscape, which = "mean")
#' 
#' ## We plot the mean isoscape of the averaging with precipitation weighting:
#' plot(x = GermanMultiscapeWeighted, which = "mean")
#' 
#' ## We build the isoscapes for a given month (here January):
#' GermanScapeJan <- isoscape(raster = ElevRasterDE,
#'                            isofit = GermanMultiFit$multi_fits[["month_1"]])
#'                          
#' ## We plot the mean isoscape for January:
#' plot(x = GermanScapeJan, which = "mean")
#' 
#' }
#' @export

isomultiscape <- function(raster, ## change as method?
                          isofit,
                          weighting = NULL,
                          verbose = interactive()
                          ) {
  
  ## In case the function is called on the output of isofit by mistake
  if (!any(class(isofit) %in% "MULTIISOFIT")) {
    return(isoscape(raster = raster,
                    isofit = isofit,
                    verbose = verbose
                    )
           )
  }
  
  ## Checking the inputs
  if (!is.null(weighting)) {
    if (!any(class(weighting) %in% c("RasterStack", "RasterBrick"))) {
      stop("the argument 'weighting' should be a RasterStack or a RasterBrick")
    }
    if (!all(names(isofit$multi.fits) %in% names(weighting))) {
      stop("the names of the layer in the object 'weighting' do not match those of your pairs of fits...")
    }
    if (raster::extent(weighting) != raster::extent(raster)) {
      stop("the extent of the object 'weighting' and 'raster' differ")
    }
    if (raster::ncell(weighting) != raster::ncell(raster)) {
      stop("the resolution of the object 'weighting' and 'raster' differ")
    }
  }
  
  isoscapes <- lapply(names(isofit$multi_fits),
                      function(fit) {
                         if (verbose) {
                           print(paste("#### building isoscapes for", fit, " in progress ####"), quote = FALSE)
                         }
                         iso <- isoscape(raster = raster,
                                         isofit = isofit$multi_fits[[fit]],
                                         verbose = verbose
                                         )
                         iso$sp_points$sources$values <- fit  ## set values for sp.points
                         return(iso)
                      }
                    )
  
  names(isoscapes) <- names(isofit$multi_fits)
  
  ## Combining mean isoscapes into RasterBricks
  brick_mean <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$mean))
  brick_mean_predVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$mean_predVar))
  brick_mean_residVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$mean_residVar))
  brick_mean_respVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$mean_respVar))

  ## Combining disp isoscapes into RasterBricks
  brick_disp <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$disp))
  brick_disp_predVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$disp_predVar))
  brick_disp_residVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$disp_residVar))
  brick_disp_respVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscapes$disp_respVar))
  
  ## Compute the weights
  if (is.null(weighting)) {
    weights <- raster::raster(raster)
    weights <- raster::setValues(weights, 1/length(isoscapes))
  } else {
    weights <- weighting / sum(weighting)
  }
  
  ## Compute the weighted averages and store then in a list of RasterBricks
  multiscape <- raster::brick(list("mean" = sum(brick_mean * weights),
                                 "mean_predVar" = sum(brick_mean_predVar * weights^2),
                                 "mean_residVar" = sum(brick_mean_residVar * weights^2),
                                 "mean_respVar" = sum(brick_mean_respVar * weights^2),
                                 "disp" = sum(brick_disp * weights),
                                 "disp_predVar" = sum(brick_disp_predVar * weights^2),
                                 "disp_residVar" = sum(brick_disp_residVar * weights^2),
                                 "disp_respVar" = sum(brick_disp_respVar * weights^2)
                                )
                            )
  
  ## Agglomerate the sources spatial points
  source_points <- Reduce("+", lapply(isoscapes, function(iso) iso$sp_points$sources))
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscapes = multiscape,
              sp_points = list(sources = source_points)
              )
  
  ## we define a new class
  class(out) <- c("ISOSCAPE", "list")
  
  return(out)
  }


print.ISOSCAPE <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}


summary.ISOSCAPE <- function(object, ...) {
  if ("ISOSIM" %in% class(object)) {
    cat("\n")
    cat("### Note: this isoscape has been simulated ###", "\n")
    cat("\n")
  }
  cat("### stack containing the isoscape")
  print(object[[1]])
  cat("\n")
  if (length(object) > 1) {
    cat("### first 5 locations of the dataset")
    print(utils::head(object[[2]][[1]], 5L))
  }
  return(invisible(NULL))
}

