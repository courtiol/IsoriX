#' @rdname IsoriX-defunct
#' @export
Isoscape <- function(...) {
  .Defunct("isoscape")
}


#' Predicts the spatial distribution of isotopic values
#' 
#' This function produces the isoscape, i.e. a spatial prediction (i.e. map) of
#' the distribution of isotopic delta values. Predictions are computed using
#' the fitted isoscape for each raster cell of the elevation raster. All shape
#' files can be exported and loaded into any Geographic Information System
#' (GIS) if needed.
#' 
#' This function computes the predictions (\code{mean}), prediction variances
#' (\code{mean.predVar}), residual variances (\code{mean.residVar}) and
#' response variances (\code{mean.respVar}) for the isotopic values at a
#' resolution equal to the one of the elevation raster. It also computes the
#' same information for the residual dispersion variance (\code{disp.pred},
#' \code{disp.predVar}, \code{disp.residVar}, or \code{disp.respVar}).
#' 
#' The predictions of isotopic values across the landscape are performed by
#' calling the function \code{\link[spaMM]{predict}} from the package
#' \pkg{\link[spaMM]{spaMM}} on the fitted isoscape produced by \code{\link{isofit}}.
#' 
#' Let us detail the meaning of \code{mean}, \code{mean.predVar} and
#' \code{mean.respVar}:
#' 
#' Our model assumes that that there is a single true unknown isoscape, which
#' is fixed but which is represented by the mixed-effect model as a random draw
#' from possible realizations of isoscapes (random draws of the
#' Matern-correlated process and of the uncorrelated random effects if
#' considered). We infer this realized isoscape by fitting the model to a
#' limited amount of data, with some uncertainty since different random draws
#' of the unknown isoscape may give the same observed data. There is thus a
#' conditional distribution of possible true isoscapes given the data. For
#' linear mixed-effects models, the mean prediction, technically called the
#' best linear unbiased prediction (BLUP), is the mean of this conditional
#' distribution. The prediction variance is ideally the mean square difference
#' between the true unknown value of the linear predictor and the BLUP at a
#' given location. The response variance has a different meaning. It estimates
#' the variance of new observations drawn from the true unknown isoscape at a
#' given location. The response variance is simply equal to the sum of the
#' prediction variance and the residual variance (note that the residual
#' variance considered assume that a single observation is being observed per
#' location).
#' 
#' The isoscape can be plotted using the function \code{\link{plot.isoscape}}
#' (see examples).
#' 
#' @aliases isoscape print.isoscape summary.isoscape
#' @param elevation.raster The elevation raster (\var{RasterLayer}) created by
#' \code{\link{relevate}}
#' @param isofit The fitted isoscape created by \code{\link{isofit}}
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session and \var{FALSE} otherwise.
#' @return This function returns a \var{list} of class \var{isoscape}
#' containing a stack of all 8 raster layers mentioned above (all being of
#' class \var{RasterLayer}), and the location of the sources as spatial points.
#' @seealso \code{\link{isofit}} for the function fitting the isoscape
#' 
#' \code{\link{plot.isoscape}} for the function plotting the isoscape model
#' 
#' \code{\link{IsoriX}} for the complete work-flow
#' @keywords models regression prediction predict
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. IsoriX.options(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(IsoriX.getOption("example_maxtime") > 30) {
#' 
#' ## We prepare the data
#' GNIPDataDEagg <- prepdata(data = GNIPDataDE)
#' 
#' ## We fit the models
#' GermanFit <- isofit(iso.data = GNIPDataDEagg,
#'                     mean.model.fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' ## We build the isoscapes
#' isoscape <- isoscape(elevation.raster = ElevRasterDE,
#'                      isofit = GermanFit)
#' 
#' isoscape
#' 
#' ## We build the plots
#' plot.mean <- plot(x = isoscape, which = "mean", plot = FALSE)
#' 
#' plot.mean.predVar <- plot(x = isoscape, which = "mean.predVar", plot = FALSE)
#' 
#' plot.mean.residVar <- plot(x = isoscape, which = "mean.residVar", plot = FALSE)
#' 
#' plot.mean.respVar <- plot(x = isoscape, which = "mean.respVar", plot = FALSE)
#' 
#' ## We display the plots
#' if(require(lattice)) {
#'  print(plot.mean, split = c(1, 1, 2, 2), more = TRUE)
#'  print(plot.mean.predVar,   split = c(2, 1, 2, 2), more = TRUE)
#'  print(plot.mean.residVar,  split = c(1, 2, 2, 2), more = TRUE)
#'  print(plot.mean.respVar,   split = c(2, 2, 2, 2), more = FALSE)
#'  
#' ## We build a sphere with our isoscape
#' plot(x = isoscape, which = "mean", plot = FALSE, sphere = list(build = TRUE))
#'  
#' ## We can save a rotating sphere with the isoscape as a .gif-file.
#' ## This file will be located inside your working directory.
#' ## Make sure your current rgl device (from before) is still open
#' ## and that you have both the packages 'rgl' and 'magick' installed.
#' ## The building of the .gif implies to create temporarily many .png
#' ## but those will be removed automatically once the .gif is done.
#' if(require("rgl") & require("magick")) {
#'   movie3d(spin3d(axis = c(0, 0, 1), rpm=2), duration = 30, dir = getwd())
#' }
#' }
#' 
#' }
#' 
#' @export

isoscape <- function(elevation.raster, ## change as method?
                     isofit,
                     verbose = interactive()) {
  
  if (any(class(isofit) %in% "multiisofit")) {
    stop("object 'isofit' of class multiisofit; use isomultiscape instead.")
  }
  
  if (verbose) {
    print("Building the isoscapes... ", quote = FALSE)
    print("(this may take a while)", quote = FALSE)
  }
  
  if (isofit$mean.fit$spaMM.version != utils::packageVersion(pkg = "spaMM")) {
    warning("The isofit has been fitted on a different version of spaMM than the one called by IsoriX. This may create troubles in paradize...")
  }
  
  time <- system.time({
    
    ## we extract lat/long from all cells of the elevation raster
    coord <- sp::coordinates(elevation.raster)
    long.to.do <- coord[, 1]  # extract the longitude
    lat.to.do <-  coord[, 2]  # extract the lattitude
    rm(coord); gc()  ## remove coord as it can be a large object
    
    ## size of chunks to split the job into smaller ones
    chunk.size.for.predict <- 1000L
    
    ## indexes of beginning of each chunk (- 1) and of last position are being computed
    steps <- unique(c(seq(from = 0L, to = length(long.to.do), by = chunk.size.for.predict),
                      length(long.to.do)))
    
    ## a logical indicating if a progression bar must be used
    draw.pb <- interactive() & (length(steps) - 1) > 2
    
    ## create empty vectors to store predictions
    mean.pred <- disp.pred <- rep(NA, length(long.to.do))
    mean.predVar <- mean.residVar <- mean.respVar <- mean.pred
    disp.predVar <- disp.residVar <- disp.respVar <- disp.pred
    
    ## initiate the progress bar
    if (draw.pb) {
      pb <- utils::txtProgressBar(style = 3)
    }
    
    ## we loop on each chunk
    for (i in 1:(length(steps) - 1)) {

      ## compute indexes for covariate values matching the current chunk
      within.steps <-  (steps[i] + 1L):steps[i + 1L] 
      
      ## select coordinates for prediction within chunk
      long <- long.to.do[within.steps]
      lat <- lat.to.do[within.steps]
      
      ## we build xs non-specifically using most complex model definition
      ## (it may look ugly but it should not increase much the computation time
      ## and it avoids a lot of uglier code)
      xs <- data.frame(long = long,
                       long.2 = long^2,
                       lat = lat,
                       lat.abs = abs(lat),
                       lat.2 = lat^2,
                       elev = raster::extract(elevation.raster, cbind(long, lat)),
                       stationID = as.factor(paste("new", within.steps, sep = "_"))
      )
      
      ## predictions from disp.fit
      pred.disp.fit <- spaMM::predict.HLfit(object = isofit$disp.fit,
                                            newdata = xs,
                                            variances = list(respVar = TRUE)
      )
      
      ## transmission of phi to mean.fit
      xs$pred.disp <- pred.disp.fit[, 1]
      
      ## predictions from mean.fit
      pred.mean.fit <- spaMM::predict.HLfit(object = isofit$mean.fit,
                                            newdata = xs,
                                            variances = list(respVar = TRUE)
      )
      
      ## we save the predictions
      mean.pred[within.steps] <- pred.mean.fit[, 1]
      mean.predVar[within.steps]  <- attr(pred.mean.fit, "predVar")
      mean.residVar[within.steps] <- attr(pred.mean.fit, "residVar") ## same as disp.pred (as it should be)
      mean.respVar[within.steps]  <- attr(pred.mean.fit, "respVar")
      
      disp.pred[within.steps] <- pred.disp.fit[, 1]
      disp.predVar[within.steps]  <- attr(pred.disp.fit, "predVar")  ## same as mean.residVar (as it should be)
      disp.residVar[within.steps] <- attr(pred.disp.fit, "residVar")  
      disp.respVar[within.steps]  <- attr(pred.disp.fit, "respVar")
      
      if (draw.pb) {
        utils::setTxtProgressBar(pb, steps[i + 1L]/length(lat.to.do)) ## update progress bar
      }
      
    }  ## we leave the loop on chunks
    
    ## the progress bar is being closed
    if (draw.pb) close(pb)
  })  ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("predictions for all", length(long.to.do),
                "locations have been computed in", time, "sec."), quote = FALSE)
  }
  
  ## we store the predictions for mean isotopic values into a raster
  SaveRaster <- function(x){
    .CreateRaster(long = long.to.do,
                  lat = lat.to.do,
                  values = x,
                  proj = "+proj=longlat +datum=WGS84"
    )
  }
  
  mean.raster <- SaveRaster(mean.pred)
  mean.predVar.raster <- SaveRaster(mean.predVar)
  mean.residVar.raster <- SaveRaster(mean.residVar)
  mean.respVar.raster <- SaveRaster(mean.respVar)
  
  disp.raster <- SaveRaster(disp.pred)
  disp.predVar.raster <- SaveRaster(disp.predVar)
  disp.residVar.raster <- SaveRaster(disp.residVar)
  disp.respVar.raster <- SaveRaster(disp.respVar)
  
  
  ## we create the spatial points for sources
  source.points  <- .CreateSpatialPoints(long = isofit$mean.fit$data$long,
                                         lat = isofit$mean.fit$data$lat,
                                         proj = "+proj=longlat +datum=WGS84"
  )
  
  ## we put all rasters in a stack
  isoscape <- raster::stack(list("mean" = mean.raster,
                                 "mean.predVar" = mean.predVar.raster,
                                 "mean.residVar" = mean.residVar.raster,
                                 "mean.respVar" = mean.respVar.raster,
                                 "disp" = disp.raster,
                                 "disp.predVar" = disp.predVar.raster,
                                 "disp.residVar" = disp.residVar.raster,
                                 "disp.respVar" = disp.respVar.raster
  )
  )
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscape = isoscape,
              sp.points = list(sources = source.points)
  )
  
  ## we define a new class
  class(out) <- c("isoscape", "isofit", "list")
  
  return(out)
}

.futureisoscape <- function(elevation.raster, ## change as method?
                           isofit,
                           verbose = interactive()) {
  # Predicts the spatial distribution of isotopic values
  # 
  # This function is an alternative implementation of isoscape().
  # It is not exported but may be put in use in a future version of IsoriX.
  # It does not compute the predictions into chunks.
    
  if (any(class(isofit) %in% "multiisofit")) {
    stop("object 'isofit' of class multiisofit; use isomultiscape instead.")
  }
  
  if (verbose) {
    print("Building the isoscapes... ", quote = FALSE)
    print("(this may take a while)", quote = FALSE)
  }
  
  if (isofit$mean.fit$spaMM.version != utils::packageVersion(pkg = "spaMM")) {
    warning("The isofit has been fitted on a different version of spaMM than the one called by IsoriX. This may create troubles in paradize...")
  }
  
  time <- system.time({
    
    ## we extract lat/long from all cells of the elevation raster
    coord <- sp::coordinates(elevation.raster)
    
    ## we create the object for newdata
    xs <- data.frame(long = coord[, 1],
                     long.2 = coord[, 1]^2,
                     lat = coord[, 2],
                     lat.abs = abs(coord[, 2]),
                     lat.2 = coord[, 2]^2,
                     elev = raster::extract(elevation.raster, coord),
                     stationID = as.factor(paste("new", 1:nrow(coord), sep = "_"))
    )

    rm(coord); gc()  ## remove coord as it can be a large object
  
    pred.disp.fit <- spaMM::predict.HLfit(object = isofit$disp.fit,
                                          newdata = xs,
                                          variances = list(respVar = TRUE),
                                          blockSize = 16000L
    )
    
    ## transmission of phi to mean.fit
    xs$pred.disp <- pred.disp.fit[, 1]
    
    ## predictions from mean.fit
    pred.mean.fit <- spaMM::predict.HLfit(object = isofit$mean.fit,
                                          newdata = xs,
                                          variances = list(respVar = TRUE),
                                          blockSize = 16000L
    )
    
    mean.pred <- pred.mean.fit[, 1]
    mean.predVar  <- attr(pred.mean.fit, "predVar")
    mean.residVar <- attr(pred.mean.fit, "residVar") ## same as disp.pred (as it should be)
    mean.respVar  <- attr(pred.mean.fit, "respVar")
    
    disp.pred <- pred.disp.fit[, 1]
    disp.predVar  <- attr(pred.disp.fit, "predVar")  ## same as mean.residVar (as it should be)
    disp.residVar <- attr(pred.disp.fit, "residVar")  
    disp.respVar  <- attr(pred.disp.fit, "respVar")
  })  ## end of system.time
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("predictions for all", nrow(xs),
                "locations have been computed in", time, "sec."), quote = FALSE)
  }
  
  ## we store the predictions for mean isotopic values into a raster
  SaveRaster <- function(x){
    .CreateRaster(long = xs$long,
                  lat = xs$lat,
                  values = x,
                  proj = "+proj=longlat +datum=WGS84"
    )
  }
  
  mean.raster <- SaveRaster(mean.pred)
  mean.predVar.raster <- SaveRaster(mean.predVar)
  mean.residVar.raster <- SaveRaster(mean.residVar)
  mean.respVar.raster <- SaveRaster(mean.respVar)
  
  disp.raster <- SaveRaster(disp.pred)
  disp.predVar.raster <- SaveRaster(disp.predVar)
  disp.residVar.raster <- SaveRaster(disp.residVar)
  disp.respVar.raster <- SaveRaster(disp.respVar)
  
  
  ## we create the spatial points for sources
  source.points  <- .CreateSpatialPoints(long = isofit$mean.fit$data$long,
                                         lat = isofit$mean.fit$data$lat,
                                         proj = "+proj=longlat +datum=WGS84"
  )
  
  ## we put all rasters in a stack
  isoscape <- raster::stack(list("mean" = mean.raster,
                                 "mean.predVar" = mean.predVar.raster,
                                 "mean.residVar" = mean.residVar.raster,
                                 "mean.respVar" = mean.respVar.raster,
                                 "disp" = disp.raster,
                                 "disp.predVar" = disp.predVar.raster,
                                 "disp.residVar" = disp.residVar.raster,
                                 "disp.respVar" = disp.respVar.raster
  )
  )
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscape = isoscape,
              sp.points = list(sources = source.points)
  )
  
  ## we define a new class
  class(out) <- c("isoscape", "isofit", "list")
  return(out)
}



#' Predicts the average spatial distribution of isotopic values over months,
#' years...
#' 
#' This function is the counterpart of \code{\link{isoscape}} for the objects
#' created with \code{\link{isomultifit}}. It creates the isoscapes for each
#' strata (e.g. month) defined by \code{split.by} during the call to
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
#' \code{\link{plot.isoscape}} for the function plotting the isoscape model
#' 
#' \code{\link{plot.isoscape}} for the function plotting the isoscape model
#' 
#' \code{\link{IsoriX}} for the complete work-flow
#' 
#' @keywords models regression prediction predict
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. IsoriX.options(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(IsoriX.getOption("example_maxtime") > 180) {
#' 
#' ## We prepare the data and split them by month:
#' 
#' GNIPDataDEmonthly <- prepdata(data = GNIPDataDE,
#'                               split.by = "month")
#' 
#' dim(GNIPDataDEmonthly)
#' 
#' ## We fit the isoscapes:#' 
#' isoscapemodels <- isomultifit(iso.data = GNIPDataDEmonthly,
#'                               mean.model.fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' ## We build the annual isoscapes by simple averaging (equal weighting):
#' isoscapes <- isomultiscape(elevation.raster = ElevRasterDE,
#'                            isofit = isoscapemodels)
#' 
#' ## We build the annual isoscapes with a weighing based on precipitation amount:
#' isoscapes.weighted <- isomultiscape(elevation.raster = ElevRasterDE,
#'                            isofit = isoscapemodels,
#'                            weighting = PrecipBrickDE)
#' 
#' ## We plot the mean isoscape of the averaging with equal weighting:
#' plot(x = isoscapes, which = "mean")
#' 
#' ## We plot the mean isoscape of the averaging with precipitation weighting:
#' plot(x = isoscapes.weighted, which = "mean")
#' 
#' ## We build the isoscapes for a given month (here January):
#' isoscape.jan <- isoscape(elevation.raster = ElevRasterDE,
#'                          isofit = isoscapemodels$multi.fits[["month_1"]])
#'                          
#' ## We plot the mean isoscape for January:
#' plot(x = isoscape.jan, which = "mean")
#' 
#' }
#' @export

isomultiscape <- function(elevation.raster, ## change as method?
                          isofit,
                          weighting = NULL,
                          verbose = interactive()
                          ) {
  
  ## In case the function is called on the output of isofit by mistake
  if (!any(class(isofit) %in% "multiisofit")) {
    return(isoscape(elevation.raster = elevation.raster,
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
    if (raster::extent(weighting) != raster::extent(elevation.raster)) {
      stop("the extent of the object 'weighting' and 'elevation.raster' differ")
    }
    if (raster::ncell(weighting) != raster::ncell(elevation.raster)) {
      stop("the resolution of the object 'weighting' and 'elevation.raster' differ")
    }
  }
  
  isoscapes <- lapply(names(isofit$multi.fits),
                      function(fit) {
                         if (verbose) {
                           print(paste("#### building isoscapes for", fit, " in progress ####"), quote = FALSE)
                         }
                         iso <- isoscape(elevation.raster = elevation.raster,
                                         isofit = isofit$multi.fits[[fit]],
                                         verbose = verbose
                                         )
                         iso$sp.points$sources$values <- fit  ## set values for sp.points
                         return(iso)
                      }
                    )
  
  names(isoscapes) <- names(isofit$multi.fits)
  
  ## Combining mean isoscapes into RasterBricks
  brick.mean <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$mean))
  brick.mean.predVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$mean.predVar))
  brick.mean.residVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$mean.residVar))
  brick.mean.respVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$mean.respVar))

  ## Combining disp isoscapes into RasterBricks
  brick.disp <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$disp))
  brick.disp.predVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$disp.predVar))
  brick.disp.residVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$disp.residVar))
  brick.disp.respVar <- raster::brick(lapply(isoscapes, function(iso) iso$isoscape$disp.respVar))
  
  ## Compute the weights
  if (is.null(weighting)) {
    weights <- raster::raster(elevation.raster)
    weights <- raster::setValues(weights, 1/length(isoscapes))
  } else {
    weights <- weighting / sum(weighting)
  }
  
  ## Compute the weighted averages and store then in a list of RasterBricks
  isoscape <- raster::brick(list("mean" = sum(brick.mean * weights),
                                 "mean.predVar" = sum(brick.mean.predVar * weights^2),
                                 "mean.residVar" = sum(brick.mean.residVar * weights^2),
                                 "mean.respVar" = sum(brick.mean.respVar * weights^2),
                                 "disp" = sum(brick.disp * weights),
                                 "disp.predVar" = sum(brick.disp.predVar * weights^2),
                                 "disp.residVar" = sum(brick.disp.residVar * weights^2),
                                 "disp.respVar" = sum(brick.disp.respVar * weights^2)
                                )
                            )
  
  ## Agglomerate the sources spatial points
  source.points <- Reduce("+", lapply(isoscapes, function(iso) iso$sp.points$sources))
  
  ## we put the stack in a list that also contains
  ## the spatial points for the sources
  out <- list(isoscape = isoscape,
              sp.points = list(sources = source.points)
              )
  
  ## we define a new class
  class(out) <- c("isoscape", "isofit", "list")
  
  return(out)
  }


print.isoscape <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}


summary.isoscape <- function(object, ...) {
  if ("isosim" %in% class(object)) {
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

