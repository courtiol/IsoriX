#' Deprecated functions
#' 
#' The function you asked help for has been deprecated (i.e. it does not longer
#' exists). A new function with a different name is surely doing the old job.
#' 
#' @param ... The call of the deprecated function
#' @aliases Calibfit GetElev Isofit Isorix Isoscape Isosim QueryGNIP RElevate
#' @name IsoriX-defunct
NULL

#' @rdname IsoriX-defunct
#' @export
Isorix <- function(...) {
  .Defunct("isofind")
}



#' Infer spatial origins
#' 
#' This function performs the assignment of organims of unknown origins.
#' 
#' An assignment is a comparison, for a given organism, of the predicted
#' isotopic value at its location of origin and the predicted isotopic value at
#' each location of the \code{isoscape}. The difference between these two
#' values constitute the statistic of the assingment test. Under the null
#' hypothesis (when the organism is at a location with the same isotopic value
#' than its original location), the test statistics follows a normal
#' distribution with mean zero and a certain variance that stems from both the
#' isoscape model fits and the calibration fit. The function \code{\link{isofind}}
#' computes the map of p-value for such an assignment test (i.e. the p-values in
#' all locations of the isoscape) for all individuals in the dataframe
#' \code{assign.data}. The function also performs a single assignment for the
#' entire group by combining the individual p-value maps using the Fisher's
#' method (Fisher 1925).
#' 
#' A mask can be used so to remove all values falling in the mask. This can be
#' useful for performing for example assignments on lands only and discard
#' anything falling in large bodies of water (see example). By default our
#' \code{\link{OceanMask}} is considered. Setting \code{mask} to NULL allows 
#' to prevent this automatic behaviour.
#' 
#' @aliases isofind print.isorix summary.isorix
#' @param assign.data A \var{dataframe} containing the assignment data (see
#' note below)
#' @param isoscape The output of the function \code{\link{isoscape}}
#' @param calibfit The output of the function \code{\link{calibfit}} (This 
#' argument is not needed if the isoscape had been fitted using isotopic 
#' ratios from sedentary animals.)
#' @param mask A \var{SpatialPolygons} of a mask to replace values on all
#' rasters by NA inside polygons (see details)
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session and \var{FALSE} otherwise.
#' @return This function returns a \var{list} of class \var{isorix} containing
#' itself three lists (\code{indiv}, \code{group}, and \code{sp.points})
#' storing all rasters built during assignment and the spatial points for
#' sources and calibration. The \var{list} \code{indiv} contains three stack of
#' raster layers: one storing the value of the test statistic ("stat"), one
#' storing the value of the variance of the test statistic ("var") and one
#' storing the p-value of the test ("pv"). The \var{list} \code{group} contains
#' one raster storing the p-values of the assignment for the group. The
#' \var{list} \code{sp.points} contains two spatial point objects:
#' \code{source.points} and \code{calib.points}.
#' @note See \code{\link{AssignDataAlien}} to know which variables are needed to
#' perform the assignment and their names.
#' @seealso \code{\link{IsoriX}} for the complete workflow
#' @references Fisher, R.A. (1925). Statistical Methods for Research Workers.
#' Oliver and Boyd (Edinburgh). ISBN 0-05-002170-2.
#' @keywords models regression
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. IsoriX.options(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(IsoriX.getOption("example_maxtime") > 200) {
#' 
#' ## We fit the models for Germany
#' GNIPDataDEagg <- queryGNIP(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(iso.data = GNIPDataDEagg)
#' 
#' 
#' ## We build the isoscape
#' isoscape <- isoscape(elevation.raster = ElevRasterDE,
#'                      isofit = GermanFit)
#' 
#' 
#' ## We fit the calibration model
#' calib <- calibfit(calib.data = CalibDataAlien,
#'                   isofit = GermanFit)
#' 
#' ## We perform the assignment on land only
#' assignment.dry <- isofind(assign.data = AssignDataAlien,
#'                           isoscape = isoscape,
#'                           calibfit = calib)
#' 
#' ## perform the assignment on land and water
#' assignment <- isofind(assign.data = AssignDataAlien,
#'                       isoscape = isoscape,
#'                       calibfit = calib,
#'                       mask = NULL)
#' 
#' ## We plot the group assignment
#' plot(assignment, who = "group", mask = list(mask = NULL))
#' 
#' plot(assignment.dry, who = "group", mask = list(mask = NULL))
#' 
#' ## We plot the assignment for the 8 first individuals
#' plot(assignment.dry, who = 1:8,
#'      sources = list(draw = FALSE),
#'      calib = list(draw = FALSE))
#' 
#' ## We plot the assignment for the individual "Alien_10"
#' plot(assignment.dry, who = "Alien_10")
#' 
#' 
#' ### Other example without calibration:
#' ### We will try to assign a weather station 
#' ### in the water isoscape
#' 
#' ## We create the assignment data taking 
#' ## GARMISCH-PARTENKIRCHEN as the station to assign
#' GPIso <- GNIPDataDEagg[GNIPDataDEagg$stationID == "GARMISCH-PARTENKIRCHEN", "isoscape.value"]
#' AssignDataGP <- data.frame(tissue.value = GPIso,
#'                              animalID = "GARMISCH-PARTENKIRCHEN")
#' 
#' ## We perform the assignment
#' assignment.GP <- isofind(assign.data = AssignDataGP,
#'                                  isoscape = isoscape,
#'                                  calibfit = NULL)
#' ## We plot the assignment and 
#' ## show where the station really is (using lattice)
#' plot(assignment.GP) +
#'   lattice::xyplot(47.48~11.06,
#'                   panel = lattice::panel.points,
#'                   cex = 5, pch = 13, lwd = 2, col = "black") 
#' 
#' 
#' }
#' 
#' @export
isofind <- function(assign.data,
                    isoscape,
                    calibfit = NULL,
                    mask = NA,
                    verbose = interactive()
                    ) {

  ### WE COMPUTE THE TEST STATISTIC
  if (verbose) {
    print("computing the test statistic and its variance...")
  }
  
  ### check for calibration data
  if (is.null(calibfit)) {
    warning(
"The assignment is computed directly on the isoscape
without using a calibration! This means that IsoriX 
considers that you directly fitted the isoscape on 
the same material than the material you are trying
to assign. If this is not the case, rerun isofind()
by providing a calibration object to the argument
calibfit!")
  }

  ## importing ocean if missing
  if (!is.null(mask) && class(mask) != "SpatialPolygons" && is.na(mask)) {
    OceanMask <- NULL
    utils::data("OceanMask", envir = environment(), package = "IsoriX")
    mask <- OceanMask
  }
  
  names.layers <- gsub(" ", "_", as.character(assign.data$animalID))
  
  time <- system.time({
    if (!is.null(calibfit)) {
      ## we predict the isotopic value at origin location  
      assign.data$mean.origin <-
        (assign.data$tissue.value - calibfit$param["intercept"])/calibfit$param["slope"]
      ## we create individual rasters containing the test statistics
      list.stat.layers <- sapply(1:nrow(assign.data),
                                 function(i) {
                                   assign.data$mean.origin[i] - isoscape$isoscape$mean
                                 }
                                 )
    } else {
      ## we create individual rasters containing the test statistics
      list.stat.layers <- sapply(1:nrow(assign.data),
                                 function(i) {
                                   assign.data$tissue.value[i] - isoscape$isoscape$mean
                                 }
                                 )
    }
    
    names(list.stat.layers) <- names.layers
    stat.stack <- raster::stack(list.stat.layers)
    if (any(names.layers != names(stat.stack))) {
      warning("Your animalID could not be used to name rasters (you may have used numbers), so they have been slightly modified by IsoriX.")
      names.layers <- names(stat.stack) ## trick to track the good names as they can change during stacking (if numbers)
    }
    
    ### WE COMPUTE THE VARIANCE OF THE TEST

    if (!is.null(calibfit)) {
      ## we compute fixedVar
      X <- cbind(1, assign.data$mean.origin)
      fixedVar <- rowSums(X * (X %*% calibfit$fixefCov)) ## = diag(X %*% calibfit$fixefCov %*% t(X))
      ## we create individual rasters containing the variance of the test statistics
      list.varstat.layers <- sapply(1:nrow(assign.data),
                                    function(i) {
                                      isoscape$isoscape$mean.predVar + 
                                      calibfit$calib.fit$phi/calibfit$param["slope"]^2 +
                                      fixedVar[i]/calibfit$param["slope"]^2 +
                                      0 ## ToDo compute fourth variance term
                                    }
                                    )
    } else {
      ## we create individual rasters containing the variance of the test statistics
      list.varstat.layers <- sapply(1:nrow(assign.data),
                                    function(i) {
                                      isoscape$isoscape$mean.respVar
                                    }
                                    )
    }

    names(list.varstat.layers) <- names.layers
    varstat.stack <- raster::stack(list.varstat.layers)

    ### WE COMPUTE THE INDIVIDUAL LOG P-VALUE SURFACES
    if (verbose) {
      print("running the assignment test...")
    }

    ## we initialize the stack
    logpv.stack <- raster::raster(varstat.stack)

    ## we create individual rasters containing the p-values of the test
    for (animalID in names.layers) {
      name.layer <- paste("logpv.stack$", animalID, sep = "")
      expr.to.run <- paste(name.layer,
                           "<- .AssignTest(raster::values(stat.stack[[animalID]]), raster::values(varstat.stack[[animalID]]))"
                           )
      eval(parse(text = expr.to.run))
    }

    ### WE COMBINE INDIVIDUAL SURFACES USING FISHER'S METHOD
    if (verbose) {
      print("combining individual assignments...")
    }

    group.pv <- raster::calc(logpv.stack, .FisherMethod)
  })  ## end of system.time

  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("assignements for all", nrow(assign.data), "organisms have been computed in", time, "sec."))
  }

  ## remove log scale
  pv.stack <- exp(logpv.stack)
  names(pv.stack) <- names.layers  ## we restore the names as they are not kept when computing
  rm(logpv.stack)

  ## replacing values by zeros if they fall in the mask (e.g. in water)
  if (!is.null(mask)) {
    if (verbose) {
      print("applying the mask...")
    }

    ## turn mask into raster with NA inside polygons
    raster.mask <- is.na(raster::rasterize(mask, stat.stack))

    ## multiplying rasters by the raster.mask    
    stat.stack <- stat.stack*raster.mask
    names(stat.stack) <- names.layers ## we restore the names as they are not kept when computing

    varstat.stack <- varstat.stack*raster.mask
    names(varstat.stack) <- names.layers ## we restore the names as they are not kept when computing

    pv.stack <- pv.stack*raster.mask
    names(pv.stack) <- names.layers ## we restore the names as they are not kept when computing
    
    group.pv <- raster::overlay(group.pv, raster.mask, fun = prod)
  }


  ### RETURNS
  
  calibs <- NULL
  if (!is.null(calibfit)) {
    calibs <- calibfit$sp.points$calibs
  }

  out <- list(indiv = list("stat" = stat.stack,
                           "stat.var" = varstat.stack,
                           "pv" = pv.stack
                           ),
              group = list("pv" = group.pv),
              sp.points = list("sources" = isoscape$sp.points$sources,
                               "calibs" = calibs
                               )
              )

  class(out) <- c("isorix", "list")
  return(out)
}


.AssignTest <- function(stats, vars, log.scale = TRUE) {
  pv <- 2*(1 - stats::pnorm(abs(stats), mean = 0, sd = sqrt(vars)))
  if (log.scale) {
    pv <- log(pv)
  }
  return(pv)
}


.FisherMethod <- function(logpv) {
  fisher.stat <- -2*sum(logpv)
  df <- 2*length(logpv)
  pv <- stats::pchisq(q = fisher.stat, df = df, lower.tail = FALSE)
  return(pv)
}

#' @export
#' @method print isorix
print.isorix <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}

#' @export
#' @method summary isorix
summary.isorix <- function(object, ...) {
  for (i in names(object)[names(object) != "sp.points"]) {
    cat(paste("######### assignment raster(s): '", i, "'"), "\n")
    print(object[[i]])
    cat("\n")
  } 
  return(invisible(NULL))
}

