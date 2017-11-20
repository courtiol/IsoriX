#' Infer spatial origins
#' 
#' This function performs the assignment of samples of unknown origins.
#' 
#' An assignment is a comparison, for a given organism, of the predicted
#' isotopic source value at its location of origin and the predicted isotopic
#' source value at each location of the \code{isoscape}. The difference between
#' these two values constitute the statistic of the assingment test. Under the
#' null hypothesis (the organism is at a location with the same isotopic value
#' than its original location), the test statistics follows a normal
#' distribution with mean zero and a certain variance that stems from both the
#' isoscape model fits and the calibration fit. The function
#' \code{\link{isofind}} computes the map of p-value for such an assignment test
#' (i.e. the p-values in all locations of the isoscape) for all samples in the
#' dataframe \code{data}. The function also performs a single assignment for the
#' entire group by combining the p-value maps of all samples using the Fisher's
#' method (Fisher 1925). Significant p-values are strong evidence that the
#' sample do NOT come from the candidate location (and not the opposite!).
#' 
#' A mask can be used so to remove all values falling in the mask. This can be
#' useful for performing for example assignments on lands only and discard
#' anything falling in large bodies of water (see example). By default our
#' \code{\link{OceanMask}} is considered. Setting \code{mask} to NULL allows 
#' to prevent this automatic behaviour.
#' 
#' @aliases isofind print.ISOFIND summary.ISOFIND
#' @param data A \var{dataframe} containing the assignment data (see
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
#' @return This function returns a \var{list} of class \var{ISOFIND} containing
#' itself three lists (\code{sample}, \code{group}, and \code{sp_points})
#' storing all rasters built during assignment and the spatial points for
#' sources and calibration. The \var{list} \code{sample} contains three set of
#' raster layers: one storing the value of the test statistic ("stat"), one
#' storing the value of the variance of the test statistic ("var") and one
#' storing the p-value of the test ("pv"). The \var{list} \code{group} contains
#' one raster storing the p-values of the assignment for the group. The
#' \var{list} \code{sp_points} contains two spatial point objects:
#' \code{sources} and \code{calibs}.
#' @note See \code{\link{AssignDataAlien}} to know which variables are needed to
#' perform the assignment and their names.
#' @seealso \code{\link{IsoriX}} for the complete workflow
#' @references Fisher, R.A. (1925). Statistical Methods for Research Workers.
#' Oliver and Boyd (Edinburgh). ISBN 0-05-002170-2.
#' @keywords models regression
#' @examples
#' 
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#' 
#' if(getOption_IsoriX("example_maxtime") > 200) {
#' 
#' ## We fit the models for Germany
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' 
#' ## We build the isoscape
#' GermanScape <- isoscape(raster = ElevRasterDE,
#'                         isofit = GermanFit)
#' 
#' 
#' ## We fit the calibration model
#' CalibAlien <- calibfit(data = CalibDataAlien,
#'                        isofit = GermanFit)
#' 
#' ## We perform the assignment on land only
#' AssignmentDry <- isofind(data = AssignDataAlien,
#'                          isoscape = GermanScape,
#'                          calibfit = CalibAlien)
#' 
#' ## perform the assignment on land and water
#' Assignment <- isofind(data = AssignDataAlien,
#'                       isoscape = GermanScape,
#'                       calibfit = CalibAlien,
#'                       mask = NULL)
#' 
#' ## We plot the group assignment
#' plot(Assignment, who = "group", mask = list(mask = NULL))
#' 
#' plot(AssignmentDry, who = "group", mask = list(mask = NULL))
#' 
#' ## We plot the assignment for the 8 first samples
#' plot(AssignmentDry, who = 1:8,
#'      sources = list(draw = FALSE),
#'      calibs = list(draw = FALSE))
#' 
#' ## We plot the assignment for the sample "Alien_10"
#' plot(AssignmentDry, who = "Alien_10")
#' 
#' 
#' ### Other example without calibration:
#' ### We will try to assign a weather station 
#' ### in the water isoscape
#' 
#' ## We create the assignment data taking 
#' ## GARMISCH-PARTENKIRCHEN as the station to assign
#' GPIso <- GNIPDataDEagg[GNIPDataDEagg$source_ID == "GARMISCH-PARTENKIRCHEN", "mean_source_value"]
#' AssignDataGP <- data.frame(sample_value = GPIso,
#'                            sample_ID = "GARMISCH-PARTENKIRCHEN")
#' 
#' ## We perform the assignment
#' AssignedGP <- isofind(data = AssignDataGP,
#'                       isoscape = GermanScape,
#'                       calibfit = NULL)
#' ## We plot the assignment and 
#' ## show where the station really is (using lattice)
#' plot(AssignedGP) +
#'   lattice::xyplot(47.48~11.06,
#'                   panel = lattice::panel.points,
#'                   cex = 5, pch = 13, lwd = 2, col = "black") 
#' 
#' 
#' }
#' 
#' @export
isofind <- function(data,
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
  
  names_layers <- gsub(" ", "_", as.character(data$sample_ID))
  
  time <- system.time({
    if (!is.null(calibfit)) {
      ## we predict the isotopic value at origin location  
      data$mean_origin <-
        (data$sample_value - calibfit$param["intercept"])/calibfit$param["slope"]
      ## we create individual rasters containing the test statistics
      list_stat_layers <- sapply(1:nrow(data),  ## ToDo: change that so it can use HD for heavy layers
                                 function(i) {
                                   data$mean_origin[i] - isoscape$isoscapes$mean
                                 }
                                 )
    } else {
      ## we create individual rasters containing the test statistics
      list_stat_layers <- sapply(1:nrow(data),
                                 function(i) {
                                   data$sample_value[i] - isoscape$isoscapes$mean
                                 }
                                 )
    }
    
    names(list_stat_layers) <- names_layers
    stat_brick <- raster::brick(list_stat_layers)
    if (any(names_layers != names(stat_brick))) {
      warning("Your sample_ID could not be used to name rasters (you may have used numbers, symbols or punctuations that is messing with the package raster), so they have been slightly modified by this package.")
      names_layers <- names(stat_brick) ## trick to track the good names as they can change during stacking (if numbers)
    }
    
    ### WE COMPUTE THE VARIANCE OF THE TEST

    if (!is.null(calibfit)) {
      ## we compute fixedVar
      X <- cbind(1, data$mean_origin)
      fixedVar <- rowSums(X * (X %*% calibfit$fixefCov)) ## = diag(X %*% calibfit$fixefCov %*% t(X))
      ## we create individual rasters containing the variance of the test statistics
      list_varstat_layers <- sapply(1:nrow(data),
                                    function(i) {
                                      isoscape$isoscapes$mean_predVar + 
                                      calibfit$phi/calibfit$param["slope"]^2 +
                                      fixedVar[i]/calibfit$param["slope"]^2 +
                                      0 ## ToDo compute fourth variance term
                                    }
                                    )
    } else {
      ## we create individual rasters containing the variance of the test statistics
      list_varstat_layers <- sapply(1:nrow(data),
                                    function(i) {
                                      isoscape$isoscapes$mean_respVar
                                    }
                                    )
    }

    names(list_varstat_layers) <- names_layers
    varstat_brick <- raster::brick(list_varstat_layers)

    ### WE COMPUTE THE INDIVIDUAL LOG P-VALUE SURFACES
    if (verbose) {
      print("running the assignment test...")
    }

    ## we initialize the brick
    logpv_brick <- raster::raster(varstat_brick)

    ## we create individual rasters containing the p-values of the test
    for (sample_ID in names_layers) {
      name_layer <- paste("logpv_brick$", sample_ID, sep = "")
      expr_to_run <- paste(name_layer,
                           "<- .assign_test(raster::values(stat_brick[[sample_ID]]), raster::values(varstat_brick[[sample_ID]]))"
                           )
      eval(parse(text = expr_to_run))
    }

    ### WE COMBINE SAMPLE SURFACES USING FISHER'S METHOD
    if (verbose) {
      print("combining assignments across samples...")
    }

    group_pv <- raster::calc(logpv_brick, .Fisher_method)
  })  ## end of system.time

  ## display time
  time <- round(as.numeric((time)[3]))
  if (verbose) {
    print(paste("assignments for all", nrow(data), "organisms have been computed in", time, "sec."))
  }

  ## remove log scale
  if (verbose) {
    print("converting log p-values into p-values...")
  }
  pv_brick <- exp(logpv_brick)
  names(pv_brick) <- names_layers  ## we restore the names as they are not kept when computing
  rm(logpv_brick)

  ## replacing values by zeros if they fall in the mask (e.g. in water)
  if (!is.null(mask)) {
    if (verbose) {
      print("applying the mask...")
    }

    ## turn mask into raster with NA inside polygons
    raster_mask <- is.na(raster::rasterize(mask, stat_brick))

    ## multiplying rasters by the raster_mask    
    stat_brick <- stat_brick*raster_mask
    names(stat_brick) <- names_layers ## we restore the names as they are not kept when computing

    varstat_brick <- varstat_brick*raster_mask
    names(varstat_brick) <- names_layers ## we restore the names as they are not kept when computing

    pv_brick <- pv_brick*raster_mask
    names(pv_brick) <- names_layers ## we restore the names as they are not kept when computing
    
    group_pv <- raster::overlay(group_pv, raster_mask, fun = prod)
  }


  ### RETURNS
  
  calibs <- NULL
  if (!is.null(calibfit)) {
    calibs <- calibfit$sp_points$calibs
  }

  out <- list(sample = list("stat" = stat_brick,
                           "stat_var" = varstat_brick,
                           "pv" = pv_brick
                           ),
              group = list("pv" = group_pv),
              sp_points = list("sources" = isoscape$sp_points$sources,
                               "calibs" = calibs
                               )
              )

  class(out) <- c("ISOFIND", "list")
  return(out)
}


.assign_test <- function(stats, vars, log_scale = TRUE) {
  pv <- 2*(1 - stats::pnorm(abs(stats), mean = 0, sd = sqrt(vars)))
  if (log_scale) {
    pv <- log(pv)
  }
  return(pv)
}


.Fisher_method <- function(logpv) {
  if (length(logpv) == 1) {
    return(exp(logpv))
  }
  Fisher_stat <- -2*sum(logpv)
  df <- 2*length(logpv)
  pv <- stats::pchisq(q = Fisher_stat, df = df, lower.tail = FALSE)
  return(pv)
}

#' @export
#' @method print ISOFIND
print.ISOFIND <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}

#' @export
#' @method summary ISOFIND
summary.ISOFIND <- function(object, ...) {
  for (i in names(object)[names(object) != "sp_points"]) {
    cat(paste("######### assignment raster(s): '", i, "'"), "\n")
    print(object[[i]])
    cat("\n")
  } 
  return(invisible(NULL))
}

