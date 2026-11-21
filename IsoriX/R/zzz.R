## Here are internal (not exported) functions of the package IsoriX.
## They should not be called by the user but are instead called by other IsoriX functions.

.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage( ## display message
    "\n IsoriX version ", utils::packageDescription("IsoriX")$Version, " is now loaded",
    "\n",
    "\n Type:",
    "\n   * `?IsoriX` for a very short description",
    "\n   * `browseURL('https://courtiol-isorix.share.connect.posit.cloud/')` for a longer (online) documentation",
    "\n   * `help(package = 'IsoriX', help_type = 'html')` for a list of the package objects and help files",
    "\n   * `citation('IsoriX')` for how to cite IsoriX (i.e. the papers you should read)",
    "\n   * `news(package = 'IsoriX')` for info on changed between versions of IsoriX",
    "\n",
    "\n Please join the mailing list 'https://groups.google.com/g/IsoriX'",
    "\n for help, news and discussions about IsoriX",
    "\n"
  )

  .load_internal_files() ## lazy loading of the internal data
}


.onLoad <- function(libname, pkgname) {
  ## This function should not be called by the user.
  .data_IsoriX$R_options <- .Options ## backup R options
}


.onUnload <- function(libpath) {
  ## This function should not be called by the user.
  ## It restores the original R options.
  options(.data_IsoriX$R_options) ## reset R options to their backed up values
}


.print_nice_and_round <- function(x, digits = 2) {
  if (digits < 0) {
    stop("digits must be positive")
  }
  ## This function should not be called by the user.
  ## It displays a rounded number keeping the number of decimals constant.
  ## digits is the number of decimals being displayed.
  formatC(x, digits = digits, format = "f")
}


.create_raster <- function(long, lat, values, proj = "+proj=longlat +datum=WGS84") {
  ## This function should not be called by the user.
  ## It creates a raster.
  ##
  ## Args:
  ##   long: a vector of the longitudes for the raster cells
  ##   lat: a vector of the latitudes for the raster cells
  ##   values: a vector of the values for the raster cells
  ##   proj: the projection system for the raster
  ##
  ## Returns:
  ##   The raster
  ##
  data <- data.frame(long = long, lat = lat, values = values)
  terra::rast(data, crs = proj) ## the raster is being created
}


.create_spatial_points <- function(long, lat, values = -9999, ID = NULL, proj = "+proj=longlat +datum=WGS84") {
  ## This function should not be called by the user.
  ## It creates spatial points.
  ##
  ## Args:
  ##   long: a vector of the longitudes for the spatial points
  ##   lat: a vector of the latitudes for the spatial points
  ##   values: a vector of the values for the spatial points
  ##   ID: an optional vector of identifiers for the spatial points (stored in the column ID)
  ##   proj: the projection system for the spatial points
  ##
  ## Returns:
  ##   The spatial points
  ##
  data <- data.frame(long = long, lat = lat, values = values)
  if (!is.null(ID)) {
    data$ID <- as.character(ID)
  }
  terra::vect(data, geom = c("long", "lat"), crs = proj)
}


.hit_return <- function() {
  ## This function should not be called by the user.
  ## It asks the user to press return in RStudio (used for plotting).
  if (interactive() && .Platform$GUI == "RStudio" && getOption_IsoriX("dont_ask") == FALSE) {
    cat("Hit <Return> for next plot")
    readline()
  }
  return(NULL)
}


.IsoriX_objects <- c(
  ## the classes of the objects created by IsoriX and the functions creating them,
  ## from the most to the least specific (the first match is used)
  MULTIISOFIT = "isomultifit()",
  ISOSIM = "isosim()",
  ISOFIT = "isofit()",
  ISOSCAPE = "isoscape() or isomultiscape()",
  CALIBFIT = "calibfit()",
  ISOFIND = "isofind()"
)


.describe_input <- function(x) {
  ## This function should not be called by the user.
  ## It describes an object in error messages about wrong inputs.
  if (is.null(x)) {
    return("NULL")
  }
  known <- intersect(names(.IsoriX_objects), class(x))
  if (length(known) > 0) {
    return(paste0("the output of ", .IsoriX_objects[[known[1]]], " (an object of class ", known[1], ")"))
  }
  paste0("an object of class ", paste(class(x), collapse = "/"))
}


.check_input_class <- function(x, expected, arg, fn, null_ok = FALSE) {
  ## This function should not be called by the user.
  ## It stops with an informative message if the input `x` (given to the
  ## argument `arg` of the function `fn`) does not inherit from the class
  ## `expected` (which can be NULL if `null_ok = TRUE`) (#164).
  if (is.null(x) && null_ok) {
    return(invisible(TRUE))
  }
  if (!inherits(x, expected)) {
    what <- if (expected %in% names(.IsoriX_objects)) {
      paste0("the output of ", .IsoriX_objects[[expected]], " (an object of class ", expected, ")")
    } else if (expected == "SpatRaster") {
      "a raster of class SpatRaster (see e.g. terra::rast() or prepraster())"
    } else if (expected == "data.frame") {
      "a data frame"
    } else {
      paste("an object of class", expected)
    }
    if (null_ok) what <- paste(what, "or NULL")
    stop(paste0(
      "The argument `", arg, "` of ", fn, "() must be ", what,
      ", but the object provided is ", .describe_input(x), "."
    ), call. = FALSE)
  }
  invisible(TRUE)
}


.check_source_data <- function(data, fn) {
  ## This function should not be called by the user.
  ## It checks that the source data are aggregated, as required to fit isoscapes (#164).
  .check_input_class(data, "data.frame", "data", fn)
  if (!"mean_source_value" %in% colnames(data)) {
    if ("source_value" %in% colnames(data)) {
      stop(paste0(
        "The argument `data` of ", fn, "() must contain source data aggregated per location, ",
        "but the data provided seem to be raw source data: use prepsources() to aggregate them first."
      ), call. = FALSE)
    }
    stop(paste0(
      "The argument `data` of ", fn, "() must contain source data aggregated per location ",
      "(as produced by prepsources()), but the column 'mean_source_value' is missing."
    ), call. = FALSE)
  }
  invisible(TRUE)
}


.complete_args <- function(fn) {
  ## This function should not be called by the user.
  ## It keeps the default list elements when
  ## a new list with fewer elements is provided.
  env <- parent.frame()
  args <- formals(fn)
  for (arg_name in names(args)) {
    if (is.call(x = arg <- args[[arg_name]])) {
      if (arg[1] == "list()") {
        arg_input <- mget(names(args), envir = env)[[arg_name]]
        arg_full <- eval(formals(fn)[[arg_name]])
        if (length(arg_input) > 0 && is.null(names(arg_input))) { ## an empty list means: keep all defaults (#206)
          if (length(arg_input) == length(arg_full)) {
            names(arg_input) <- names(arg_full)
          } else {
            stop(paste("The list", arg_name, "should contain names, or be of length equal to the default."))
          }
        }
        arg_full_updated <- utils::modifyList(arg_full, arg_input)
        assign(arg_name, arg_full_updated, envir = env)
      }
    }
  }
  return(NULL)
}


.converts_months_to_numbers <- function(x) {
  ## This function should not be called by the user.
  ## It converts an English month names (abbreviated or not) into numbers.
  ## If the months are already as numbers, it works too.
  ## Example: .converts_months_to_numbers(c("January", "Feb", 3, "April", "Toto"))
  end <- sapply(x, function(x) {
    res <- match(tolower(x), tolower(month.abb)) ## deals with abbreviation
    if (is.na(res)) {
      res <- match(tolower(x), tolower(month.name)) ## deals with full names
    }
    if (is.na(res)) { ## deal with other cases
      res <- x
    }
    if (res %in% paste(1:12)) { ## check if other cases are numbers (quoted or not)
      res <- as.numeric(res)
    }
    if (is.numeric(res)) {
      return(res)
    } else {
      warning("some months are NA after the conversion in integers, please check/fix your data!")
      return(NA) ## if final output is not a number, it returns NA
    }
  })
  return(end)
}

.summarize_values <- function(var, nb_quantiles = 1e4) {
  ## This function should not be called by the user.
  ## It extracts and summarizes the raster values using quantiles if needed.
  if (!inherits(var, "SpatRaster")) {
    return(var)
  } else { # if (terra::inMemory(var))
    return(as.numeric(terra::values(var)))
  }

  if (interactive()) {
    print("extracting values from stored rasters...")
  }

  if (inherits(var, c("SpatRaster"))) {
    if (terra::nlyr(var) == 1) {
      var <- terra::quantile(var, seq(0, 1, length.out = nb_quantiles))
      return(var)
    } else if (terra::nlyr(var) > 1) {
      max_var <- max(terra::values(max(var)))
      min_var <- min(terra::values(min(var)))
      var <- unique(c(
        min_var,
        apply(terra::quantile(var, seq(0, 1, length.out = nb_quantiles)), 2, stats::median),
        max_var
      ))
      return(var)
    }
  }

  stop("'var' has an unknown class")
}


.check_raster_valid <- function(raster, arg = "raster") {
  ## This function should not be called by the user.
  ## It stops with an informative error if the SpatRaster `raster` can no
  ## longer be used, e.g. because it was restored from a saved workspace
  ## (.RData) or from another R session: the data of such objects (stored by
  ## terra outside of R) are then lost, which otherwise leads to obscure errors
  ## such as "NULL value passed as symbol address". The class of `raster` must
  ## be checked beforehand (see .check_input_class()).
  valid <- tryCatch(
    {
      terra::ext(raster)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!valid) {
    stop("The SpatRaster passed to the argument '", arg, "' is no longer valid in this R session ",
      "(it was probably restored from a saved workspace or from another R session). ",
      "Please re-create it using terra::rast() on the file downloaded with getelev() ",
      "(see ?getelev).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


.crop_withmargin <- function(raster, xmin, xmax, ymin, ymax, margin_pct = 5) {
  ## This function should not be called by the user.
  ## It crops a raster using a safety margin.
  ## Crops crossing the antimeridian are supported (see .crop_long()).
  margin_long <- (xmax - xmin) * margin_pct / 100
  margin_lat <- (ymax - ymin) * margin_pct / 100

  .crop_long(raster,
    xmin = xmin - margin_long,
    xmax = xmax + margin_long,
    ymin = ymin - margin_lat,
    ymax = ymax + margin_lat
  )
}


## Helpers for handling the antimeridian (International Date Line, #180) --------
##
## IsoriX expresses longitudes within [-180, 180] by default. When the area of
## interest crosses the antimeridian, longitudes are instead expressed
## continuously beyond 180 (e.g. from 70 to 210), so that the area is
## contiguous. The helpers below create and handle this representation.

.continuous_long_range <- function(long_min, long_max, from = -180) {
  ## This function should not be called by the user.
  ## It expresses a range of longitudes continuously, with `long_min` within
  ## [from, from + 360) and `long_max` greater than `long_min`, e.g.:
  ## (70, -150) -> (70, 210), (70, 210) -> (70, 210), (-200, -150) -> (160, 210).
  if (long_min > long_max) long_max <- long_max + 360
  shift <- 360 * floor((long_min - from) / 360)
  c(long_min - shift, long_max - shift)
}


.wrap_long_like <- function(long, long_ref) {
  ## This function should not be called by the user.
  ## It expresses the longitudes `long` in the same representation as the
  ## reference longitudes `long_ref`: within [0, 360] if some reference
  ## longitudes are greater than 180 and within [-180, 180] otherwise.
  if (is.null(long) || length(long_ref) == 0) {
    return(long)
  }
  if (any(long_ref > 180, na.rm = TRUE)) {
    return(ifelse(!is.na(long) & long < 0, long + 360, long))
  }
  ifelse(!is.na(long) & long > 180, long - 360, long)
}


.muffle_long_warning <- function(expr) {
  ## This function should not be called by the user.
  ## It evaluates `expr` while muffling the warning emitted by spaMM when
  ## longitudes are greater than 180 degrees. Such longitudes are used on
  ## purpose by IsoriX when the area of interest straddles the antimeridian,
  ## and the great-circle distances computed by spaMM ("Earth" method) are not
  ## affected by the representation of longitudes.
  withCallingHandlers(expr,
    warning = function(w) {
      if (grepl("max(abs(longitude)) should be <= 180", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


.is_global_raster <- function(raster) {
  ## This function should not be called by the user.
  ## It tests whether a raster covers all longitudes.
  (terra::xmax(raster) - terra::xmin(raster)) >= (360 - terra::res(raster)[1] / 2)
}


.crop_long <- function(raster, xmin, xmax, ymin, ymax) {
  ## This function should not be called by the user.
  ## It crops a raster as terra::crop() but, if the raster covers all
  ## longitudes, it also allows for extents crossing the antimeridian, defined
  ## either with xmin > xmax (e.g. xmin = 70, xmax = -150) or with longitudes
  ## beyond 180 (e.g. xmin = 70, xmax = 210). In such cases, the raster
  ## returned is expressed with continuous longitudes (e.g. 70 to 210).
  if (!.is_global_raster(raster)) {
    return(terra::crop(raster, terra::ext(xmin, xmax, ymin, ymax)))
  }
  if ((xmax - xmin) >= 360) { ## no crop needed in longitude
    return(terra::crop(raster, terra::ext(terra::xmin(raster), terra::xmax(raster), ymin, ymax)))
  }
  long_range <- .continuous_long_range(xmin, xmax, from = terra::xmin(raster))
  east <- terra::crop(raster, terra::ext(long_range[1], min(long_range[2], terra::xmax(raster)), ymin, ymax))
  if (long_range[2] <= terra::xmax(raster)) {
    return(east)
  }
  west <- terra::crop(raster, terra::ext(terra::xmin(raster), long_range[2] - 360, ymin, ymax))
  .assemble_across_antimeridian(east = east, west = west)
}


.assemble_across_antimeridian <- function(east, west) {
  ## This function should not be called by the user.
  ## It assembles two rasters located on each side of the antimeridian (east:
  ## up to 180, west: from -180) into a single raster with continuous
  ## longitudes (e.g. from 70 to 210). The western raster is shifted by 360
  ## degrees and resampled onto the grid of the eastern one, since rasters
  ## downloaded separately are not necessarily aligned.
  west <- terra::shift(west, dx = 360)
  grid <- terra::extend(east, terra::union(terra::ext(east), terra::ext(west)))
  terra::cover(grid, terra::resample(west, grid))
}


.wrap_vector_to_extent <- function(vector, extent) {
  ## This function should not be called by the user.
  ## It adds to a SpatVector (e.g. country borders, a mask or points) copies
  ## shifted by 360 degrees of longitude when the extent of a map goes beyond
  ## 180 degrees (or below -180 degrees), so that the vector is drawn over the
  ## whole map (e.g. for maps centred on the Pacific). Copies falling outside
  ## the map are simply not drawn.
  if (!inherits(vector, "SpatVector") || is.null(extent)) {
    return(vector)
  }
  shifts <- 0
  if (terra::xmax(extent) > 180) shifts <- c(shifts, 360)
  if (terra::xmin(extent) < -180) shifts <- c(shifts, -360)
  if (length(shifts) == 1) {
    return(vector)
  }
  do.call(rbind, lapply(shifts, function(dx) terra::shift(vector, dx = dx)))
}

.invert_reg <- function(intercept, slope, SE_I, SE_S, phi, N, sign_mean_Y) {
  ## This function should not be called by the user.
  ## It turns a regression x ~ y to a regression y ~ x.
  Nminus1 <- N - 1L
  Nminus2 <- N - 2L
  Nfac <- Nminus1 / N

  MSExony <- phi
  VarSxony <- SE_S^2

  Vary <- MSExony / (Nminus1 * VarSxony)
  Covxy <- Vary * slope
  Varx <- (MSExony * (slope^2 + Nminus2 * VarSxony)) / (Nminus1 * VarSxony)
  o_slope <- Covxy / Varx

  resid_MSE <- (Vary - Covxy^2 / Varx) * Nminus1 / Nminus2
  o_SE_S <- sqrt(resid_MSE / (Nminus1 * Varx))

  Ey2 <- (SE_I / SE_S)^2
  Ey <- sign_mean_Y * sqrt(Ey2 - Vary * Nfac)
  Ex <- intercept + slope * Ey
  Ex2 <- Varx * Nfac + Ex^2
  o_SE_I <- sqrt(resid_MSE * Ex2 / (Nminus1 * Varx))
  vcov12 <- -resid_MSE * Ex / (Nminus1 * Varx)
  o_vcov <- matrix(c(o_SE_I^2, vcov12, vcov12, o_SE_S^2), ncol = 2)

  list(
    intercept = Ey - o_slope * Ex,
    slope = o_slope,
    SE_I = o_SE_I,
    SE_S = o_SE_S,
    phi = resid_MSE,
    vcov = o_vcov
  )
}
# Example:
# set.seed(123)
# xy <- data.frame(x = x <- rnorm(20), y = rnorm(20, mean = 10) + 0.7*x)
# input <- lm(x ~ y, data = xy)
# output <- lm(y ~ x, data = xy)
#
# foo <- .invert_reg(intercept = coef(input)[1],
#                   slope = coef(input)[2],
#                   SE_I = sqrt(vcov(input)[1, 1]),
#                   SE_S = sqrt(vcov(input)[2, 2]),
#                   phi = summary(input)$sigma^2,
#                   sign_mean_Y = sign(mean(xy$y)),
#                   N = 20)
#
# d_output <- data.frame(intercept =  coef(output)[1],
#                       slope = coef(output)[2],
#                       SE_I = sqrt(vcov(output)[1, 1]),
#                       SE_S = sqrt(vcov(output)[2, 2]),
#                       phi = summary(output)$sigma^2)
#
# d_foo <- data.frame(intercept =  foo$intercept,
#                    slope = foo$slope,
#                    SE_I = foo$SE_I,
#                    SE_S = foo$SE_S,
#                    phi = foo$phi)
#
# rbind(d_output, d_foo)


.load_internal_files <- function() {
  ## This function should not be called by the user.
  ## It performs the lazy loading of the data since terra cannot handle rda files.
  assign("ElevRasterDE", terra::rast(system.file("extdata/ElevRasterDE.tif", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("CountryBorders", terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("OceanMask", terra::readRDS(system.file("extdata/OceanMask.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("PrecipBrickDE", terra::readRDS(system.file("extdata/PrecipBrickDE.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
}

## from purrr::quietly()
.quiet <- function(job) {
  ## This function should not be called by the user.
  ## It capture warnings and messages in some functions.
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })
  result <- withCallingHandlers(job, warning = wHandler, message = mHandler)
  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")
  list(
    result = result,
    output = output,
    warnings = warnings,
    messages = messages
  )
}

.safe_and_quiet_predictions <- function(object, newdata, variances = list(), ...) {
  ## This function should not be called by the user.
  ## It run the predictions while capturing errors, warnings and messages without stopping

  res <- .quiet(tryCatch(.muffle_long_warning(spaMM::predict.HLfit(object = object, newdata = newdata, variances = variances, ...)),
    error = function(e) {
      ## debugging mode
      if (getOption_IsoriX("spaMM_debug")) {
        stop(e)
      }

      ## convert error into warning to avoid stopping
      warning(paste("  WARNING converted from ERROR in `spaMM::predict.HLfit()`:\n", e))

      ## return NAs for all outputs of `spaMM::predict.HLfit` if there is an error
      m <- matrix(NA, nrow = nrow(newdata), ncol = 1)
      if (isTRUE(as.vector(variances)[["predVar"]]) || isTRUE(as.vector(variances)[["respVar"]])) {
        if (isTRUE(as.vector(variances)[["cov"]])) {
          param_fixed <- names(spaMM::fixef(object))
          m <- matrix(NA, nrow = nrow(length(param_fixed)), ncol = nrow(length(param_fixed)))
          rownames(m) <- colnames(m) <- names(param_fixed)
          attr(m, "predVar") <- m
        }
        attr(m, "predVar") <- rep(NA, nrow(newdata))
      }
      if (isTRUE(as.vector(variances)[["residVar"]])) {
        if (isTRUE(as.vector(variances)[["cov"]])) {
          param_fixed <- names(spaMM::fixef(object))
          m <- matrix(NA, nrow = nrow(length(param_fixed)), ncol = nrow(length(param_fixed)))
          rownames(m) <- colnames(m) <- names(param_fixed)
          attr(m, "residVar") <- m
        }
        attr(m, "residVar") <- rep(NA, nrow(newdata))
      }
      if (isTRUE(as.vector(variances)[["respVar"]])) {
        if (isTRUE(as.vector(variances)[["cov"]])) {
          param_fixed <- names(spaMM::fixef(object))
          m <- matrix(NA, nrow = nrow(length(param_fixed)), ncol = nrow(length(param_fixed)))
          rownames(m) <- colnames(m) <- names(param_fixed)
          attr(m, "respVar") <- m
        }
        attr(m, "respVar") <- rep(NA, nrow(newdata))
      }
      return(m)
    }
  ))

  ## debugging mode
  if (getOption_IsoriX("spaMM_debug")) {
    if (length(res$warnings) > 0) warnings(res$warnings)
    if (length(res$messages) > 0) message(res$messages)
  }

  ## return
  res
}

release_bullets <- function() {
  ## This function should not be called by the user.
  ## It is used to add bullet points to GitHub issues created with `usethis::use_release_issue()`
  c(
    "run `styler:::style_active_pkg()` for reformatting of the code according to `styler::tidyverse_style()`",
    "run `lintr::lint_package()` for checking code in depth",
    "run `all_checks <- rhub::rhub_check()`",
    "run `usethis::use_cran_comments()` and paste the output of `all_checks$cran_summary()` in the created file",
    "run `make build` & `make check` in terminal (at the root of the repo)",
    "check folder `IsoriX.Rcheck` and file in there",
    "run `tests/local_tests.R` step-by-step",
    "run `devtools::test()` (which also runs the plot snapshot tests, skipped otherwise) and review any intended change in plots with `testthat::snapshot_review()`",
    "from the root of the repo, run `Rscript release_checks/compare_versions.R` to compare all plots and key numbers of the full workflow between the CRAN version and the candidate version; review `report.html` (see release_checks/README.md)",
    "update manual using `devtools::build_manual(path = '../pkg_manuals')`",
    "from the root of the repo, run `Rscript release_checks/list_links.R` and review `report.html`: fix broken links, update redirected (possibly outdated) ones and open in a browser those with access denied, in the bookdown and in the documentation",
    "rebuilt bookdown (after `usethis::use_version('patch')`) to make sure all works in there too (follow instructions in bookdown/README)",
    "update bookdown (after `usethis::use_version('patch')`; follow instructions in bookdown/README)",
    "check that the bookdown links in the default-model message of `isofit()` and in `?isofit` work after updating the bookdown",
    "write email to Google Group (https://groups.google.com/g/IsoriX)"
  )
}


utils::globalVariables(c("CountryBorders", "OceanMask"))

