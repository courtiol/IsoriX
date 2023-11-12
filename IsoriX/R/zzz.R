## Here are internal (not exported) functions of the package IsoriX.
## They should not be called by the user but are instead called by other IsoriX functions.

.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
                        "\n IsoriX version ", utils::packageDescription("IsoriX")$Version," is now loaded",
                        "\n",
                        "\n Type:",
                        "\n   * `?IsoriX` for a very short description",
                        "\n   * `browseURL('https://bookdown.org/content/782/')` for a longer (online) documentation",
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
  options(.data_IsoriX$R_options)  ## reset R options to their backed up values
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


.create_spatial_points <- function(long, lat, values = -9999, proj = "+proj=longlat +datum=WGS84") {
  ## This function should not be called by the user.
  ## It creates spatial points.
  ##
  ## Args:
  ##   long: a vector of the longitudes for the spatial points
  ##   lat: a vector of the latitudes for the spatial points
  ##   values: a vector of the values for the spatial points
  ##   proj: the projection system for the spatial points
  ##
  ## Returns:
  ##   The spatial points
  ##
  data <- data.frame(long = long, lat = lat, values = values)
  terra::vect(data, geom = c("long", "lat"), crs = proj)
}


.hit_return <- function() {
  ## This function should not be called by the user.
  ## It asks the user to press return in RStudio (used for plotting).
  if (interactive() & .Platform$GUI == "RStudio" & getOption_IsoriX("dont_ask") == FALSE) {
    cat("Hit <Return> for next plot")
    readline()
  }
  return(NULL)
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
        arg_full  <- eval(formals(fn)[[arg_name]])
        if (is.null(names(arg_input))) {
          if (length(arg_input) == length(arg_full)) {
            names(arg_input) <- names(arg_full)
          }
          else {
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
    res <- match(tolower(x), tolower(month.abb))  ## deals with abbreviation
    if (is.na(res)) {
      res <- match(tolower(x), tolower(month.name))  ## deals with full names
    }
    if (is.na(res)) {  ## deal with other cases
      res <- x
    }
    if (res %in% paste(1:12)) { ## check if other cases are numbers (quoted or not)
      res <- as.numeric(res)
    }
    if (is.numeric(res)) {
      return(res)
    } else {
      warning("some months are NA after the conversion in integers, please check/fix your data!")
      return(NA)  ## if final output is not a number, it returns NA
    }
  })
  return(end)
}

.summarize_values <- function(var, nb_quantiles = 1e4) {
  ## This function should not be called by the user.
  ## It extracts and summarizes the raster values using quantiles if needed.
  if (!inherits(var, "SpatRaster")) {
    return(var)
  } else if (terra::inMemory(var)) {
    return(as.numeric(terra::values(var)))
  }
  
  if (interactive()) {
    print("extracting values from stored rasters...")
  }
  
  if (inherits(var, c("SpatRaster"))) {
    if (terra::nlyr(var) == 1) {
       var <- terra::quantile(var, seq(0, 1, length = nb_quantiles))
       return(var)
    } else if (terra::nlyr(var) > 1) {
    max_var <- max(terra::values(max(var)))
    min_var <- min(terra::values(min(var)))
    var <- unique(c(min_var,
                    apply(terra::quantile(var, seq(0, 1, length = nb_quantiles)), 2, stats::median),
                    max_var))
    return(var)
    }
  }
  
  stop("'var' has an unknown class")
}


.crop_withmargin <- function(raster, xmin, xmax, ymin, ymax, margin_pct = 5) {
  ## This function should not be called by the user.
  ## It crops a raster using a safety margin.
  margin_long <- (xmax - xmin) * margin_pct/100
  margin_lat  <- (ymax - ymin) * margin_pct/100
  
  terra::crop(raster, terra::ext(xmin - margin_long,
                                 xmax + margin_long,
                                 ymin - margin_lat,
                                 ymax + margin_lat))

}

.invert_reg <- function(intercept, slope, SE_I, SE_S, phi, N, sign_mean_Y) {
  ## This function should not be called by the user.
  ## It turns a regression x ~ y to a regression y ~ x.
  Nminus1 <- N - 1L
  Nminus2 <- N - 2L
  Nfac <- Nminus1/N
  
  MSExony <- phi
  VarSxony <- SE_S^2

  Vary <- MSExony/(Nminus1*VarSxony)
  Covxy <- Vary*slope
  Varx <- (MSExony*(slope^2 + Nminus2*VarSxony))/(Nminus1*VarSxony)
  o_slope <- Covxy/Varx
  
  resid_MSE <-  (Vary - Covxy^2/Varx)*Nminus1/Nminus2
  o_SE_S <- sqrt(resid_MSE/(Nminus1*Varx))
  
  Ey2 <- (SE_I/SE_S)^2 
  Ey <- sign_mean_Y * sqrt(Ey2 - Vary*Nfac)
  Ex <- intercept + slope*Ey
  Ex2 <- Varx*Nfac + Ex^2
  o_SE_I <- sqrt(resid_MSE*Ex2/(Nminus1*Varx))
  vcov12 <- -resid_MSE*Ex/(Nminus1*Varx)
  o_vcov <- matrix(c(o_SE_I^2, vcov12, vcov12, o_SE_S^2), ncol = 2)
  
  list(intercept = Ey - o_slope*Ex, 
       slope = o_slope, 
       SE_I = o_SE_I, 
       SE_S = o_SE_S, 
       phi = resid_MSE,
       vcov = o_vcov)
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
  ## It performs the lazy loading of the data since terra cannot handle rda files
  assign("ElevRasterDE", terra::rast(system.file("extdata/ElevRasterDE.tif", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("CountryBorders", terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("OceanMask", terra::readRDS(system.file("extdata/OceanMask.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
  assign("PrecipBrickDE", terra::readRDS(system.file("extdata/PrecipBrickDE.rds", package = "IsoriX")), envir = as.environment("package:IsoriX"))
}

.suppress_warning <-  function(x, warn = "") {
  ## This function should not be called by the user.
  ## It hides expected warnings in some functions
  withCallingHandlers(x, warning = function(w) {
    if (!grepl(warn, x = w[[1]])) {
      warning(w[[1]], call. = FALSE)
    }
    invokeRestart("muffleWarning")
  })
}

utils::globalVariables(c("CountryBorders", "OceanMask"))
