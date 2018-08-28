## Here are internal (not exported) functions of the package IsoriX.
## They should not be called by the user but are instead called by other IsoriX functions.

.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
                        "\n IsoriX version ", utils::packageDescription("IsoriX")$Version," is now loaded!",
                        "\n",
                        "\n The names of all objects (including functions) are finally getting stable,",
                        "\n but they have changed a lot since the version 0.7.1 (sorry!).",
                        "\n We kept revising them to make IsoriX more intuitive for you to use.",
                        "\n We will do our best to limit changes from version 0.8 onward!!",
                        "\n",
                        "\n Type:",
                        "\n    * ?IsoriX for a very short description.",
                        "\n    * help(package = 'IsoriX') for a list of the package objects and their help files.",
                        "\n    * browseURL('https://bookdown.org/content/782/') for online tutorials & documentation.",
                        "\n    * citation('IsoriX') for information on how to cite IsoriX.",
                        "\n    * news(package = 'IsoriX') for info on what has changed between the different versions of IsoriX.",
                        "\n"
                        )
  }


.onLoad <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It changes the default behaviour of sp concerning lat/long boundaries
  ## and stores the all R options.
  .data_IsoriX$sp_options$sp_ll_warn <- sp::get_ll_warn()
  sp::set_ll_warn(TRUE)  ## makes sp creating warning instead of errors when lat/long out of boundaries
  .data_IsoriX$R_options <- .Options ## backup R options
}


.onUnload <- function(libpath) {
  ## This function should not be called by the user.
  ## It restores the original behaviour of sp
  ## and the original R options.
  sp::set_ll_warn(.data_IsoriX$sp_options$sp_ll_warn)
  options(.data_IsoriX$R_options)  ## reset R options to their backed up values
}


.print_nice_and_round <- function(x, digits = 2) {
  if (digits < 0) {
    stop("digits must be positive")
  }
  ## This function should not be called by the user.
  ## It displays a rounded number keeping the number of decimals constant.
  ## digits is the number of decimals beeing displayed.
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
  sp::coordinates(data) <- ~ long+lat  ## coordonates are being set for the raster
  sp::proj4string(data) <- sp::CRS(proj)  ## projection is being set for the raster
  sp::gridded(data) <- TRUE  ## a gridded structure is being set for the raster
  raster::raster(data)  ## the raster is being created
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
  sp::coordinates(data) <- ~long+lat
  sp::proj4string(data) <- sp::CRS(proj)
  return(data)
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
    if (is.call(arg <- args[[arg_name]])) {
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
  if (!class(var) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    return(var)
  } else if (raster::inMemory(var)) {
    return(as.numeric(raster::values(var)))
  }
  
  if (interactive()) {
    print("extracting values from stored rasters...")
  }
  
  if (class(var) %in% c("RasterLayer")) {
    max_var <- raster::maxValue(var)
    min_var <- raster::minValue(var)
    var <- unique(c(min_var,
                    raster::quantile(var, seq(min_var, max_var, length = nb_quantiles)),
                    max_var))
    return(var)
  } else if (class(var) %in% c("RasterStack", "RasterBrick")) {
    max_var <- max(raster::maxValue(var))
    min_var <- min(raster::minValue(var))
    var <- unique(c(min_var,
                    apply(raster::quantile(var, seq(min_var, max_var, length = nb_quantiles)), 2, stats::median),
                    max_var))
    return(var)
  }
  stop("'var' has an unknown class")
}
