## Here are internal functions of the package IsoriX

.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It display a message when the package is being loaded
  packageStartupMessage(## display message
                        "\n IsoriX version ", utils::packageDescription("IsoriX")$Version," is loaded!",
                        "\n",
                        "\n Many functions and objects have changed names since the version 0.4.",
                        "\n This is to make IsoriX more intuitive for you to use.",
                        "\n We will do our best to limit changes in names in the future!!",
                        "\n",
                        "\n Type:",
                        "\n    * ?IsoriX for a short description.",
                        "\n    * browseVignettes(package = 'IsoriX') for tutorials.",
                        "\n    * news(package = 'IsoriX') for news.",
                        "\n"
                        )
  }


.IsoriX_options <- new.env(parent = emptyenv())


.onLoad <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It changes the default beahviour of sp concerning lat/long boundaries
  .IsoriX_options$sp_ll_warn <- sp::get_ll_warn()
  sp::set_ll_warn(TRUE)  ## makes sp creating warning instead of errors when lat/long out of boundaries
}


.onUnload <- function(libpath) {
  ## This function should not be called by the user.
  ## It restores the original behaviour of sp
  sp::set_ll_warn(.IsoriX_options$sp_ll_warn)
}


.NiceRound <- function(x, digits) {
  formatC(round(x, digits), digits = digits, format = "f")
}


.CreateRaster <- function(long, lat, values, proj) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It creates a raster.
  ##
  ## Args:
  ##   long: a vector of the longitudes of the raster cells
  ##   lat: a vector of the latitudes of the raster cells
  ##   values: a vector of the values of the raster cells
  ##   proj: the projection system for the raster
  ##   save.spatial.files: logical indicating if an hard copy of the raster should be saved (as ascii)
  ##   filename: name of the file for the hard copy
  ##   overwrite.spatial.files: logical indicating if an existing hard copy should be overwritten or not
  ##
  ## Returns:
  ##   The raster.
  ##
  data <- data.frame(long = long, lat = lat, values = values)
  sp::coordinates(data) <- ~long+lat  ## coordonates are being set for the raster
  sp::proj4string(data) <- sp::CRS(proj)  ## projection is being set for the raster
  sp::gridded(data) <- TRUE  ## a gridded structure is being set for the raster
  data.raster <- raster::raster(data)  ## the raster is being created
  # if(save.spatial.files) writeRaster(
  #   data.raster,
  #   filename = paste(filename, ".asc", sep = ""),
  #   overwrite = overwrite.spatial.files
  #   )  ## if save = TRUE the raster is exported as an ascii file
  return(data.raster) ## the raster is being returned
}


.CreateSpatialPoints <- function(long, lat, values = -9999, proj) {
  ##  This function should not be called by the user but is itself called by .CreateRasterFromAssignment().
  data.sp <- data.frame(long = long, lat = lat, values = values)
  sp::coordinates(data.sp) <- ~long+lat
  sp::proj4string(data.sp) <- sp::CRS(proj)
  return(data.sp)
}


.HitReturn <- function() {
  ## This function should not be called by the user but is itself called by other functions.
  ## It ask the user to press return in RStudio (for plotting).
  if (interactive() & .Platform$GUI == "RStudio") {
    cat("Hit <Return> for next plot")
    readline()
  }
  return(NULL)
}


.CompleteArgs <- function(fn) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It keeps the default list elements when
  ## a new list with fewer elements is provided
  env <- parent.frame()
  args <- formals(fn)
  for (arg.name in names(args)) {
    if (is.call(arg <- args[[arg.name]])) {
      if (arg[1] == "list()") {
        arg.input <- mget(names(args), envir = env)[[arg.name]]
        arg.full  <- eval(formals(fn)[[arg.name]])
        arg.full.updated <- utils::modifyList(arg.full, arg.input)
        assign(arg.name, arg.full.updated, envir = env)
      }
    }
  }
  return(NULL)
}


.BuildAdditionalLayers <- function(x, sources, calib, borders, mask, mask2 = NULL) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It build the additional layers for plots

  ## layer for sources
  if (!sources$draw) {
    sources.layer <- latticeExtra::layer()
  } else {
    sources.layer <- latticeExtra::layer(sp::sp.points(sources,
                                                   col = pt$col,
                                                   cex = pt$cex,
                                                   pch = pt$pch,
                                                   lwd = pt$lwd
                                                   ),
                           data = list(sources = x$sp.points$sources,
                                       pt = sources,
                                       sp.points = sp::sp.points
                                       )
                           )
  }

  ## layer for calibration points
  if (is.null(calib)) {
    calib.layer <- latticeExtra::layer()
  } else {
    if (!calib$draw) {
      calib.layer <- latticeExtra::layer()
    } else {
      calib.layer <- latticeExtra::layer(sp::sp.points(calib, col = pt$col,
                                         cex = pt$cex,
                                         pch = pt$pch,
                                         lwd = pt$lwd
                                         ),
                           data = list(calib = x$sp.points$calibs,
                                       pt = calib,
                                       sp.points = sp::sp.points
                                       )
                           )
    }
  }

  ## layer for country borders
  if (is.null(borders$borders)) {
    borders.layer <- latticeExtra::layer()
  }  else {
    borders.layer <- latticeExtra::layer(sp::sp.polygons(b$borders,
                                                     lwd = b$lwd,
                                                     col = b$col,
                                                     fill = "transparent"
                                                     ),
                           data = list(b = borders,
                                       sp.polygons = sp::sp.polygons
                                       )
                           )
  }
  
  ## layer for mask
  if (is.null(mask$mask)) {
    mask.layer <- latticeExtra::layer()
  } else {
    mask.layer <- latticeExtra::layer(sp::sp.polygons(m$mask,
                                      fill = m$fill,
                                      col = m$col,
                                      lwd = m$lwd
                                      ),
                        data = list(m = mask,
                                    sp.polygons = sp::sp.polygons
                                    )
                        )
  }
  
  if (is.null(mask2$mask)) {
    mask2.layer <- latticeExtra::layer()
  } else {
    mask2.layer <- latticeExtra::layer(sp::sp.polygons(m$mask,
                                       fill = m$fill,
                                       col = m$col,
                                       lwd = m$lwd
                                       ),
                         data = list(m = mask2,
                                     sp.polygons = sp::sp.polygons
                                     )
                         )
  }
  
  out <- list(sources.layer = sources.layer,
              calib.layer = calib.layer,
              borders.layer = borders.layer,
              mask.layer = mask.layer,
              mask2.layer = mask2.layer
              )
  
  ## tweack to please code checking procedure
  b <- m <- pt <- NULL

  return(out)
}
