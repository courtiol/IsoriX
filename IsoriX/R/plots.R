#' Plotting functions for IsoriX
#'
#' These functions plot objects created by \pkg{\link{IsoriX}}
#' (with the exception of plot method for RasterLayer created using \pkg{\link{raster}}).
#'
#' When called upon an object of class \var{isofit}, the plot function
#' draws diagnostic information for the fits of the isoscape geostatistical
#' model.
#'
#' When called upon an object of class \var{calibfit}, the plot function draws
#' the fitted calibration function.
#'
#' When called upon an object of class \var{isoscape}, the plot function draws
#' a fine-tuned plot of the isoscape.
#'
#' When used on a fitted isoscape, the user can choose between plotting the
#' predictions (\code{which} = "mean"; default), the prediction variance (\code{which} =
#' "mean.predVar"), the residual variance (\code{which} = "mean.residVar"), or the
#' response variance (\code{which} = "mean.respVar") for the mean model; or the
#' corresponding information for the residual dispersion variance model ("disp",
#' "disp.predVar", "disp.residVar", or "disp.respVar").
#'
#' When used on a simulated isoscape, the user can choose between plotting the
#' mean isotopic value (\code{which} = "mean") or the dispersion (\code{which} = "disp").
#'
#' When called upon an object of class \var{isorix}, the plot function draws a
#' fine-tuned plot of the assignment. You can use the argument \code{who} to choose
#' between plotting the assignment for the group or for some individuals (check
#' the vignette "Workflow" for examples).
#'
#' The arguments \code{cutoff}, \code{sources}, \code{calib}, \code{borders}, \code{mask}, and \code{mask2}
#' are used to fine-tune additional layers that can be added to the main plot to
#' embellish it. These arguments must be lists that provide details on how to
#' draw, respectively, the area outside the prediction interval (for assignment
#' plots), the locations of sources (for both isoscape and assignment plots),
#' the locations of the calibration sampling area (for assignment plots, the
#' borders (for both types of plots), and the mask (again, for both)). For
#' assignment maps, an extra mask can be used (mask2), as one may want to add a
#' mask covering the area outside the biological range of the species. Within
#' these lists, the elements \code{lwd}, \code{col}, \code{cex}, \code{pch} and \code{fill} influences
#' their respective objects as in traditional R plotting functions (see
#' \code{\link{par}} for details). The element \code{draw} should be a \var{logical}
#' that indicates whether the layer must be created or not. The argument
#' \code{borders} (within the list borders) expects an object of the class
#' \var{SpatialPolygons} such as the object \code{\link{CountryBorders}} provided with this
#' package. The argument \code{mask} (within the list maks) expects an object of the
#' class \var{SpatialPolygons} such as the object \code{\link{OceanMask}} provided with this
#' package (see examples).
#'
#' The argument \code{palette} is used to define how to colour the isoscape and
#' assignment plot. Within this list, \code{step} defines the number of units on the
#' z-scale that shares a given colour; \code{range} can be used to constrain the
#' minimum and/or maximum values to be drawn (e.g. range = c(0, 1)) (this latter
#' argument is usefull if one wants to create several plots with the same
#' z-scale); \code{n.labels} allows for the user to approximatively define the
#' maximum number of numbers plotted on the z-scale; \code{digits} defines the number
#' of digits displayed for the numbers used as labels; and \code{fn} is used to
#' specify the function that is used to sample the colours. If \code{fn} is NULL
#' (default) the palette functions derived from \code{\link{isopalette1}} and
#' \code{\link{isopalette2}} are used when ploting isoscape and assignments,
#' respectivelly.
#' 
#' When called upon an object of class \var{RasterLayer}, the plot function is 
#' a simple shortcut to \code{\link[rasterVis:levelplot-methods]{levelplot}}.
#'
#' @name plots
#' @aliases plot.isofit plot.isoscape plot.calibfit plot.isorix plot
#' @param x The return object of an \code{\link{isofit}},
#'   \code{\link{isoscape}}, \code{\link{calibfit}}, \code{\link{isofind}}, or \code{\link[raster:raster]{raster}}
#'   call
#' @param cex.scale A \var{numeric} giving a scalling factor for the points in
#'   the plots
#' @param which A \var{string} indicating the name of the raster to be plotted
#'   (see details)
#' @param who Either "group", or a vector of indices (e.g. 1:3) or names of the
#'   individuals (e.g. c("Mbe_1", "Mbe_3")) to be considered in assignment plots
#' @param what A \var{string} indicating the name of the raster to be plotted
#'   (should remain "pv" if who = "group", otherwise could be "stat", "stat.var",
#'   or "pv")
#' @param cutoff A \var{list} containing information for the display of the
#'   region outside the prediction interval (see details)
#' @param sources A \var{list} containing information for the display of the
#'   location of the sources (see details)
#' @param calib A \var{list} containing information for the display of the
#'   location of the calibration sampling area (see details)
#' @param borders A \var{list} containing information for the display of borders
#'   (e.g. country borders) (see details)
#' @param mask A \var{list} containing information for the display of a mask
#'   (e.g. an ocean mask) (see details)
#' @param mask2 A \var{list} containing information for the display of a mask
#'   (e.g. a distribution mask) (see details)
#' @param palette A \var{list} containing information for the display of the
#'   colours for the isoscape (see details)
#' @param plot A \var{logical} indicating whether the plot shall be plotted or
#'   just returned
#' @param sphere A \var{list} containing information wether the raster should be 
#'   returned as a rotating sphere and if the image created during the process 
#'   should be saved in your current working directory. The default settings are FALSE.
#' @param ... Additional arguments (not in use)
#'
#' @seealso \code{\link{isofit}} for the function fitting the isoscape
#'
#'   \code{\link{isoscape}} for the function building the isoscape
#'
#'   \code{\link{calibfit}} for the function fitting the calibration function
#'
#'   \code{\link{isofind}} for the function performing the assignment
#'
#'   \code{\link{IsoriX}} for the complete work-flow
#' @keywords plot
#' @examples ## See ?isoscape or ?isofind for examples
#'
NULL

#' @rdname plots
#' @method plot isoscape
#' @export
plot.isoscape <- function(x,
                          which   = "mean",
                          sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
                          borders = list(borders = NA, lwd = 0.5, col = "black"),
                          mask    = list(mask = NA, lwd = 0, col = "black", fill = "black"),
                          palette = list(step = NA, range = c(NA, NA), n.labels = 11, digits = 2, fn = NA),
                          plot    = TRUE,
                          sphere  = list(build = FALSE, keep.image = FALSE),
                          ... ## we cannot remove the dots because of the S3 export...
                          ) {
    if (!(any(class(x) %in% "isoscape"))) {
      stop("This function must be called on an object of class isoscape.")
    }
  
    simu <- "isosim" %in% class(x)

    ## complete input with default setting
    .CompleteArgs(plot.isoscape)

    ## importing palette if missing
    if (!is.null(palette$fn) && !is.function(palette$fn) && is.na(palette$fn)) {
      isopalette1 <- NULL ## to please R CMD check
      utils::data("isopalette1", envir = environment(), package = "IsoriX")
      palette$fn <- grDevices::colorRampPalette(isopalette1, bias = 1)
    }

    ## importing country borders if missing
    if (!is.null(borders$borders) && is.na(borders$borders)) {
      CountryBorders <- NULL
      utils::data("CountryBorders", envir = environment(), package = "IsoriX")
      borders$borders <- CountryBorders
    }

    ## importing ocean if missing
    if (!is.null(mask$mask) && class(mask$mask) != "SpatialPolygons" && is.na(mask$mask)) {
      OceanMask <- NULL
      utils::data("OceanMask", envir = environment(), package = "IsoriX")
      mask$mask <- OceanMask
    }

    if (simu) {
      if (sources$draw) {
        sources$draw <- FALSE
        message("you asked to plot sources, but it does not make sense for simulations as each raster cell is a source. The argument 'plot.sources' was thus considered to be FALSE")
      }
      if (!(which %in% c("mean", "disp"))) {
        stop("for simulated data, the argument 'which' must be 'mean' or 'disp'")
      }
    } else {
      if (!(which %in% c("mean", "mean.predVar", "mean.residVar", "mean.respVar",
                       "disp", "disp.predVar", "disp.residVar", "disp.respVar"))) {
      stop("argument 'which' unknown")
      }
    }

    ## compute the colors
    colours <- .cutandcolor(var     = x$isoscape[[which]]@data@values,
                           step     = palette$step,
                           range    = palette$range,
                           palette  = palette$fn,
                           n.labels = palette$n.labels,
                           digits   = palette$digits)

    ## define y.title
    simu.title <- ""
    if (simu) simu.title <- "simulated"
    ## create the levelplot
    ##  note the use of bquote() which contrary to expression(paste())
    ##  allows for the evaluation of arguments.
    ##  (the stars are used to remove spaces)
    map <- rasterVis::levelplot(x$isoscape[[which]],
                                maxpixels = 4e6,
                                margin = FALSE,
                                col.regions = colours$all.cols,
                                at = colours$at,
                                colorkey = list(labels = list(at = colours$at.keys, labels = colours$at.labels)),
                                main = bquote(.(simu.title)~.(sub(".", " ", which, fixed = TRUE))~delta*D[p]))

    ## create the additional plot(s)
    decor <- .BuildAdditionalLayers(x = x,
                                    sources = sources,
                                    calib = NULL,
                                    borders = borders,
                                    mask = mask)


    complete.map <- map + decor$borders.layer + decor$mask.layer + decor$sources.layer

    ## plotting
    if (plot & !sphere$build) {
      ## check if prompt must appear in examples
      if (.IsoriX.data$options$dont_ask) {
        options(example.ask = "FALSE") ## only effective for the next example run, not the current one...
      }
      ## send plot to graphic device
      print(complete.map)
    }

    ## tweak to please codetools::checkUsagePackage('IsoriX', skipWith = TRUE)
    rm(simu.title)

    ## build the 3D-Sphere
    if (sphere$build) {
      .build_sphere(x$isoscape[[which]], colours = colours, decor = decor)
      if (!sphere$keep.image) {
        file.remove("world_image.png")
      }
    }
    
    return(invisible(complete.map))

}

.build_sphere <- function(x, colours, decor) {
  ## we are indebted to Deepayan Sarkar for helping use with the following lattice code
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("the package 'rgl' is needed for this function,
    you can install it by typing install.packages('rgl')")
  }
  
  if (interactive()) {
    print("Building the sphere...", quote = FALSE)
    print("(this may take a few seconds)", quote = FALSE)
  }
  
  ### check extent of the raster and extend to world if necessary
  world.raster <- raster::raster()
  raster::extent(world.raster) <- c(-180, 180, -90, 90)
  if (raster::extent(x) < raster::extent(world.raster)) {
    x <- raster::extend(x, raster::extent(world.raster))
  }

  p <- rasterVis::levelplot(x, col.regions = colours$all.cols,
                            at = colours$at,
                            colorkey = FALSE) + decor$borders.layer + 
    decor$mask.layer + decor$mask2.layer + decor$sources.layer + decor$calib.layer
  grDevices::png(filename = "world_image.png", width = 3000, height = 2000)
  print(p)
  pargs <- lattice::trellis.panelArgs(p, 1)
  lims <- do.call(lattice::prepanel.default.levelplot, pargs)
  
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = grDevices::extendrange(lims$xlim, f = 0.07),
                                    yscale = grDevices::extendrange(lims$ylim, f = 0.07)))
  do.call(p$panel, pargs) #do.call(lattice::panel.levelplot, pargs)
  grDevices::dev.off()
  
  if (length(rgl::rgl.dev.list()) > 0) rgl::rgl.close() ## close all open devices
  rgl.sphere <- function(x, y = NULL, z = NULL, ng = 50, radius = 1, color = "white", add = F, ...) {
    ## code inspired from https://stackoverflow.com/questions/30627647/how-to-plot-a-perfectly-round-sphere-in-r-rgl-spheres
    lat <- matrix(seq(90, -90, len = ng)*pi/180, ng, ng, byrow = TRUE)
    long <- matrix(seq(-180, 180, len = ng)*pi/180, ng, ng)
    xyz <- grDevices::xyz.coords(x, y, z, recycle = TRUE)
    vertex <- matrix(rbind(xyz$x, xyz$y, xyz$z), nrow = 3, dimnames = list(c("x", "y", "z"), NULL))
    nvertex <- ncol(vertex)
    radius  <- rbind(vertex, rep(radius, length.out = nvertex))[4,]
    color  <- rbind(vertex, rep(color, length.out = nvertex))[4,]
    
    for (i in 1:nvertex) {
      add2 <- if (!add) i > 1 else T
      x <- vertex[1,i] + radius[i]*cos(lat)*cos(long)
      y <- vertex[2,i] + radius[i]*cos(lat)*sin(long)
      z <- vertex[3,i] + radius[i]*sin(lat)
      rgl::persp3d(x, y, z, add = add2, color = color[i], axes = FALSE,
                   xlab = "", ylab = "", zlab = "", ...)
    }
  }
  
  rgl::par3d("windowRect" = c(0, 0, 500, 500))
  rgl::bg3d(sphere = TRUE, color = "darkgrey", lit = FALSE)
  rgl.sphere(0, texture = "world_image.png", lit = FALSE, color = "white") ## or rgl::rgl.spheres()
}

#' @rdname plots
#' @method plot isorix
#' @export
plot.isorix <- function(x,
                        who     = "group",
                        what    = "pv",
                        cutoff  = list(draw = TRUE, level = 0.05, col = "#909090"),
                        sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
                        calib   = list(draw = TRUE, cex = 0.5, pch = 4, lwd = 1, col = "blue"),
                        borders = list(borders = NA, lwd = 0.5, col = "black"),
                        mask    = list(mask = NA, lwd = 0, col = "black", fill = "black"),
                        mask2   = list(mask = NA, lwd = 0, col = "purple", fill = "purple"),
                        palette = list(step = NA, range = c(0, 1), n.labels = 11, digits = 2, fn = NA),
                        plot    = TRUE,
                        sphere  = list(build = FALSE, keep.image = FALSE),
                        ... ## we cannot remove the dots because of the S3 export...
                        ) {
  
  if (!(any(class(x) %in% "isorix"))) {
    stop("This function must be called on an object of class isorix.")
  }
  
  ## complete input with default setting
  .CompleteArgs(plot.isorix)

  ## importing palette if missing
  if (!is.null(palette$fn) && !is.function(palette$fn) && is.na(palette$fn)) {
    isopalette2 <- NULL ## to please R CMD check
    utils::data("isopalette2", envir = environment(), package = "IsoriX")
    palette$fn <- grDevices::colorRampPalette(isopalette2, bias = 0.75)
  }

  ## importing country borders if missing
  if (!is.null(borders$borders) && is.na(borders$borders)) {
    CountryBorders <- NULL
    utils::data("CountryBorders", envir = environment(), package = "IsoriX")
    borders$borders <- CountryBorders
  }

  ## importing ocean if missing
  if (!is.null(mask$mask) && class(mask$mask) != "SpatialPolygons" && is.na(mask$mask)) {
    OceanMask <- NULL
    utils::data("OceanMask", envir = environment(), package = "IsoriX")
    mask$mask <- OceanMask
  }

  ## changing missing setting for mask2
  if (!is.null(mask2$mask) && class(mask2$mask) != "SpatialPolygons" && is.na(mask2$mask)) {
    mask2$mask <- NULL
  }

  ## changing cutoff level to null when we don't want to draw the cutoff
  if (what != "pv" | !cutoff$draw) {
    cutoff$level <- 0
  }

  ## create the main plot(s)
  if ("group" %in% who) {
    colours <- .cutandcolor(var        = x$group$pv, #@data@values, removed
                            step       = palette$step,
                            range      = palette$range,
                            palette    = palette$fn,
                            cutoff     = cutoff$level,
                            col.cutoff = cutoff$col,
                            n.labels   = palette$n.labels,
                            digits     = palette$digits)

    map <- rasterVis::levelplot(x$group$pv, # x$group$pv * (x$group$pv > cutoff$level)
                                maxpixels = 4e6,
                                margin = FALSE,
                                col.regions = colours$all.cols,
                                at = colours$at,
                                colorkey = list(labels = list(at = colours$at.keys, labels = colours$at.labels)),
                                main = "Group assignment"
                                )
  } else {
    main.title <- if (length(who) == 1) {
      names(x$indiv[[what]][[who]])
    } else {
      NULL
    }

    colours <- .cutandcolor(var        = x$indiv[[what]][[who]], #@data@values removed: does not work if raster on hard drive...
                            step       = palette$step,
                            range      = palette$range,
                            palette    = palette$fn,
                            cutoff     = cutoff$level,
                            col.cutoff = cutoff$col,
                            n.labels   = palette$n.labels,
                            digits     = palette$digits)

    map <- rasterVis::levelplot(x$indiv[[what]][[who]], #x$indiv[[what]][[who]] * (x$indiv$pv[[who]] > cutoff$level)
                                maxpixels = 4e6,
                                margin = FALSE,
                                col.regions = colours$all.cols,
                                at = colours$at,
                                colorkey = list(labels = list(at = colours$at.keys, labels = colours$at.labels)),
                                main = main.title)
  }

  ## create the additional plot(s)
  decor <- .BuildAdditionalLayers(x = x,
                                  sources = sources,
                                  calib = calib,
                                  borders = borders,
                                  mask = mask,
                                  mask2 = mask2
                                  )

  ## changing the colour below the threshold
  if (cutoff$level > 0) {
    index <- 1:max(which(map$legend$right$args$key$at < cutoff$level))
    map$legend$right$args$key$col[index] <- cutoff$col
  }

  ## we add the legend for the side bar
  ## (thanks to Deepayan Sarkar, for solving a device opening hicup at this stage)
  map$legend$right <- list(fun = latticeExtra::mergedTrellisLegendGrob,
                           args = list(map$legend$right,
                                       list(fun = grid::textGrob,
                                            args = list(label = "P-value", rot = 90)
                                            ),
                                       vertical = FALSE
                                       )
                           )

  ## pilling all layers together
  complete.map <- map + decor$borders.layer + decor$mask.layer + decor$mask2.layer +
    decor$sources.layer + decor$calib.layer

  ## plotting
  if (plot & !sphere$build) {
    ## check if prompt must appear in examples
    if (.IsoriX.data$options$dont_ask) {
      options(example.ask = "FALSE") ## only effective for the next example run, not the current one...
    }
    ## send plot to graphic device
    print(complete.map)
  }
  
  ## build the 3D-Sphere
  if (sphere$build) {
    .build_sphere(x[[who]][[what]], colours = colours, decor = decor)
    if (!sphere$keep.image) {
      file.remove("world_image.png")
    }
  }

  return(invisible(complete.map))

}


.cutandcolor <- function(var,
                         step = NA,
                         range = NA,
                         palette = NULL,
                         cutoff = NA,
                         col.cutoff = "#909090",
                         n.labels = 99,
                         digits = 2) {
  
  var <- .summarizevalues(var) ## converting rasters info into numerics if needed
  max_var <- max(var, na.rm = TRUE)
  min_var <- min(var, na.rm = TRUE)

  if (is.na(n.labels)) {
    warning("The argument n.labels of the palette was changed to 10 because it was not defined!")
    n.labels <- 10
  }
  if (length(range) == 1) {
    range <- c(NA, NA)
  }
  if (is.na(range)[1]) {
    range[1] <- min_var
  }
  if (is.na(range)[2]) {
    hard.top <- FALSE
    range[2] <- max_var
  } else {
    hard.top <- TRUE
  }
  if (is.na(step)) {
    step <- (max(range, na.rm = TRUE) - min(range, na.rm = TRUE)) / (n.labels - 1)
  }
  where.cut <- seq(min(range, na.rm = TRUE), (max(range, na.rm = TRUE)), step)
  if ((max(where.cut) < max_var) & !hard.top) {
    where.cut <- c(where.cut, max(where.cut) + step)
    n.labels <- n.labels + 1
  }
  if ((min_var < min(where.cut)) || (max_var > max(where.cut))) {
    warning(paste0("Range for palette too small! It should be at least: [",
                   min_var, "-", max_var, "]"))
  }
  if (!is.na(cutoff)) {
    where.cut <- sort(unique(c(cutoff, where.cut)))
  }
  if (length(unique(var)) > 1) { 
    cats <- cut(var, where.cut, ordered_result = TRUE)  ## also works on non raster
  } else { ## case if no variation
    cats <- as.factor(where.cut)
    where.cut <- c(where.cut, where.cut + 1e-10)
    warning("There is no variation in the map!")
  }
  if (is.null(palette)) {
    palette <- viridisLite::viridis
  }
  all.cols <- do.call(palette, list(n = length(levels(cats))))
  if (!is.na(cutoff)) {
    all.cols[1:(which(where.cut == cutoff) - 1)] <- col.cutoff
  }
  cols <- all.cols[match(cats, levels(cats))]
  at.keys <- where.cut
  if (length(at.keys) > n.labels) {
    at.keys <- seq(min(where.cut), max(where.cut), length = n.labels)
    if (!is.na(cutoff)) {
      at.keys <- sort(unique(c(cutoff, at.keys)))
    }
  }
  if (sum(at.keys %% 1) == 0) {
    digits <- 0
  }
  at.labels <-  formatC(round(at.keys, digits = digits), digits = digits, format = "f")
  return(list(cols = cols, at = where.cut, all.cols = all.cols, at.keys = at.keys, at.labels = at.labels))
}

#' @rdname plots
#' @method plot isofit
#' @export
plot.isofit <- function(x, cex.scale = 0.2, ...) {

  if (!(any(class(x) %in% "isofit"))) {
    stop("This function must be called on an object of class isofit.")
  }
  
  ## Test if RStudio is in use
  RStudio <- .Platform$GUI == "RStudio"

  if (!any(class(x) %in% "multiisofit")) {
    ## Determine number of plots in panel
    if (RStudio) {
      nplot <- 1
    } else {
      nplot <- 2 + x$info.fit$disp.model.rand$spatial +
        x$info.fit$mean.model.rand$spatial
    }

    ## Define mfrow (number of rows and column in panel)
    mfrow <- switch(as.character(nplot),
                    "1" = c(1, 1),
                    "2" = c(1, 2),
                    "3" = c(1, 3),
                    "4" = c(2, 2),
                    stop("nplot value not anticipated")
    )

    ## Setup the graphic device
    graphics::par(mfrow = mfrow)

    ## Plots from spaMM
    spaMM::plot.HLfit(x$mean.fit,
                      "predict",
                      cex = 0.1 + cex.scale*log(x$mean.fit$data$weights.mean),
                      las = 1, ...
    )
    graphics::title(main = "Pred vs Obs in mean.fit")
    .HitReturn()

    spaMM::plot.HLfit(x$disp.fit,
                      "predict",
                      cex = 0.1 + cex.scale*log(x$disp.fit$data$weights.disp),
                      las = 1, ...
    )
    graphics::title(main = "Pred vs Obs in disp.fit")

    ## Plot Matern autocorrelation
    if (x$info.fit$mean.model.rand$spatial) {
      .HitReturn()
      .PlotMatern(x$mean.fit, ...)
      graphics::title(main = "Autocorrelation in mean.fit")
    }

    if (x$info.fit$disp.model.rand$spatial) {
      .HitReturn()
      .PlotMatern(x$disp.fit, ...)
      graphics::title(main = "Autocorrelation in disp.fit")
    }

    ## Reset the graphic device
    graphics::par(mfrow = c(1, 1))
  } else {
    for (fit in 1:length(x$multi.fits)) {
      cat("\n")
      cat(paste("##### Plots for pair of models", names(x$multi.fits)[fit]), "#####")
      cat("\n")
      graphics::plot(x$multi.fits[[fit]])
    }
  }
  return(invisible(NULL))
}


.PlotMatern <- function(model, limit = 0.5, ...) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It plots the Matern autocorrelation.
  d.stop <- FALSE
  d <- 0

  while ((d < 50000) & !d.stop) {
    d <- d + 10
    m <- spaMM::MaternCorr(d = d,
                           rho = model$corrPars$rho,
                           nu = model$corrPars$nu
    )
    if (m < limit) d.stop <- TRUE
  }

  distances <- seq(0, d, 1)

  m <- spaMM::MaternCorr(d = distances,
                         rho = model$corrPars$rho,
                         nu = model$corrPars$nu
  )

  graphics::plot(m ~ distances,
                 type = "l",
                 las = 1,
                 xlab = "Distances (km)",
                 ylab = "Correlation",
                 ...
  )
}



#' @rdname plots
#' @method plot calibfit
#' @export
plot.calibfit <- function(x, ...) {
  xs <- with(x$calib.data,
             seq(min(mean.iso),
                 max(mean.iso),
                 length = 100
             )
  )
  
  if (!(any(class(x) %in% "calibfit"))) {
    stop("This function must be called on an object of class calibfit.")
  }
  

  X <- cbind(1, xs)
  fitted <- X %*% x$param
  fixedVar <- rowSums(X * (X %*% x$fixefCov)) ## = diag(X %*% x$fixefCov %*% t(X))

  with(x$calib.data,
       graphics::plot(tissue.value ~ mean.iso,
                      xlab = "Isotopic value in the environment",
                      ylab = "Isotopic value in the organisms",
                      las = 1,
                      ...
       )
  )

  graphics::points(fitted ~ xs, type = "l", col = "blue", lwd = 2)
  graphics::points(fitted + stats::qnorm(0.025)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")
  graphics::points(fitted + stats::qnorm(0.975)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")

  ## tweak to please codetools::checkUsagePackage('IsoriX', skipWith = TRUE)
  rm(fitted, fixedVar)

  return(invisible(NULL))
}

#' @rdname plots
#' @method plot RasterLayer
#' @export
plot.RasterLayer <- function(x, ...) {
  print(rasterVis::levelplot(x, margin = FALSE, ...))
  return(invisible(NULL))
}
