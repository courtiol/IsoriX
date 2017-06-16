#' Plotting functions for IsoriX
#' 
#' These functions plot objects created by \pkg{\link{IsoriX}}.
#' 
#' When called upon an object of class \code{isofit}, the plot function
#' draws diagnostic information for the fits of the isoscape geostatistical
#' model.
#' 
#' When called upon an object of class \code{calibfit}, the plot function draws 
#' the fitted calibration function.
#' 
#' When called upon an object of class \code{isoscape}, the plot function draws 
#' a fine-tuned plot of the isoscape.
#' 
#' When used on a fitted isoscape, the user can choose between plotting the 
#' predictions (which = "mean"; default), the prediction variance (which =
#' "mean.predVar"), the residual variance (which = "mean.residVar"), or the
#' response variance (which = "mean.respVar") for the mean model; or the
#' corresponding information for the residual dispersion variance model ("disp",
#' "disp.predVar", "disp.residVar", or "disp.respVar").
#' 
#' When used on a simulated isoscape, the user can choose between plotting the
#' mean isotopic value (which = "mean") or the dispersion (which = "disp").
#' 
#' When called upon an object of class \code{isorix}, the plot function draws a
#' fine-tuned plot of the assignment. You can use the argument "who" to choose
#' between plotting the assignment for the group or for some individuals (check
#' the vignette "Workflow" for examples). The arguments "cutoff", "sources",
#' "calib", "borders", "mask" and "mask2" are used to fine-tune additional
#' layers that can be added to the main plot to embellish it. These arguments
#' must be lists that provide details on how to draw, respectively, the area
#' outside the prediction interval (for assignment plots), the locations of
#' sources (for both isoscape and assignment plots), the locations of the
#' calibration sampling area (for assignment plots, the borders (for both types
#' of plots) and a mask (again, for both)). For assignment maps, an extra mask
#' can be used (mask2), as one may want to add a mask covering the area outside
#' the biological range of the species. Within these lists, the element "lwd",
#' "col", "cex", "pch" and "fill" influences their respective objects as in
#' traditional R plotting functions (see \code{\link{par}} for details). The 
#' element "draw" should be a \var{logical} that indicates whether the layer 
#' must be created or not. The argument "borders" (within the list borders) 
#' expects an object of the class \var{SpatialPolygons} such as the object 
#' "countries" provided with this package. The argument "mask" (within the list 
#' maks) expects an object of the class \var{SpatialPolygons} such as the object
#' oceanmask provided with this package (see examples).
#' 
#' @name plots
#' @aliases plot.isofit plot.isoscape plot.calibfit plot.isorix plot
#' @param x The return object of an \code{\link{isofit}},
#'   \code{\link{isoscape}}, \code{\link{calibfit}}, or \code{\link{isofind}}
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
#' @param palette An optional vector of colours (\var{character})
#' @param plot A \var{logical} indicating whether the plot shall be plotted or
#'   just returned
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
                          borders  = list(borders = NULL, lwd = 0.5, col = "black"),
                          mask    = list(mask = NULL, lwd = 0, col = "black", fill = "black"),
                          palette = grDevices::terrain.colors(20),
                          plot    = TRUE,
                          ... ## we cannot remove the dots because of the S3 export...
                          ) {

    simu <- "isosim" %in% class(x)
    
    ## complete input with default setting
    .CompleteArgs(plot.isoscape)
    
    ## checking the inputs
    if (length(palette) < 2) {
      stop("wrong palette, more colours needed")
    }
    
    if (("isosim" %in% class(x))) {
      if (sources$draw) {
        sources$draw <- FALSE
        message("you asked to plot sources, but it does not make sense for simulations as each raster cell is a source. The argument 'plot.sources' was thus considered to be FALSE")
      }
      if (!(which %in% c("mean", "disp"))) {
        stop("for simulated data, the argument 'which' must be 'mean' or 'disp'")
      }
    }
    
    if (("isofit" %in% class(x)) & !(which %in% c("mean",
                                                  "mean.predVar", "mean.residVar", "mean.respVar",
                                                  "disp", "disp.predVar", "disp.residVar", "disp.respVar"
                                                  )
                                     )
        ) {
      stop("argument 'which' unknown")
    }
    
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
                                cuts = length(palette) - 1,
                                col.regions = palette,
                                main = bquote(.(simu.title)~.(sub(".", " ", which, fixed = TRUE))~delta*D[p])
                                )
    
    ## create the additional plot(s)
    decor <- .BuildAdditionalLayers(x = x,
                                    sources = sources,
                                    calib = NULL,
                                    borders = borders,
                                    mask = mask
                                    )
    
    
    complete.map <- map + decor$borders.layer + decor$mask.layer + decor$sources.layer
    
    ## send plot to graphic device
    if (plot) {
      print(complete.map)
    }
    
    ## tweak to please codetools::checkUsagePackage('IsoriX', skipWith = TRUE)
    rm(simu.title)
    
    return(invisible(complete.map))
    
  }

#' @rdname plots
#' @method plot isorix
#' @export
plot.isorix <- function(x,
                        who     = "group",
                        what    = "pv",
                        cutoff   = list(draw = TRUE, level = 0.05, col = "#909090"),
                        sources  = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
                        calib    = list(draw = TRUE, cex = 0.5, pch = 4, lwd = 1, col = "blue"),
                        borders = list(borders = NULL, lwd = 0.5, col = "black"),
                        mask    = list(mask = NULL, lwd = 0, col = "black", fill = "black"),
                        mask2   = list(mask = NULL, lwd = 0, col = "purple", fill = "purple"),
                        palette = rev(grDevices::terrain.colors(100)),
                        plot     = TRUE,
                        ... ## we cannot remove the dots because of the S3 export...
                        ) {

  ## complete input with default setting
  .CompleteArgs(plot.isorix)
  
  ## checking the inputs
  if (length(palette) < 2) {
    stop("wrong palette, more colors needed")  
  }
  
  ## adding colour for non assignment
  if (cutoff$level > 0) {
    palette <- c(cutoff$col, palette)
  }
  
  ## changing cutoff level to null when we don't want to draw the cutoff
  if (what != "pv" | !cutoff$draw) {
    cutoff$level <- 0
  }
  
  ## create the main plot(s)
  splits <- seq(0, 1, length = length(palette))

  if ("group" %in% who) {
    map <- rasterVis::levelplot(x$group$pv * (x$group$pv > cutoff$level),
                                maxpixels = 4e6,
                                margin = FALSE,
                                at = splits,
                                col.regions = palette,
                                main = "Group assignment"
                                )
  } else {
    main.title <- if (length(who) == 1) {
      names(x$indiv[[what]][[who]]) 
    } else {
      NULL
    }
    map <- rasterVis::levelplot(x$indiv[[what]][[who]] * (x$indiv$pv[[who]] > cutoff$level),
                                maxpixels = 4e6,
                                margin = FALSE,
                                at = splits,
                                col.regions = palette,
                                main = main.title
                                )
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

  if (plot) print(complete.map)
  
  return(invisible(complete.map))

}


.cutandcolor <- function(var, step, range = NA, palette = viridisLite::viridis) {
  where.cut <- seq(floor(min(var, na.rm = TRUE)),
                   ceiling(max(var, na.rm = TRUE)) + step, step)
  if (!any(is.na(range))) {
    where.cut <- seq(min(range), max(range), step)
    if ((min(var, na.rm = TRUE) < min(where.cut)) ||
        (max(var, na.rm = TRUE) > max(where.cut)))
      stop(
        paste0("Range too small! It should be at least: [",
               floor(min(var, na.rm = TRUE)), "-", ceiling(max(var, na.rm = TRUE)),
               "]")
      )
  }
  cats <- cut(var, where.cut, ordered_result = TRUE)
  all.cols <- do.call(palette, list(n = length(levels(cats))))
  cols <- all.cols[match(cats, levels(cats))]
  return(list(
    cols = cols,
    at = where.cut,
    all.cols = all.cols
  ))
}
