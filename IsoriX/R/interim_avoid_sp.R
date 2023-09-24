# Here is a set of functions allowing one to draw points, lines and polygons on top of SpatRaster objects.
# These functions should be absorbed by lattice and rasterVis in the future.
# See: 
#   - https://github.com/oscarperpinan/rastervis/issues/101
#   - https://github.com/deepayan/lattice/issues/29
#   - https://github.com/deepayan/lattice/pull/30


lpoints.SpatVector <- function(x, ...) {
  if (is.null(x)) return(NULL)
  xy <- terra::crds(x, df = TRUE)
  lattice::lpoints(xy, ...)
}

llines.SpatVector <- function(x, ...) {
  if (is.null(x)) return(NULL)
  xy <- terra::crds(x, list = TRUE)
  names(xy) <- c("x", "y")
  lattice::llines(xy, ...)
}

lpolygon.SpatVector <- function(x, ...) {
  if (is.null(x)) return(NULL)
  xy <- terra::crds(x, list = TRUE)
  for (i in seq_along(xy)) {
    for (j in seq_along(xy[[i]])) {
      lattice::lpolygon(x = xy[[i]][[j]][[1]], xy[[i]][[j]][[2]], ...)
    }
  }
}

## we turn the original function lpolygon() from lattice into a generic
lpolygon <- function(x, ...) UseMethod("lpolygon")

## the original function lpolygon() from lattice becomes the default method lattice::lpolygon()
lpolygon.default <- function(x,
                             y = NULL,
                             border = "black",
                             col = "transparent",
                             fill = NULL,
                             font,
                             fontface,
                             ...,
                             identifier = NULL,
                             name.type = "panel") {
  if (sum(!is.na(x)) < 1)
    return()
  border <-
    if (all(is.na(border)))
      "transparent"
  else if (is.logical(border))
  {
    if (border)
      "black"
    else
      "transparent"
  }
  else
    border
  xy <- grDevices::xy.coords(x, y, recycle = TRUE)
  if (hasGroupNumber())
    group <- list(...)$group.number
  else
    group <- 0
  n <- length(xy$x)
  w <- which(is.na(xy$x) | is.na(xy$y))
  id.lengths <- diff(c(0, w, n))
  grid::grid.polygon(
    x = xy$x,
    y = xy$y,
    id.lengths = id.lengths,
    default.units = "native",
    name = primName("polygon", identifier, name.type, group),
    gp = gpar(fill = col, col = border, ...)
  )
}

## copied from lattice with no alteration
primName <- function(name, identifier = NULL, name.type = "panel", group = 0) {
  lattice::trellis.grobname(name = ifelse(is.null(identifier), name,
                            paste(identifier, name, sep = ".")),
                            type = name.type,
                            group = group)
}

## copied from lattice with no alteration
hasGroupNumber <- function() {
  aname <- "group.number"
  fnames <- names(formals(sys.function(sys.parent())))
  if (is.na(match(aname, fnames))) {
    if (is.na(match("...", fnames)))
      FALSE
    else {
      dotsCall <- eval(quote(substitute(list(...))), sys.parent())
      !is.na(match(aname, names(dotsCall)))
    }
  }
  else FALSE
}
