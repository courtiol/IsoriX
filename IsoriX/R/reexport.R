# reexport from sp:

#' @importFrom sp sp.polygons
#' @export
sp::sp.polygons

#' @importFrom sp sp.points
#' @export
sp::sp.points

#' @importFrom sp CRS
#' @export
sp::CRS


# reexport from raster:

#' @importFrom raster raster
#' @export
raster::raster

#' @importFrom raster values
#' @export
raster::values

#' @importFrom raster area
#' @export
raster::area

#' @importFrom raster extent
#' @export
raster::extent

#' @importFrom raster "extent<-"
#' @export
raster::`extent<-`

#' @importFrom raster "projection<-"
#' @export
raster::`projection<-`

#' @importFrom raster shift
#' @export
raster::shift



# reexport from rasterVis:

#' @importFrom rasterVis RdBuTheme
#' @export
rasterVis::RdBuTheme

#' @importFrom rasterVis levelplot
#' @export
rasterVis::levelplot



# reexport from grid:

#' @importFrom grid grid.text
#' @export
grid::grid.text



# reexport from lattice:

#' @importFrom lattice xyplot
#' @export
lattice::xyplot

#' @importFrom lattice panel.points
#' @export
lattice::panel.points



# reexport from latticeExtra:

#' @importFrom latticeExtra layer
#' @export
latticeExtra::layer
