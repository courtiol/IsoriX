\name{elevraster}
\alias{elevraster}

\docType{data}
\title{The raster of world elevation}

\description{This raster contains the elevation of the world surface [meters above sea level] with a resolution of approximately 100 square-km.}

\usage{data(elevraster)}

\format{
A \var{RasterLayer}
}

\details{
This raster contains elevation data of the world in a highly aggregated form corresponding to a resolution of approximately one elevation value per 100 square-km. This is only for the purpose of having a small and easy-to-handle file to practice, but it should not be used to perform real assignments!

In the example below, we show how we generated this small raster from a large original \var{DEM} (digital elevation model). The original raster has a resolution of approximately one elevation value per square-km (cell size of 30 arcseconds, i.e. 0.0083 decimal degrees). Although working on large rasters is technically problematic (memory and CPU greedy), we highly recommend to rely on high-resolution rasters with small to moderate aggregation levels in order to perform reliable assignment analyses. Indeed, large aggregation of raster cells can bias assignments due to the transformation of all elevations into a single value per aggregated raster cell.

We downloaded "Global Multi-resolution Terrain Elevation Data 2010" from: \cr \url{http://topotools.cr.usgs.gov/gmted_viewer/} \cr and converted it into a \var{tif} file. Because the original file is very large, we directly provide the url link of the \var{tif} file in the example below.
}

\source{\url{http://topotools.cr.usgs.gov/gmted_viewer/}}

\seealso{
\code{\link{relevate}} to crop and/or aggregate the elevation raster

}

\examples{

## The following example require to have downloaded
## a large elevation raster with the function getelev()
## and will therefore not run unless you type:
## example(elevraster, run.dontrun=TRUE)

\dontrun{
### CREATING THE OBJECT elevraster

## download the tif file (ca. 700 Mb):
## (see ?getelev for details on how to get the tif file)
# getelev()

## convert the tif into R raster format:
library(raster)
elevationrasterbig <- raster("gmted2010_30mn.tif")

## create the highly agregated elevation raster
## (90 sec on one of our computers):
elevraster <- relevate(
    elevationrasterbig,
    aggregation.factor=100)
}

}

\keyword{datasets}

