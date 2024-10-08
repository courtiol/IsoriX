% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepcipitate.R
\name{prepcipitate}
\alias{prepcipitate}
\title{Prepare the raster brick containing the precipitation data}
\usage{
prepcipitate(path = NULL, raster, verbose = interactive())
}
\arguments{
\item{path}{A \emph{string} indicating the path where the WorldClim data have
been downloaded. If the path is null (the default) the function will assume
that the folder containing the precipitation data is in the current
directory}

\item{raster}{A \emph{raster} containing the structural raster}

\item{verbose}{A \emph{logical} indicating whether information about the
progress of the procedure should be displayed or not while the function is
running. By default verbose is \code{TRUE} if users use an interactive R
session, and \code{FALSE} otherwise.}
}
\description{
This functions turns the WorldClim data downloaded using the function
\code{\link{getprecip}} into a \emph{SpatRaster} of same resolution and
extent as the structural raster. This function is designed to be used with
\code{\link{isomultiscape}}.
}
\examples{

## The following example takes some time and download a large amount of data (~ 1 Gb).
## It will therefore not be run unless you uncomment it

### We fit the models for Germany:
# GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#
# GermanFit <- isofit(data = GNIPDataDEagg,
#                     mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
#
### We prepare the structural raster:
# StrRaster <- prepraster(raster = ElevRasterDE,
#                         isofit = GermanFit,
#                         aggregation_factor = 0)
#
### We download the precipitation data:
# temp_folder <- tempdir()
# getprecip(path = temp_folder)
#
### We prepare the raster brick with all the precipitation data:
# PrecipitationBrick <- prepcipitate(path = temp_folder,
#                                    raster = StrRaster)
#
### We plot the precipitation data:
# levelplot(PrecipitationBrick)

}
\seealso{
\code{\link{getprecip}} to download the relevant precipitation data

\code{\link{PrecipBrickDE}} for the stored precipitation data for Germany

\code{\link{prepelev}} to prepare an elevation raster
}
