\name{plot.methods}
\alias{plot.isofit}
\alias{plot.isoscape}
\alias{plot.calibfit}
\alias{plot.isorix}
\alias{plot}

\title{Plotting functions for IsoriX}

\description{These functions plot objects created by \pkg{\link{IsoriX}}.}

\usage{

\method{plot}{isofit}(
    x,
    cex.scale=0.2,
    ...
)

\method{plot}{calibfit}(
    x,
    ...
)

\method{plot}{isoscape}(
    x,
    which="mean",
    sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
    borders=list(borders=NULL, lwd=0.5, col="black"),
    mask=   list(mask=NULL, col="black"),
    palette=terrain.colors(20),
    plot=TRUE,
    ... 
)

\method{plot}{isorix}(
    x,
    who="group",
    what="pv",
    cutoff= list(draw=TRUE, level=0.05, col="#909090"),
    sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
    calib=  list(draw=TRUE, cex=0.5, pch=4, lwd=1, col="blue"),
    borders=list(borders=NULL, lwd=0.5, col="black"),
    mask=   list(mask=NULL, col="black"),
    palette=rev(terrain.colors(100)),
    plot=TRUE,
    ...
)

}

\arguments{
    \item{x}{The return object of an \code{\link{Isofit}}, \code{\link{Isoscape}}, \code{\link{Calibfit}}, or \code{\link{Isorix}} call}
    \item{cex.scale}{a \var{numeric} giving a scalling factor for the points in the plots}
    \item{which}{A \var{string} indicating the name of the raster to be plotted (see details)}
    \item{who}{Either "group", or a vector of indices (e.g. 1:3) or names of the individuals (e.g. c("Mbe_1", "Mbe_3")) to be considered in assignment plots}
    \item{what}{A \var{string} indicating the name of the raster to be plotted (should remain "pv" if who="group", otherwise could be "stat", "stat.var", or "pv")}
    \item{cutoff}{A \var{list} containing information for the display of the region outside the prediction interval (see details)}
    \item{sources}{A \var{list} containing information for the display of the location of the sources (see details)}
    \item{calib}{A \var{list} containing information for the display of the location of the calibration sampling area (see details)}
    \item{borders}{A \var{list} containing information for the display of borders (e.g. country borders) (see details)}
    \item{mask}{A \var{list} containing information for the display of a mask (e.g. an ocean mask) (see details)}
    \item{palette}{A vector of colours (\var{character})}
    \item{plot}{A \var{logical} indicating whether the plot shall be plotted or just returned}
    \item{...}{Additional arguments (not in use)}
}

\details{
When called upon an object of class \code{isofit}, the plot function draws diagnostic information for the fits of the isoscape geostatistical model.

When called upon an object of class \code{calibfit}, the plot function draws the fitted calibration function.

When called upon an object of class \code{isoscape}, the plot function draws a fine-tuned plot of the isoscape. When used on a fitted isoscape, the user can choose between plotting the  predictions (which = "mean"; default), the prediction variance (which = "mean.predVar"), the residual variance (which = "mean.residVar"), or the response variance (which = "mean.respVar") for the mean model; or the corresponding information for the residual dispersion variance model ("disp", "disp.predVar", "disp.residVar", or "disp.respVar"). When used on a simulated isoscape, the user can choose between plotting the mean isotopic value (which = "mean") or the dispersion (which = "disp").

When called upon an object of class \code{isorix}, the plot function draws a fine-tuned plot of the assignment. You can use the argument "who" to choose between plotting the assignment for the group or for some individuals (check the vignette "Workflow" for examples).

The arguments "cutoff", "sources", "calib", "borders" and "mask" are used to fine-tune additional layers that can be added to the main plot to embellish it. These arguments must be lists that provide details on how to draw, respectively, the area outside the prediction interval (for assignment plots), the locations of sources (for both isoscape and assignment plots), the locations of the calibration sampling area (for assignment plots, the borders (for both types of plots) and a mask (again, for both)). Within these lists, the element "col", "cex", "pch" and "lwd" influences their respective objects as in traditional R plotting functions (see \code{\link{par}} for details). The element "draw" should be a \var{logical} that indicates whether the layer must be created or not. The argument "borders" (within the list borders) expects an object of the class \var{SpatialPolygons} such as the object "worldcountries" provided with this package. The argument "mask" (within the list maks) expects an object of the class \var{SpatialPolygons} such as the object oceanmask provided with this package (see examples).

}

\seealso{
\code{\link{Isofit}} for the function fitting the isoscape

\code{\link{Isoscape}} for the function building the isoscape

\code{\link{Calibfit}} for the function fitting the calibration function

\code{\link{Isorix}} for the function performing the assignment

\code{\link{IsoriX}} for the complete work-flow
}

\examples{
## See ?Isoscape or ?Isorix for examples
}

\keyword{plot}
