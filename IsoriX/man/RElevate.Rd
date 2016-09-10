\name{RElevate}
\alias{RElevate}

\title{Prepare the elevation raster}

\description{This function prepares the elevation raster for the follow-up analyses. The size and extent of the elevation raster defines the resolution at which the isoscape and the origin assignment are defined.}

\usage{
RElevate(
    elevation.raster,
    isofit=NULL,
    aggregation.factor=0L,
    aggregation.fun=mean,
    manual.crop=NULL,
    verbose=interactive()
)
}

\arguments{
    \item{elevation.raster}{The elevation raster (\var{RasterLayer})}
    \item{isofit}{The fitted isoscape model returned by the function \code{\link{Isofit}}}
    \item{aggregation.factor}{The number of neighbouring cells (\var{integer}) to merge during aggregation}
    \item{aggregation.fun}{The \var{function} used to aggregate cells}
    \item{manual.crop}{A vector of four coordinates (\var{numeric}) for manual cropping, e.g. the spatial extent}
    \item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \var{TRUE} if users use an interactive R session, and \var{FALSE} otherwise.}
}

\value{The fine-tuned elevation raster of class \var{RasterLayer}}

\details{
This functions allows the user to crop an elevation raster according to either the extent of the isoscape or manually. If a fitted isoscape object is provided (see \code{\link{Isofit}}), the function extracts the observed locations of isotopic sources from the model object and crops the elevation raster accordingly. Alternatively, \code{manual.crop} allows you to crop the elevation raster to a desired extent. If no model and no coordinates for manual cropping are provided, no crop will be performed. Importantly, cropping is recommended as it prevents extrapolations outside the latitude/longitude range of the source data. Predicting outside the range of the source data may lead to highly unreliable predictions.

Aggregation changes the spatial resolution of the raster, making computation faster and using less memory (this can affect the assignment; see note below). An aggregation factor of zero (or one) keeps the resolution constant (default).
}

\note{Aggregating the raster may lead to different results for the assignment, because the elevation of raster cells changes depending on the aggregation function (see example below), which in turn affects model predictions.}

\seealso{
\code{\link{elevationrastersmall}} for information on elevation rasters

\code{\link{IsoriX}} for the complete workflow
}


\examples{

### DIFFERENCES IN AGGREGATION

## Loading objects
data(elevationrastersmall)
data(GNIP_Model)
data(worldcountries)
data(oceanmask)

## We aggregate and crop using different settings
elevation.raster1 <- RElevate(
    elevation.raster=elevationrastersmall,
    isofit=GNIP_Model,
    aggregation.factor=0)

elevation.raster2 <- RElevate(
    elevation.raster=elevationrastersmall,
    isofit=GNIP_Model,
    aggregation.factor=5)

elevation.raster3 <- RElevate(
    elevation.raster=elevationrastersmall,
    isofit=GNIP_Model,
    aggregation.factor=5, aggregation.fun=max)


## The following plot takes a few second too many and will 
## therefore not be run unless you type: 
## example(RElevate, run.dontrun=TRUE)

\dontrun{
    ## We plot the outcome of the 3 different aggregation schemes
if(require(rasterVis)) {
    data(worldcountries)
    data(oceanmask)
    plot.aggregation1 <- levelplot(elevation.raster1,
            margin=FALSE, main="Original small raster") + 
        layer(sp.polygons(worldcountries)) +
        layer(sp.polygons(oceanmask, fill="blue"))
    plot.aggregation2 <- levelplot(elevation.raster2,
            margin=FALSE, main="Small raster aggregated (by mean)") + 
        layer(sp.polygons(worldcountries)) +
        layer(sp.polygons(oceanmask, fill="blue"))
    plot.aggregation3 <- levelplot(elevation.raster3,
            margin=FALSE, main="Small raster aggregated (by max)") + 
        layer(sp.polygons(worldcountries)) +
        layer(sp.polygons(oceanmask, fill="blue"))  
    ## panel using lattice syntax:
    print(plot.aggregation1, split=c(1, 1, 1, 3), more=TRUE)
    print(plot.aggregation2, split=c(1, 2, 1, 3), more=TRUE)
    print(plot.aggregation3, split=c(1, 3, 1, 3))
}
}


}

\keyword{utilities}
