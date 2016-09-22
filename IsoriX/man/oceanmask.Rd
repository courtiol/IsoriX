\name{oceanmask}
\alias{oceanmask}

\docType{data}
\title{Mask of world oceans}

\description{This dataset contains a polygon shapefile that can be used to mask large bodies of water.}

\usage{data(oceanmask)}

\format{
A \var{SpatialPolygons} object
}

\source{
This \var{SpatialPolygons} is derived from the \code{\link{countries}}.
See example for details on how we created the dataset.
}

\seealso{
\code{\link{countries}} for another polygon shapefile used to embellish the plots

}

\examples{
data(oceanmask)
if(require(sp))
    plot(oceanmask, col='blue')

## HOW DID WE CREATE THIS FILE?
## (This example takes some time and will therefore not be run
##  unless you type: example(oceanmask, run.dontrun=TRUE) )
  
\dontrun{
if(require(raster) & require(rgeos)){
    worldlimit <- as(extent(countries), "SpatialPolygons")
    proj4string(worldlimit) <- crs(countries)
    oceanmask <- gDifference(worldlimit, countries)  
    oceanmask
    ## uncomment the following to store the file:
    #save(oceanmask, file="oceanmask.rda", compress="xz")
}
}

}

\keyword{datasets}

