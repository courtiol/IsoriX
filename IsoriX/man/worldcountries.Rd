\name{countries}
\alias{countries}

\docType{data}
\title{Borders of world countries}

\description{This dataset contains a polygon shapefile that can be used to plot the borders of countries.}

\usage{data(countries)}

\format{A \var{SpatialPolygons}}

\source{
This \var{SpatialPolygons} is derived from the \code{\link[maps]{world}} of the package \pkg{maps}.
Please refer to this other package for description and sources of this dataset.
See example for details on how we created the dataset.
}

\seealso{
\code{\link{oceanmask}} for another polygon shapefile used to embellish the plots

}

\examples{
data(countries)

if(require(sp))
  plot(countries, border="red", col="darkgrey")


### HOW DID WE CREATE THIS FILE?
### (This example takes some time and will therefore not be run
###  unless you type: example(countries, run.dontrun=TRUE) )

\dontrun{
if(require(maps) & require(maptools) & require(raster) & require(rgeos)){
    worldmap <- map("world", fill=TRUE, plot=FALSE)
    countries <- map2SpatialPolygons(worldmap, IDs=worldmap$names)
    countries <- gBuffer(countries, byid=TRUE, width=0)
    proj4string(countries) <- CRS("+proj=longlat +datum=WGS84")
    countries
    ## uncomment the following to store the file:
    #save(countries, file="countries.rda", compress="xz")
}
}

}

\keyword{datasets}

