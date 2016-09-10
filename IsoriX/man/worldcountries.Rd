\name{worldcountries}
\alias{worldcountries}

\docType{data}
\title{Borders of world countries}

\description{This dataset contains a polygon shapefile that can be used to plot the borders of countries.}

\usage{data(worldcountries)}

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
data(worldcountries)

if(require(sp))
  plot(worldcountries, border="red", col="darkgrey")


### HOW DID WE CREATE THIS FILE?
### (This example takes some time and will therefore not be run
###  unless you type: example(worldcountries, run.dontrun=TRUE) )

\dontrun{
if(require(maps) & require(maptools) & require(raster) & require(rgeos)){
    worldmap <- map("world", fill=TRUE, plot=FALSE)
    worldcountries <- map2SpatialPolygons(worldmap, IDs=worldmap$names)
    worldcountries <- gBuffer(worldcountries, byid=TRUE, width=0)
    proj4string(worldcountries) <- CRS("+proj=longlat +datum=WGS84")
    worldcountries
    ## uncomment the following to store the file:
    #save(worldcountries, file="worldcountries.rda", compress="xz")
}
}

}

\keyword{datasets}

