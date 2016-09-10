\name{isorixpalette}
\alias{isorixpalette}
\alias{isoscapepalette}

\docType{data}
\title{Colour palettes for plotting}

\description{
These datasets contain colour vectors that can be used for plotting. In our examples, we use the \code{isoscapepalette} for plotting the isoscape using \code{\link{plot.isoscape}} and \code{isorixpalette} for plotting the assignment outcoume using \code{\link{plot.isorix}}.
}

\usage{
data(isoscapepalette)
data(isorixpalette)
}

\format{
A vector of colours
}

\details{
Colour palettes can be created by using the function \code{\link{colorRamp}} that interpolates colours between a set of given colours. One can also use \code{\link{colorRampPalette}} to create functions providing colours. Also interesting, the function \code{colorspace::choose_palette} offers a GUI interface allowing to create and save a palette in a hexadecimal format (which can later on be imported into R). This latter function is however limited to a maximum of 50 colours. You can also use R colour palettes already available such as \code{\link{terrain.colors}} or others available (see examples below). Alternatively, you can design your own colour palette by writing standard hexadecimal code of colours into a vector.
}

\note{
We use the package \pkg{rasterVis} for plotting. Instead of using colour palettes directly, one can also use any "Theme" designed for the lattice graphic environment (see source for details).
}

\source{For information on how to use themes, check:

\url{https://oscarperpinan.github.io/rastervis/#themes}
}

\seealso{
\code{\link{rainbow}} for information about R colour palettes

\code{\link{colorRamp}} and \code{colorspace::choose_palette} to create your own palettes

}

\examples{
### A COMPARISON OF SOME COLOUR PALETTES
data(isoscapepalette)
data(isorixpalette)

par(mfrow=c(2, 3))
pie(rep(1, length(isoscapepalette)), col = isoscapepalette,
	border=NA, labels=NA, clockwise=TRUE, main="isoscapepalette")
pie(rep(1, length(isorixpalette)), col = isorixpalette,
	border=NA, labels=NA, clockwise=TRUE, main="isorixpalette")
pie(rep(1, 100), col = terrain.colors(100), border=NA, labels=NA,
    clockwise=TRUE, main="terrain.colors")
pie(rep(1, 100), col = rainbow(100), border=NA, labels=NA,
    clockwise=TRUE, main="rainbow")
pie(rep(1, 100), col = topo.colors(100), border=NA, labels=NA,
    clockwise=TRUE, main="topo.colors")
pie(rep(1, 100), col = heat.colors(100), border=NA, labels=NA,
    clockwise=TRUE, main="heat.colors")

### CREATING YOUR OWN COLOUR PALETTE
my.palette  <- colorRampPalette(c("blue", "green", "red"), bias=0.7)
par(mfrow=c(1, 1))
pie(1:100, col=my.palette(100), border=NA, labels=NA,
    clockwise=TRUE, main="a home-made palette")
}


\keyword{color}
\keyword{datasets}

