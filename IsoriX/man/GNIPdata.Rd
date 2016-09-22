\name{GNIPdata}
\alias{GNIPdata}

\docType{data}
\title{World-wide weather station data}

\description{This dataset contains the mean and variance of Deuterium delta precipitation values of weather stations sampled between 1960 and 2013. Data have been compiled by the International Atomic Energy Agency IAEA in Vienna (GNIP Project: Global Network of Isotopes in Precipitation).}

\usage{data(GNIPdata)}

\format{
	The \var{dataframe} includes 54513 observations on the following variables:
	\tabular{rlll}{
	[, 1] \tab lat \tab (\var{numeric}) \tab  Latitude coordinate [decimal degrees]\cr
	[, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
	[, 3] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
	[, 4] \tab isoscape.value \tab (\var{numeric}) \tab Deuterium stable hydrogen delta value [per thousand]\cr
	[, 5] \tab year \tab (\var{numeric}) \tab Year of sampling\cr
	[, 6] \tab month \tab (\var{numeric}) \tab Month of sampling\cr
	[, 7] \tab stationID \tab (\var{Factor}) \tab The unique identifier of the weather station\cr
	}   
}

\details{
The dataset contains non-aggregated data for 858 weather stations across the world.

This dataset is the raw data source and should not be directly used for fitting isoscapes.

Please use \code{\link{queryGNIP}} to filter the dataset by time and location.
}

\note{Some elevations seem to be wrong... see \code{\link{GNIPdataEU}} for details.}

\source{Data extracted from IAEA (see below for more details).}

\seealso{
\code{\link{GNIPdataEU}} for information on what your working dataset should look like
\code{\link{queryGNIP}} to filter the dataset by time and location.
}

\references{
GNIP Project IAEA Global Network of Isotopes in Precipitation:

\url{http://www.iaea.org}
}

\examples{
data(GNIPdata)
head(GNIPdata)
}

\keyword{datasets}

