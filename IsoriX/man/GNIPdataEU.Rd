\name{GNIPdataEU}
\alias{GNIPdataEU}

\docType{data}
\title{European weather station data}

\description{This dataset contains the mean and variance of Deuterium delta precipitation values of European weather stations sampled between 1960 and 2013. Original data have been compiled by the International Atomic Energy Agency IAEA in Vienna (GNIP Project: Global Network of Isotopes in Precipitation).}

\usage{data(GNIPdataEU)}

\format{
	The \var{dataframe} includes 296 observations for the following variables:
	\tabular{rlll}{
	[, 1] \tab stationID \tab (\var{Factor}) \tab Identification number of weather station\cr
	[, 2] \tab isoscape.value \tab (\var{numeric}) \tab Mean Deuterium delta value [per thousand]\cr
	[, 3] \tab var.isoscape.value \tab (\var{numeric}) \tab Variance in Deuterium delta value [per million]\cr
	[, 4] \tab lat \tab (\var{numeric}) \tab  Latitude coordinate [decimal degrees]\cr
	[, 5] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
	[, 6] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
	}   
}

\note{
Users who wish to use their own dataset for isoscape model fitting should create a similar \var{dataframe}. All columns should possess the same names as the ones described above. We found occasional strong discrepancies between elevations from the \code{\link{GNIPdata}} dataset and elevation from the \cr GMTED2010 satellite dataset. We also found occasional variation among elevations from the \code{\link{GNIPdata}} same weather stations. For these reasons, we decided to compute all elevations for GNIPdataEU using satellite estimation only, before we clarify the situation.

}

\source{Data extracted from IAEA (see below for more details).}

\seealso{
\code{\link{queryGNIP}} to see how we created this dataset from \code{\link{GNIPdata}}
\code{\link{isofit}} to fit an isoscape based on these data
\code{\link{GNIPdata}} for the full non-aggregated dataset

}

\references{
GNIP Project IAEA Global Network of Isotopes in Precipitation:

\url{http://www.iaea.org}
}

\examples{
data(GNIPdataEU)
head(GNIPdataEU)
}

\keyword{datasets}

