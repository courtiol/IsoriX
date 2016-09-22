\name{Europefit}
\alias{Europefit}
\docType{data}

\title{The fitted isoscape model for GNIPdataEU}

\description{This dataset contains an object of class \code{isofit} containing the mean model and residual dispersion model fitted on the data \code{\link{GNIPdataEU}}.}

\usage{data(Europefit)}

\format{
An object of class \code{isofit}
}

\details{
This fitted isoscape model has been obtained by running the function \code{\link{isofit}} on the isotopic source data stored in \code{\link{GNIPdataEU}} (see example).
}

\seealso{
\code{\link{isofit}} for information about how to fit an isoscape model

\code{\link{GNIPdataEU}} for information about the weather station data used to fit this isoscape model
}


\examples{

data(Europefit)
Europefit
plot(Europefit)

## The following example takes a lot of time and will therefore not
## be run unless you type: example(Europefit, run.dontrun=TRUE)

\dontrun{
## We load the data
data(GNIPdataEU)

## We fit the isoscape model
Europefit <- isofit(iso.data=GNIPdataEU,
    mean.model.fix=list(elev=TRUE, lat.abs=TRUE),
    mean.model.rand=list("uncorr"=TRUE),
    disp.model.rand=list("uncorr"=TRUE))

## We created the object Europefit stored in this package using
## save(Europefit, file="Europefit.rda", compress="xz")

Europefit
plot(Europefit)
}

}

\keyword{datasets}
\keyword{models}
