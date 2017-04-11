\name{Europefit}
\alias{Europefit}
\docType{data}

\title{The fitted isoscape model for Europe}

\description{This dataset contains an object of class \code{isofit} containing the mean model and residual dispersion model fitted on the data for Europe (see example).}

\usage{data(Europefit)}

\format{
An object of class \code{isofit}
}

\details{
This fitted isoscape model has been obtained by running the function \code{\link{isofit}} on the isotopic source data stored in GNIPdataEU (see example).
}

\seealso{
\code{\link{isofit}} for information about how to fit an isoscape model

}


\examples{

data(Europefit)
Europefit
plot(Europefit)

## The following example takes a lot of time and will therefore not
## be run unless you type: example(Europefit, run.dontrun=TRUE)

\dontrun{
## We load the data for the entire world
data(GNIPdata)

## We prepare the data for Europe
GNIPdataEU <- queryGNIP(
    data=GNIPdata,
    long.min = -30, 
    long.max = 60,
    lat.min = 30, 
    lat.max = 70)

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
