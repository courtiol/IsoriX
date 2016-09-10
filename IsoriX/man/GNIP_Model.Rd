\name{GNIP_Model}
\alias{GNIP_Model}
\docType{data}

\title{The fitted isoscape model for GNIP_Europe}

\description{This dataset contains an object of class \code{isofit} containing the mean model and residual dispersion model fitted on the data \code{\link{GNIP_Europe}}.}

\usage{data(GNIP_Model)}

\format{
An object of class \code{isofit}
}

\details{
This fitted isoscape model has been obtained by running the function \code{\link{Isofit}} on the isotopic source data stored in \code{\link{GNIP_Europe}} (see example).
}

\seealso{
\code{\link{Isofit}} for information about how to fit an isoscape model

\code{\link{GNIP_Europe}} for information about the weather station data used to fit this isoscape model
}


\examples{

data(GNIP_Model)
GNIP_Model
plot(GNIP_Model)

## The following example takes a lot of time and will therefore not
## be run unless you type: example(GNIP_Model, run.dontrun=TRUE)

\dontrun{
## We load the data
data(GNIP_Europe)

## We fit the isoscape model
GNIP_Model <- Isofit(iso.data=GNIP_Europe,
    mean.model.fix=list(elev=TRUE, lat.abs=TRUE),
    mean.model.rand=list("uncorr"=TRUE),
    disp.model.rand=list("uncorr"=TRUE))

## We created the object GNIP_Model stored in this package using
## save(GNIP_Model, file="GNIP_Model.rda", compress="xz")

GNIP_Model
plot(GNIP_Model)
}

}

\keyword{datasets}
\keyword{models}
