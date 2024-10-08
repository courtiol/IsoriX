% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serialize.R
\name{serialize}
\alias{serialize}
\alias{serialise}
\alias{readRDS}
\alias{saveRDS}
\alias{saveRDS_IsoriX}
\alias{saveRDS.ISOSCAPE}
\alias{saveRDS.CALIBFIT}
\alias{saveRDS.ISOFIND}
\alias{readRDS.character}
\alias{saveRDS,ISOSCAPE-method}
\alias{saveRDS,CALIBFIT-method}
\alias{saveRDS,ISOFIND-method}
\alias{readRDS,character-method}
\title{Save and read objects produced by IsoriX using RDS files}
\usage{
saveRDS_IsoriX(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\method{saveRDS}{ISOSCAPE}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\method{saveRDS}{CALIBFIT}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\method{saveRDS}{ISOFIND}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\method{readRDS}{character}(file, refhook = NULL)

\S4method{saveRDS}{ISOSCAPE}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\S4method{saveRDS}{CALIBFIT}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\S4method{saveRDS}{ISOFIND}(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

\S4method{readRDS}{character}(file, refhook = NULL)
}
\arguments{
\item{object}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) R object to serialize.}

\item{file}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) a connection or the name of the file where the R object is saved to or read from.}

\item{ascii}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) a logical. If \code{TRUE} or \code{NA}, an ASCII representation is written; otherwise (default), a binary one is used. See the comments in the help for \code{\link[base:save]{base::save}}.}

\item{version}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) the workspace format version to use. \code{NULL} specifies the current default version (3). The only other supported value is \code{2}, the default from R 1.4.0 to R 3.5.0.}

\item{compress}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) a logical specifying whether saving to a named file is to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if file is a connection.}

\item{refhook}{(definition copied from  \code{\link[base:readRDS]{base::readRDS}}:) a hook function for handling reference objects.}
}
\value{
For \code{saveRDS}, \code{NULL} invisibly.

For \code{readRDS}, an R object.
}
\description{
Because files created with IsoriX contain \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} and
\code{\link[terra:SpatVector-class]{terra::SpatVector}} objects, they cannot be saved using \code{\link[base:readRDS]{base::saveRDS}}
or \code{\link[base:save]{base::save}} functions. The reason is that objects created with \code{\link[terra:terra-package]{terra::terra}}
point to data stored in memory which are not contained in the R objects
themselves. Adapting the approach implemented in the \code{\link[terra:terra-package]{terra::terra}} package, we
provide a wrapper for \code{\link[base:readRDS]{base::saveRDS}} and \code{\link[base:readRDS]{base::readRDS}} functions,
which allows one to save and read objects produced with IsoriX by simply
using \code{saveRDS()} and \code{readRDS()}.
}
\details{
\code{\link[base:readRDS]{base::saveRDS}} and \code{\link[base:readRDS]{base::readRDS}} are standard S3 functions. So in
order to be able to have a specific behaviour for objects produced with
IsoriX, we imported \code{saveRDS} and \code{readRDS} S4 generics from  \code{\link[terra:terra-package]{terra::terra}} to
dispatch both S3 and S4 IsoriX-specific methods (see \link{Methods_for_S3}). The
S3 implementation is consistent with the rest of the package and presents all
usual benefits associated with S3 methods (e.g. simple access to the code).
The S4 implementation makes IsoriX methods compatible with the use of
\code{\link[terra:serialize]{terra::saveRDS}} and \code{\link[terra:serialize]{terra::readRDS}}.
}
\section{Functions}{
\itemize{
\item \code{saveRDS_IsoriX()}: S3 function to save IsoriX objects into a RDS file

\item \code{saveRDS(ISOSCAPE)}: S3 method to save an \code{ISOSCAPE} object into a RDS file

\item \code{saveRDS(CALIBFIT)}: S3 method to save a \code{CALIBFIT} object into a RDS file

\item \code{saveRDS(ISOFIND)}: S3 method to save an \code{ISOFIND} object into a RDS file

\item \code{readRDS(character)}: S3 method to read an object produced with IsoriX (or other) stored in a RDS file

\item \code{saveRDS(ISOSCAPE)}: S4 method to save an \code{ISOSCAPE} object into a RDS file

\item \code{saveRDS(CALIBFIT)}: S4 method to save an \code{CALIBFIT} object into a RDS file

\item \code{saveRDS(ISOFIND)}: S4 method to save an \code{ISOFIND} object into a RDS file

\item \code{readRDS(character)}: S4 method to read an object produced with IsoriX (or other) stored in a RDS file

}}
\examples{
if (getOption_IsoriX("example_maxtime") > 30) {
  ## We prepare the data
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## We fit the models
  GermanFit <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  ## We build the isoscapes
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  ## Saving as RDS
  filename <- tempfile(fileext = ".rds") # or whatever names you want
  saveRDS(GermanScape, file = filename)

  ## Reading RDS
  GermanScape2 <- readRDS(filename)
  GermanScape2
}

}
\keyword{saving}
