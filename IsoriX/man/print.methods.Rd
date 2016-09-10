\name{print.isofit}

\alias{print.isofit}
\alias{print.isoscape}
\alias{print.calibfit}
\alias{print.isorix}
\alias{print}

\alias{summary.isofit}
\alias{summary.isoscape}
\alias{summary.calibfit}
\alias{summary.isorix}
\alias{summary}

\title{Print and summary methods for IsoriX}

\description{These functions print summary information of objects of class \var{isofit}, \var{isoscape} or \var{isorix}.}

\usage{
\method{print}{isofit}(x, ...)
\method{print}{isoscape}(x, ...)
\method{print}{calibfit}(x, ...)
\method{print}{isorix}(x, ...)

\method{summary}{isofit}(object, ...)
\method{summary}{isoscape}(object, ...)
\method{summary}{calibfit}(object, ...)
\method{summary}{isorix}(object, ...)
}

\arguments{
  \item{object}{The return object of an \code{\link{Isofit}}, \code{\link{Isoscape}}, \code{\link{Calibfit}}, or \code{\link{Isorix}} call}
  \item{x}{The return object of an \code{\link{Isofit}}, \code{\link{Isoscape}}, \code{\link{Calibfit}}, or \code{\link{Isorix}} call}
  \item{...}{Additional arguments}
}

\value{These functions return nothing}

\seealso{
\code{\link{Isofit}} for the function: fitting the isoscape

\code{\link{Isoscape}} for the function: building the isoscape

\code{\link{Calibfit}} for the function: fitting the calibration function

\code{\link{Isorix}} for the function: making the assignment

\code{\link{IsoriX}} for the complete workflow

}


\keyword{print}
