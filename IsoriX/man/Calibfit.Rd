\name{Calibfit}
\alias{Calibfit}
\alias{print.calibfit}
\alias{summary.calibfit}

\title{Fit the calibration model}

\description{
This function fits a model that establishes the relationship between the isotopic values of organisms (e.g. tissues such as hair, horn, ivory or feathers) and the isotopic values of their environment (e.g. precipitation).}
\usage{
Calibfit(
	calib.data,
	isofit,
	verbose=interactive(),
	control.optim=list()
)
}

\arguments{
\item{calib.data}{A \var{dataframe} containing the calibration data (see note below)}
\item{isofit}{The fitted isoscape model created by \code{\link{Isofit}}}
\item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \var{TRUE} if users use an interactive R session and \var{FALSE} otherwise.}
\item{control.optim}{A \var{list} to pass information to the argument control of the optim call (for advanced users only).}
}

\value{
This function returns a \var{list} of class \var{calibfit} containing the fixed-effect estimates of the calibration function, the covariance of the fixed effects, the fitted calibration model, the original calibration data set with additional information added during the fit, and the location of the calibration points as spatial points.
}

\details{
The calibration model is a linear mixed-effects model (LMM) that fits the isotopic values of sedentary organisms as a linear function of the isotopic values in their environment (e.g. precipitation).

This function considers that the isotopic values from the environment (e.g. from precipitation) at the locations at which organisms were sampled are not known. The function therefore predicts these isotopic values from the geostatistical model fitted by the function \code{\link{Isofit}}, which is provided to \code{Calibfit} using the argument \code{isofit}.

The LMM used to fit the calibration function has a simple fixed-effect structure: an intercept and a slope. The random effect is more complex: it is normally distributed with mean zero, a certain variance between locations proportional to the squared fixed slope, and a covariance structure defined by the prediction covariance matrix of the isoscape model between the calibration locations. All models used in \pkg{IsoriX} will be soon detailed in an additional document.

This function is only needed in the case for which the assignment of organisms has to be performed within an isoscape that was built using another source of isotopic values (e.g., precipitation). This implies that if the isoscape had been fitted using isotopic ratios from sedentary animals, then this calibration step is not needed.

If source isotopic values are known at the locations where sedentary organisms were sampled, users should calibrate their data directly using the function \code{\link[stats]{lm}} by fitting tissue isotopic values as a function of source isotopic values.
}

\note{
See \code{\link{calibrationdata}} to know which variables are needed to perform the calibration fit and their names.
}

\seealso{
\code{\link{IsoriX}} for the complete workflow
}

\examples{

\dontrun{
## The following example takes some time and will therefore not
## be run unless you type: example(Calibfit, run.dontrun=TRUE)

data(calibrationdata)
data(GNIP_Model)

## fit the calibration model
calib <- Calibfit(
	calib.data=calibrationdata,
	isofit=GNIP_Model)

## display minimal information
calib

## display more information
summary(calib)

## plot the calibration function
plot(calib)
}

}

\keyword{models}
\keyword{regression}
