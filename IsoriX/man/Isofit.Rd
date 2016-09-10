\name{Isofit}
\alias{Isofit}

\title{Fit the isoscape model}

\description{This function fits the isocape as a mixed model. The fitting procedures are done by the package \pkg{\link[spaMM]{spaMM}} which we use to jointly fit the mean isotopic values and their associated residual dispersion variance in a spatially explicit manner.}

\usage{
Isofit(
    iso.data,
    mean.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
    disp.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
    mean.model.rand=list(uncorr=FALSE, spatial=TRUE),
    disp.model.rand=list(uncorr=FALSE, spatial=TRUE),
    uncorr.terms=list(mean.model="lambda", disp.model="lambda"), ## or: "nugget"
    spaMM.method=list(mean.model="fitme", disp.model="fitme"), ## or: "corrHLfit", "HLfit"
    dist.method="Earth", ## or: "Euclidean"
    control.mean=list(),
    control.disp=list(),
    verbose=interactive()
)
}

\arguments{
\item{iso.data}{The \var{dataframe} containing the data used for fitting the isoscape model}
\item{mean.model.fix}{A \var{list} of \var{logical} indicating which fixed effects to consider in mean.fit}
\item{disp.model.fix}{A \var{list} of \var{logical} indicating which fixed effects to consider in disp.fit}
\item{mean.model.rand}{A \var{list} of \var{logical} indicating which random effects to consider in mean.fit}
\item{disp.model.rand}{A \var{list} of \var{logical} indicating which random effects to consider in disp.fit}
\item{uncorr.terms}{A \var{list} of two strings defining the parametrization used to model the uncorrelated random effects for mean.fit and disp.fit}
\item{spaMM.method}{A \var{list} of two strings defining the spaMM functions used for mean.fit and disp.fit}
\item{dist.method}{A \var{string} indicating the distance method}
\item{control.mean}{A \var{list} of additional arguments to be passed to the call of mean.fit}
\item{control.disp}{A \var{list} of additional arguments to be passed to the call of disp.fit}
\item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \code{TRUE} if users use an interactive R session and \code{FALSE} otherwise.}
}

\value{
This function returns a \var{list} of class \code{isofit} containing two inter-related fits: \code{mean.fit} and \code{disp.fit}. The returned \var{list} also contains the object \code{info.fit} that contains all the call arguments.
}

\details{
The detailed statistical definition of the isoscape model will be soon available in another document. Briefly, the fitting procedure of the isoscape model is divided into two fits: \code{mean.fit} and \code{disp.fit}. \code{mean.fit} corresponds to the fit of the "mean model", which we will use to predict the mean isotopic values at any location in other functions of the package. \code{disp.fit} corresponds to the fit of the "residual dispersion model", which we will use to predict the residual dispersion variance associated to the mean predictions. \code{mean.fit} is a linear mixed-effects model (LMM) with fixed effects, an optional spatial random effect with a Matern correlation structure and an optional uncorrelated random effect accounting for variation between weather station unrelated to their location. \code{disp.fit} is  a Gamma Generalized LMM (Gamma GLMM) that also has fixed effects, an optional spatial random effect with a Matern correlation structure and an optional uncorrelated random effect. For the GLMM the residual variance is fixed to its theoretical expectation.

The \var{dataframe} \code{iso.data} must contain a single row per source location with the following columns: \code{isoscape.value} (the isotopic value), \code{var.isoscape.value} (the unbiased variance estimate of the isotopic value at the location), \code{n.isoscape.value} (the number of measurements performed at the location, could be 1) and \code{stationID} (a factor defining the identity of the sources at a given location).

The arguments \code{mean.model.fix} and \code{disp.model.fix} allow the user to choose among different fixed-effect structures for each model. These arguments are lists of booleans (\code{TRUE} or \code{FALSE}), which define which of the following fixed effects must be considered: the elevation (\code{elev}), the absolute value of the latitude (\code{lat.abs}), the squared latitude (\code{lat.2}), the longitude (\code{long}) and the squared longitude (\code{long.2}). An intercept is always considered in both models. By default, only intercept are being fitted. To fit an isoscape with the same fixed effect structure as the seminal Bowen model (Bowen et al. 2005), one should thus change \code{mean.model.fix} to \code{list(elev=TRUE, lat.abs=TRUE, lat.2=TRUE)}.

In the models, the mean (for the mean model) or the log residual variance (for the residual dispersion model) follow a Gaussian distribution around a constant value. The arguments \code{mean.model.rand} and \code{disp.model.rand} allow to choose among different random effects for each model influencing the realizations of these Gaussian random processes. For each model one can choose not to include random effects or to include an uncorrelated random effect, a spatial random effect, or both (default). Setting \code{"uncorr"=TRUE} implies that the different realizations are identical for a given weather station (e.g. some micro-climate or some measurement errors trigger a shift in all measurements (mean model) or a shift in the variance between measurements (residual dispersion model) performed on a given weather station by the same amount). Setting \code{"spatial"=TRUE} (default) implies that the random realizations of the Gaussian process follow a Matern correlation structure. Put simply, this implies that the closer two locations are, the more similar the means (or the log residual variance) in isotopic values are (e.g. because they are likely to be traversed by the same air masses).

The arguments \code{uncorr.terms} allow the choice between two alternative parameterizations for the uncorrelated random effect in the fits: \code{"lambda"} or \code{"nugget"} for each model. When using \code{"lambda"}, the variance of the uncorrelated random terms is classically modeled by a variance. When a spatial random effect is considered, one can alternatively choose \code{"nugget"}, which modifies the Matern correlation value when distance between location tends to zero. If no random effect is considered, one should stick to the default setting and it will be ignored by the function. The choice of the parametrization is a matter of personal preferences and it does not change the underlying models, so the estimations for all the other parameters of the models will not be impacted by whether one chooses \code{lambda} or \code{nugget}. Depending on the data one parametrization may lead to faster fit than the other.

The argument \code{spaMM.method} is also a list of two \var{strings} where the first element defines the spaMM functions used for fitting the mean model and the second element defines the spaMM method used for fitting the residual dispersion model. The possible options are "HLfit", "corrHLfit" and "fitme". Note that "HLfit" shall only be used in the absence of a Matern correlation structure and "corrHLfit" shall only be used in the presence of it. In contrast, "fitme" should work in all situations. Which method is best remains to be determined and it is good practice to try different methods (if applicable) to check for the robustness of the results. If all is well one should obtain very similar results with the different methods. If this is not the case,  carefully check the model output to see if one model fit did not get stuck at a local minimum during optimization (which would translate in a lower likelihood).

The argument \code{dist.method} allows modifying how the distance between locations is computed to estimate the spatial correlation structure. By default, we consider the so-called "Earth" distances which are technically called orthodromic distances. They account for earth curvature. The alternative "Euclidean" distances do not. For studies performed on a small geographic scale, both distance methods should lead to similar results.

The arguments \code{control.mean} and \code{control.dist} are lists that are transmitted to the \pkg{\link[spaMM]{spaMM}} fitting functions (defined by \code{spaMM.method}). These lists can be used to finely control the fitting procedure, so advanced knowledge of the package \pkg{\link[spaMM]{spaMM}} is required before messing around with these inputs.

We highly recommend users to examine the output produced by \code{Isofit}. Sometimes, poor fit may occur and such models should therefore not be used for building isoscapes or performing assignments.
}

\note{
There is no reason to restrict \code{mean.fit} and \code{disp.fit} to using the same parametrization for fixed and random effects.

Never use a mean.fit object to draw predictions without considering a disp.fit object: mean.fit is not fitted independently from disp.fit.

For all methods, fixed effects are being estimated by Maximum Likelihood (ML) and dispersion parameters (i.e. random effects and Matern correlation parameters) are estimated by Restricted Maximum Likelihood (REML). Using REML provides more accurate prediction intervals but impedes the accuracy of Likelihood Ratio Tests (LRT). Our choice for REML was motivated by the fact that our package is more likely to be used for drawing inferences than null hypothesis testing. Users interested in model comparisons may rely on the AIC values that can be extracted from fitted models using the function \code{\link[spaMM:extractors]{AIC}} from the \pkg{\link[spaMM]{spaMM}}.

Variable names for \code{iso.data} must be respected to ensure a correct utilization of this package. Alteration to the fixed effect structure is not implemented so far (beyond the different options proposed) to avoid misuse of the package. Users that would require more flexibility should consider using spaMM directly (at their own risks). We will soon provide a document explaining how to do so.
}

\source{
\url{http://kimura.univ-montp2.fr/~rousset/spaMM.htm}
}

\seealso{
\pkg{\link[spaMM]{spaMM}} for an overview of the \pkg{spaMM} package

\code{\link[spaMM]{fitme}} and \code{\link[spaMM]{corrHLfit}} for information about the two possible fitting procedures that can be used here

\code{\link[spaMM]{Matern.corr}} for information about the Matern correlation structure

\code{\link{IsoriX}} for the complete work-flow of our package

}

\references{
Rousset, F., Ferdy, J. B. (2014). Testing environmental and genetic effects in the presence of spatial autocorrelation. Ecography, 37(8):781-790.

Bowen, G. J., Wassenaar, L. I., Hobson, K. A. (2005). Global application of stable hydrogen and oxygen isotopes to wildlife forensics. Oecologia, 143(3):337-348.
}

\examples{

## We fit the isoscape model (on the first 50 rows only,
## otherwise it takes too long for a simple example)

data(GNIP_Europe)

isoscapemodel1 <- Isofit(iso.data=GNIP_Europe[1:50, ],
    mean.model.fix=list(elev=TRUE, lat.abs=TRUE))

isoscapemodel1

## Diagnostics for the fits
plot(isoscapemodel1)


## The following example takes some time and will therefore not
## be run unless you type: example(Isofit, run.dontrun=TRUE)

\dontrun{
## Model with one uncorrelated random effect using the Lambda
## parametrization for the fitting the mean model and with one 
## uncorrelated random effect using the Nugget parametrization 
## for the fitting the residual dispersion model:

isoscapemodel2 <- Isofit(iso.data=GNIP_Europe[1:50, ],
    mean.model.fix=list(elev=TRUE, lat.abs=TRUE),
    mean.model.rand=list(uncorr=TRUE),
    disp.model.rand=list(uncorr=TRUE),
    uncorr.terms=list(disp.model="nugget"))
        
isoscapemodel2
}

}

\keyword{models}
\keyword{regression}

