\name{isosim}
\alias{isosim}

\title{Simulate isotopic values}

\description{This function allows for the simulation of isoscapes. Both partial or complete (i.e. maps) isoscapes can be simulated.}

\usage{
isosim(
    simu.data,
    mean.model.fix.coef=c(intercept=64, elev=-0.01,
        lat.abs=-2.3, lat.2=0, long=0, long.2=0),
    disp.model.fix.coef=c(intercept=5.8, elev=0,
        lat.abs=0, lat.2=0, long=0, long.2=0),
    mean.model.matern.coef=c(nu=0.35, rho=5e-5, lambda=899),
    disp.model.matern.coef=c(nu=3.2e-01, rho=1.5e-05, lambda=5),
    mean.model.uncorr.coef=c(nugget=0, lambda=0),
    disp.model.uncorr.coef=c(nugget=0, lambda=0),
    dist.method="Earth",
    seed=NULL,
    save.dataframe=FALSE,
    verbose=interactive()
)
}

\arguments{
\item{simu.data}{A \var{data.frame} containing the covariates needed for the simulation, or alternatively an elevation raster of class \var{RasterLayer}}
\item{mean.model.fix.coef}{A \var{vector} of coefficients for fixed-effects}
\item{disp.model.fix.coef}{A \var{vector} of coefficients for fixed-effects}
\item{mean.model.matern.coef}{A \var{vector} of coefficients for the spatial random effect}
\item{disp.model.matern.coef}{A \var{vector} of coefficients for the spatial random effect}
\item{mean.model.uncorr.coef}{A \var{vector} of coefficients for the uncorrelated random effect}
\item{disp.model.uncorr.coef}{A \var{vector} of coefficients for the uncorrelated random effect}
\item{dist.method}{A \var{string} indicating the distance method}
\item{seed}{An \var{integer} used to set the seed of the random generator}
\item{save.dataframe}{A \var{logical} indicating whether the detailed \var{data.frame} containing the simulated data should be saved}
\item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \var{TRUE} if users use an interactive R session and \var{FALSE} otherwise.}
}

\details{
This function takes as inputs the values for all covariates matching a series of locations (which can be provided as an elevation raster or as a \var{data.frame}), as well as the parameters of the isoscape model. The function is not required to fit an isoscape, nor to perform assignments. It is an additional function that can be useful to test the method, and to study the effect of different parameters on isoscapes. We chose default values for parameters inspired to model fits in order to simulate a relatively realistic isoscape. We precised 'relatively' because, in the simulations, the Matern realizations for the mean and the dispersion are drawn independently, which is not the case in nature. Note that extra parameters present in the input lists will not make the function crash but won't be considered during computations either.
}

\value{This function returns a \var{list} of class \var{isoscape} containing a stack of raster and an optional \var{data.frame}. The stack contains the raster \code{mean.raster} storing the mean isotopic value, and the raster \cr \code{disp.raster} storing the residual dispersion variance. The optional \var{data.frame} contains the simulated data and details of the computation in an object called \code{data}.
}

\note{
The spatial autocorrelation and the Nugget are computed by the functions \code{\link[RandomFields:RMmatern]{RMwhittle}} and \code{\link[RandomFields]{RMnugget}}, respectively. These two functions are part of the powerful package \pkg{\link[RandomFields]{RandomFields}}.
}

\seealso{
\code{\link{isofit}} for the function fitting the isoscape model
\code{\link{IsoriX}} for the complete work-flow
}

\examples{

## This example takes couple of minutes and will therefore not be run
## unless you type: example(isosim, run.dontrun=TRUE)

\dontrun{
## We load an elevation raster
data(elevraster)
data(countries)
data(oceanmask)
data(isopalette1)

elevationraster <- relevate(elevraster,
    manual.crop=c(-30, 60, 30, 70))

## We simulate data under default settings
simu <- isosim(simu.data=elevationraster,
    save.dataframe=TRUE, seed=2)

simu

## We build the plots of the outcome using IsoriX
plot.mean.simu <- plot(
    x=simu,
    which="mean",
    borders=list(borders=countries),
    mask=list(mask=oceanmask),
    palette=isopalette1)

plot.disp.simu <- plot(
    x=simu,
    which="disp",
    borders=list(borders=countries),
    mask=list(mask=oceanmask),
    palette=isopalette1)


## We fit the simulated data by sampling 200 locations

set.seed(123)
newdat <- simu$data[sample(1:nrow(simu$data), 200), ]

isoscapemodel <- isofit(iso.data=newdat,
    mean.model.fix=list(elev=TRUE, lat.abs=TRUE))

isoscape <- isoscape(elevationraster, isoscapemodel)

plot.mean.fitted <- plot(
    x=isoscape,
    which="mean",
    sources=list(draw=FALSE),
    borders=list(borders=countries),
    mask=list(mask=oceanmask),
    palette=isopalette1)

plot.disp.fitted <- plot(
    x=isoscape,
    which="disp",
    sources=list(draw=FALSE),
    borders=list(borders=countries),
    mask=list(mask=oceanmask),
    palette=isopalette1)

## We compare simulated and fitted data visually
if(require(lattice)){
    print(plot.mean.simu, split=c(1, 1, 2, 2), more=TRUE)
    print(plot.disp.simu, split=c(2, 1, 2, 2), more=TRUE)
    print(plot.mean.fitted, split=c(1, 2, 2, 2), more=TRUE)
    print(plot.disp.fitted, split=c(2, 2, 2, 2), more=FALSE)
}

## We compare simulated and fitted data numerically
## Note that differences are expected, as the geographic
##   area is small compared to the scale at which
##   spatial auto-correlation occurs
round(cbind(
    simulated=c(
        intercept=64,
        lat.abs=-2.3, 
        elev=-0.01,
        nu=0.35,
        rho=5e-5,
        rho_div_nu = 5e-5/0.35,
        lambda.matern=899,
        intercept.disp=5.8,
        nu.disp=3.2e-01,
        rho.disp=1.5e-05,
        lambda.matern.disp=5),
    fitted=c(
        intercept=isoscapemodel$mean.fit$fixef[1],
        lat.abs=isoscapemodel$mean.fit$fixef[2], 
        elev=isoscapemodel$mean.fit$fixef[3],
        nu=isoscapemodel$mean.fit$ranFix$nu,
        rho=isoscapemodel$mean.fit$ranFix$rho,
        rho_div_nu=with(isoscapemodel$mean.fit$ranFix,rho/nu),
        lambda.matern=isoscapemodel$mean.fit$lambda,
        intercept.disp=isoscapemodel$disp.fit$fixef[1],
        nu.disp=isoscapemodel$disp.fit$ranFix$nu,
        rho.disp=isoscapemodel$disp.fit$ranFix$rho,
        lambda.matern.disp=isoscapemodel$disp.fit$lambda)), 4)

}
}

\keyword{simulate}
\keyword{simulation}
