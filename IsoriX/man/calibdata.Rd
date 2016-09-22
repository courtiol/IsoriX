\name{calibdata}
\alias{calibdata}

\docType{data}
\title{Calibration dataset}

\description{This dataset contains deuterium delta values of fur keratin from sedentary bat species captured between 2005 and 2009 from Popa-Lisseanu et al. (2012). The data can be used as an example to fit a calibration model using the function \code{\link{calibfit}}.}

\usage{
data(calibdata)
}

\format{
A \var{dataframe} with 178 observations on 7 variables:
\tabular{rlll}{
[, 1] \tab siteID \tab (\var{Factor}) \tab Identification of the sampling site\cr
[, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
[, 3] \tab lat \tab (\var{numeric}) \tab  Latitude coordinate [decimal degrees]\cr
[, 4] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
[, 5] \tab animalID \tab (\var{Factor}) \tab Identification of the sampled animal\cr
[, 6] \tab species \tab (\var{Factor}) \tab Species name \cr
[, 7] \tab tissue.value \tab (\var{numeric}) \tab Deuterium delta value of the tissue\cr
}   
}

\details{
Users who wish to use their own dataset for calibration should create a \var{dataframe} of similar structure than this one (only the column 'species' can be dropped). The columns should possess the same names as the ones described above. If the elevation is unknown at the sampling sites, elevation information can be extracted from a high resolution elevation raster using the function \pkg{\link[raster]{extract}}. In this dataset, we retrieved elevations from the Global Multi-resolution Terrain Elevation Data 2010. Note that the original study used a different source of elevation data.
}

\source{data directly provided by the authors of the following publication}

\references{
Popa-Lisseanu, A. G., Soergel, K., Luckner, A., Wassenaar, L. I., Ibanez, C., Kramer-Schadt, S., Ciechanowski, M., Goerfoel, T., Niermann, I., Beuneux, G., Myslajek, R. W., Juste, J., Fonderflick, J., Kelm, D., Voigt, C. C. (2012). A triple isotope approach to predict the breeding origins of European bats. PLoS ONE 7(1):e30388.
}

\examples{
data(calibdata)
head(calibdata)
str(calibdata)

## The following example require to have downloaded
## a large elevation raster with the function getelev()
## and will therefore not run unless you type:
## example(calibdata, run.dontrun=TRUE)

\dontrun{
if(require(raster)){
    ### DELETE AND RECREATE ELEVATION DATA
    calibdata$elev <- NULL  ## we delete them

    ## we reconstruct the elevation data using an elevation raster:
    ## (see ?getelev for details on how to get the tif file)
    elevationrasterbig <- raster("gmted2010_30mn.tif")
    calibdata$elev <- extract(
        elevationrasterbig,
        cbind(calibdata$long, calibdata$lat))
    head(calibdata)
}

}

}

\seealso{
\code{\link{calibfit}} to fit a calibration model

}

\keyword{datasets}

