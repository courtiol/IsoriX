\name{QueryGNIP}
\alias{QueryGNIP}

\title{Filter the dataset to create an isoscape}

\description{This function prepares the worldwide GNIP data (\var{GNIP_World}) to be used for creating the isoscape. This function allows the trimming of data by months, years and location, and for the aggregation of selected data per weather station. The function can also be used to randomly exclude some observations.}

\usage{
QueryGNIP(
    data,
    month.min = 1,
    month.max = 12,
    year.min,
    year.max,
    long.min ,
    long.max,
    lat.min,
    lat.max,
    prop.random = 0,
    random.level = "station"
    )
}

\arguments{
    \item{data}{A \var{dataframe} containing original isotopic measurements similar in structure to \code{\link{GNIP_World}}}
    \item{month.min}{A \var{numeric} indicating the minimum month to select from. Should be a round number between 1 and 12. The default value is 1 (January).}
    \item{month.max}{A \var{numeric} indicating the maximum month to select from. Should be a round number between 1 and 12. 
    The default value is 12 (December).}
    \item{year.min}{A \var{numeric} indicating the oldest year to select from. If not provided, the oldest year of \code{GNIP_World} will be considered}
    \item{year.max}{A \var{numeric} indicating the most recent year to select from.  If not provided, the most recent year of \code{\link{GNIP_World}} will be considered}
    \item{long.min}{A \var{numeric} indicating the minimum longitude to select from. Should be a number between -180 and 180. If not provided, -180 will be considered.}
    \item{long.max}{A \var{numeric} indicating the maximal longitude to select from. Should be a number between -180 and 180. If not provided, 180 will be considered.}
    \item{lat.min}{A \var{numeric} indicating the minimum latitude to select from. Should be a number between -90 and 90. If not provided, -90 will be considered.}
    \item{lat.max}{A \var{numeric} indicating the maximal latitude to select from. Should be a number between -90 and 90. If not provided, 90 will be considered.}
    \item{prop.random}{A \var{numeric} indicating the proportion of observations or weather stations (depending on the argument for \code{random.level}) that will be kept. If \code{prop.random} is greater than 0, then the function will return a list containing two dataframes: one containing the selected data, called \code{selected.data}, and one containing the remaining data, called \code{remaining.data}.}
    \item{random.level}{A \var{string} indicating the level at which random draws can be performed. The two possibilities are \code{"obs"}, which indicates that observations are randomly drawn taken independently of their location, or "station" (default), which indicates that observations are randomly drawn at the level of weather stations.}
}

\value{This function returns a \var{dataframe} containing the filtered data aggregated by weather station, or a \var{list}, see above argument \code{prop.random}. For each weather station the mean and variance sample estimates are computed.}

\details{This functions allows the user to filter the world-wide weather station data (\var{GNIP_World}) based on time (years and/ or months) and space (locations given in geographic coordinates, i.e. longitude and latitude) to calculate tailored isoscapes matching e.g. the time of sampling and speeding up the model fit by cropping/ clipping a certain area. The dataframe produced by this function can be used as input to fit the isoscape (see \code{\link{Isofit}}).
}

\note{If one wants e.g. to select the winter months November to February, one has to select 11 as minimum and 2 as maximum and not the opposite.}

\seealso{
\code{\link{IsoriX}} for the complete work-flow
\code{\link{GNIP_World}} for the complete dataset
\code{\link{GNIP_Europe}} for an example of a dataset produced by this function

}


\examples{
data(GNIP_World)

### HOW DID WE CREATE THE OBJECT GNIP_Europe?

## The following example require to have downloaded
## a large elevation raster with the function GetElev()
## and will therefore not run unless you type:
## example(QueryGNIP, run.dontrun=TRUE)

## we first correct the elevations in GNIP_World:
\dontrun{
if(require(raster)) {
    elevationrasterbig <- raster("elevation_raster_gmted2010_30mn.tif")
            GNIP_World$elev <- extract(
                elevationrasterbig,
                cbind(GNIP_World$long, GNIP_World$lat))
    }
}

## then we used our function:
GNIP_Europe <- QueryGNIP(
    data=GNIP_World,
    long.min = -27.34, 
    long.max = 57.1,
    lat.min = 31.62, 
    lat.max = 68.97)
head(GNIP_Europe)
## uncomment the following to store the file:
# save(GNIP_Europe, file="GNIP_Europe.rda", compress="xz")


### CREATE ISOSCAPE-DATASET FOR WARM MONTHS IN EUROPE
GNIP_Europe_Warm <- QueryGNIP(
    data=GNIP_World,
    month.min = 5,  
    month.max = 8,
    year.min =  1960,
    year.max =  2013,
    long.min = -27.34, 
    long.max = 57.1,
    lat.min = 31.62, 
    lat.max = 68.97)

head(GNIP_Europe_Warm)


### CREATE A DATASET WITH 90% OF OBS
GNIP_World_90pct <- QueryGNIP(
    data=GNIP_World,
    prop.random=0.9,
    random.level = "obs")

lapply(GNIP_World_90pct, head) # show beginnng of both datasets

### CREATE A DATASET WITH HALF THE WEATHER STATIONS
GNIP_World_50pctStations <- QueryGNIP(
    data=GNIP_World,
    prop.random=0.5,
    random.level = "station")

lapply(GNIP_World_50pctStations, head)
    
}

