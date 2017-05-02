#' @rdname IsoriX-defunct
QueryGNIP <- function(...) {
  .Defunct("queryGNIP")
}



#' Filter the dataset to create an isoscape
#' 
#' This function prepares the worldwide GNIP data (\var{GNIPdata}) to be used
#' for creating the isoscape. This function allows the trimming of data by
#' months, years and location, and for the aggregation of selected data per
#' weather station. The function can also be used to randomly exclude some
#' observations.
#' 
#' This function aggregates the data per location as required for the IsoriX
#' workflow. The function also allows the user to filter the world-wide weather
#' station data (\var{GNIPdata}) based on time (years and/ or months) and space
#' (locations given in geographic coordinates, i.e. longitude and latitude) to
#' calculate tailored isoscapes matching e.g. the time of sampling and speeding
#' up the model fit by cropping/ clipping a certain area. The dataframe
#' produced by this function can be used as input to fit the isoscape (see
#' \code{\link{isofit}}).
#' 
#' @param data A \var{dataframe} containing original isotopic measurements
#' similar in structure to \code{\link{GNIPdata}}
#' @param month.min A \var{numeric} indicating the minimum month to select
#' from. Should be a round number between 1 and 12. The default value is 1
#' (January).
#' @param month.max A \var{numeric} indicating the maximum month to select
#' from. Should be a round number between 1 and 12.  The default value is 12
#' (December).
#' @param year.min A \var{numeric} indicating the oldest year to select from.
#' If not provided, the oldest year of \code{GNIPdata} will be considered
#' @param year.max A \var{numeric} indicating the most recent year to select
#' from.  If not provided, the most recent year of \code{\link{GNIPdata}} will
#' be considered
#' @param long.min A \var{numeric} indicating the minimum longitude to select
#' from. Should be a number between -180 and 180. If not provided, -180 will be
#' considered.
#' @param long.max A \var{numeric} indicating the maximal longitude to select
#' from. Should be a number between -180 and 180. If not provided, 180 will be
#' considered.
#' @param lat.min A \var{numeric} indicating the minimum latitude to select
#' from. Should be a number between -90 and 90. If not provided, -90 will be
#' considered.
#' @param lat.max A \var{numeric} indicating the maximal latitude to select
#' from. Should be a number between -90 and 90. If not provided, 90 will be
#' considered.
#' @param prop.random A \var{numeric} indicating the proportion of observations
#' or weather stations (depending on the argument for \code{random.level}) that
#' will be kept. If \code{prop.random} is greater than 0, then the function
#' will return a list containing two dataframes: one containing the selected
#' data, called \code{selected.data}, and one containing the remaining data,
#' called \code{remaining.data}.
#' @param random.level A \var{string} indicating the level at which random
#' draws can be performed. The two possibilities are \code{"obs"}, which
#' indicates that observations are randomly drawn taken independently of their
#' location, or "station" (default), which indicates that observations are
#' randomly drawn at the level of weather stations.
#' @return This function returns a \var{dataframe} containing the filtered data
#' aggregated by weather station, or a \var{list}, see above argument
#' \code{prop.random}. For each weather station the mean and variance sample
#' estimates are computed.
#' @note If one wants e.g. to select the winter months November to February,
#' one has to select 11 as minimum and 2 as maximum and not the opposite.
#' @seealso \code{\link{IsoriX}} for the complete work-flow
#' \code{\link{GNIPdata}} for the complete dataset
#' @examples
#' 
#' data(GNIPdata)
#' 
#' ### CREATE A PROCESSED DATASET FOR EUROPE
#' GNIPdataEU <- queryGNIP(
#'     data = GNIPdata,
#'     long.min = -30, 
#'     long.max = 60,
#'     lat.min = 30, 
#'     lat.max = 70)
#' 
#' head(GNIPdataEU)
#' 
#' ### CREATE ISOSCAPE-DATASET FOR WARM MONTHS IN EUROPE
#' GNIPdataEU_Warm <- queryGNIP(
#'     data=GNIPdata,
#'     month.min = 5,  
#'     month.max = 8,
#'     year.min = 1960,
#'     year.max = 2013,
#'     long.min = -30, 
#'     long.max = 60,
#'     lat.min = 30, 
#'     lat.max = 70)
#' 
#' head(GNIPdataEU_Warm)
#' 
#' 
#' ### CREATE A DATASET WITH 90% OF OBS
#' GNIPdata_90pct <- queryGNIP(
#'     data = GNIPdata,
#'     prop.random = 0.9,
#'     random.level = "obs")
#' 
#' lapply(GNIPdata_90pct, head) # show beginnng of both datasets
#' 
#' ### CREATE A DATASET WITH HALF THE WEATHER STATIONS
#' GNIPdata_50pctStations <- queryGNIP(
#'     data = GNIPdata,
#'     prop.random = 0.5,
#'     random.level = "station")
#' 
#' lapply(GNIPdata_50pctStations, head)
#'     
#' 
#' @export queryGNIP
queryGNIP <- function(data,
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
                      ) {

  ## Some checks
  if ((month.min %% 1 != 0) | (month.max %% 1 != 0)) {
    stop("Months must be provided as a round numeric values or as an integer.")
  }

  if (prop.random>1) {
    stop("The value you entered for prop.random is > 1. It must be a proportion (so between 0 and 1)!")
  }

  ## Handle missing data
  if (missing("year.min")) year.min <- min(data$year, na.rm=TRUE)
  if (missing("year.max")) year.max <- max(data$year, na.rm=TRUE)
  if (missing("long.min")) long.min <- -180
  if (missing("long.max")) long.max <- 180
  if (missing("lat.min")) lat.min <- -90
  if (missing("lat.max")) lat.max <- 90 

  ## Prepare selection
  if (month.max < month.min) {
    month.select <- data$month >= month.min | data$month <= month.max
  } else {
    month.select <- data$month >= month.min & data$month <= month.max
  }

  year.select <- data$year >= year.min & data$year >= year.min
  long.select <- data$long >= long.min & data$long <= long.max
  lat.select  <- data$lat  >= lat.min  & data$lat  <= lat.max
  all.select <-  month.select & year.select & long.select & lat.select
  
  ## Apply selection
  query.data <- data[all.select, ]

  ## Defining function returning unique values with test
  unique2 <- function(x, key) {
    u.x <- unique(x)
    if (length(u.x) == 1) return(u.x)
    warning(paste(c("some", key, "values are not unique but should be, so the first element was taken among:", u.x), collapse = " "))
    return(u.x[1])
  }

  ## Defining function for aggregation
  aggregate.data <- function(d) {
    d <- droplevels(d)
    df <- data.frame(stationID = levels(d$stationID),
                     isoscape.value = c(tapply(d$isoscape.value, d$stationID, mean, na.rm = TRUE)),
                     var.isoscape.value = c(tapply(d$isoscape.value, d$stationID, var, na.rm = TRUE)),
                     n.isoscape.value = c(tapply(d$stationID, d$stationID, length)),
                     lat = c(tapply(d$lat, d$stationID, unique2, key = "latitude")),
                     long = c(tapply(d$long, d$stationID, unique2, key = "longitude")),
                     elev = c(tapply(d$elev, d$stationID, unique2, key = "elevation")))
    ## Note that above the c() prevent the creation of 1d arrays that are troublesome in spaMM
    if (any(df$n.isoscape.value == 1)) {
      warnings("station presenting a single observation have been discarded during aggregation")
      df <- df[df$n.isoscape.value>1, ]
    }
    df <- droplevels(df)
    rownames(df) <- NULL
    return(df)
  }

  ## Return aggregated data if no random selection is needed
  if (prop.random == 0) {
    return(aggregate.data(query.data))
  }
 
  ## Random draw of observations
  if (random.level == "obs") {
    howmanylines <- round(prop.random * nrow(query.data))
    whichlines <- sample(x = nrow(query.data), size = howmanylines, replace = FALSE)
    selected.data <- query.data[whichlines, ] 
    remaining.data <- query.data[-whichlines, ]
    return(list(selected.data = aggregate.data(selected.data),
                remaining.data = aggregate.data(remaining.data)
                )
    )
  } 

  ## Random draw of stationID
  if (random.level == "station") {
    howmanystations <- round(prop.random * length(unique(query.data$stationID)))
    whichsstations <- sample(x = unique(query.data$stationID), size = howmanystations, replace = FALSE)
    whichlines <- query.data$stationID %in% whichsstations
    selected.data <- query.data[whichlines, ] 
    remaining.data <- query.data[-whichlines, ]
    return(list(selected.data = aggregate.data(selected.data),
                remaining.data = aggregate.data(remaining.data))
    )
  }

  ## Display error if no return encountered before
  stop("the argument you chose for random.level is unknown.")

}
