#' Filter and aggregate the raw source dataset
#'
#' This function prepares the available dataset to be used for creating the
#' isoscape (e.g. [`GNIPDataDE`]). This function allows the trimming of data
#' by months, years and location, and for the aggregation of selected data per
#' location, location:month combination or location:year combination. The
#' function can also be used to randomly exclude some observations.
#'
#' This function aggregates the data as required for the IsoriX workflow. Three
#' aggregation schemes are possible for now. The most simple one, used as
#' default, aggregates the data so to obtained a single row per sampling
#' location. Datasets prepared in this way can be readily fitted with the
#' function [`isofit`] to build an isoscape. It is also possible to
#' aggregate data in a different way in order to build sub-isoscapes
#' representing temporal variation in isotope composition, or in order to
#' produce isoscapes weighted by the amount of precipitation (for isoscapes on
#' precipitation data only). The two possible options are to either split the
#' data from each location by month or to split them by year. This is set with
#' the `split_by` argument of the function. Datasets prepared in this way
#' should be fitted with the function [`isomultifit`].
#'
#' The function also allows the user to filter the sampling locations based on
#' time (years and/ or months) and space (locations given in geographic
#' coordinates, i.e. longitude and latitude) to calculate tailored isoscapes
#' matching e.g. the time of sampling and speeding up the model fit by
#' cropping/clipping a certain area. The dataframe produced by this function can
#' be used as input to fit the isoscape (see [`isofit`] and
#' [`isomultifit`]).
#'
#' @param data A *dataframe* containing raw isotopic measurements of sources
#' @param month A *numeric vector* indicating the months to select from.
#'   Should be a vector of round numbers between 1 and 12. The default is 1:12
#'   selecting all months.
#' @param year A *numeric vector* indicating the years to select from.
#'   Should be a vector of round numbers. The default is to select all years
#'   available.
#' @param long_min A *numeric* indicating the minimum longitude to select
#'   from. Should be a number between -180 and 180 (default = -180).
#' @param long_max A *numeric* indicating the maximal longitude to select
#'   from. Should be a number between -180 and 180 (default = 180).
#' @param lat_min A *numeric* indicating the minimum latitude to select
#'   from. Should be a number between -90 and 90 (default = -90).
#' @param lat_max A *numeric* indicating the maximal latitude to select
#'   from (default = 90).
#' @param split_by A *string* indicating whether data should be aggregated
#'   per location (`split_by = NULL`, the default), per location:month
#'   combination (`split_by = "month"`), or per location:year combination
#'   (\code{split_by = "year"}).
#' @param prop_random A *numeric* indicating the proportion of observations
#'   or sampling locations (depending on the argument for `random_level`)
#'   that will be kept. If `prop_random` is greater than 0, then the
#'   function will return a list containing two dataframes: one containing the
#'   selected data, called `selected_data`, and one containing the
#'   remaining data, called `remaining_data`.
#' @param random_level A *string* indicating the level at which random draws
#'   can be performed. The two possibilities are `"obs"`, which indicates
#'   that observations are randomly drawn taken independently of their location,
#'   or "source" (default), which indicates that observations are randomly drawn
#'   at the level of sampling locations.
#' @param col_source_value A *string* indicating the column containing the
#'   isotopic measurements
#' @param col_source_ID A *string* indicating the column containing the ID of
#'   each sampling location
#' @param col_lat A *string* indicating the column containing the latitude
#'   of each sampling location
#' @param col_long A *string* indicating the column containing the longitude
#'   of each sampling location
#' @param col_elev A *string* indicating the column containing the elevation
#'   of each sampling location
#' @param col_month A *string* indicating the column containing the month of
#'   sampling
#' @param col_year A *string* indicating the column containing the year of
#'   sampling
#' @return This function returns a *dataframe* containing the filtered data
#'   aggregated by sampling location, or a *list*, see above argument
#'   `prop_random`. For each sampling location the mean and variance sample
#'   estimates are computed.
#' @examples
#' ## Create a processed dataset for Germany
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#' head(GNIPDataDEagg)
#'
#' ## Create a processed dataset for Germany per month
#' GNIPDataDEmonthly <- prepsources(
#'   data = GNIPDataDE,
#'   split_by = "month"
#' )
#'
#' head(GNIPDataDEmonthly)
#'
#' ## Create a processed dataset for Germany per year
#' GNIPDataDEyearly <- prepsources(
#'   data = GNIPDataDE,
#'   split_by = "year"
#' )
#'
#' head(GNIPDataDEyearly)
#'
#' ## Create isoscape-dataset for warm months in germany between 1995 and 1996
#' GNIPDataDEwarm <- prepsources(
#'   data = GNIPDataDE,
#'   month = 5:8,
#'   year = 1995:1996
#' )
#'
#' head(GNIPDataDEwarm)
#'
#'
#' ## Create a dataset with 90% of obs
#' GNIPDataDE90pct <- prepsources(
#'   data = GNIPDataDE,
#'   prop_random = 0.9,
#'   random_level = "obs"
#' )
#'
#' lapply(GNIPDataDE90pct, head) # show beginning of both datasets
#'
#' ## Create a dataset with half the weather sources
#' GNIPDataDE50pctsources <- prepsources(
#'   data = GNIPDataDE,
#'   prop_random = 0.5,
#'   random_level = "source"
#' )
#'
#' lapply(GNIPDataDE50pctsources, head)
#'
#'
#' ## Create a dataset with half the weather sources split per month
#' GNIPDataDE50pctsourcesMonthly <- prepsources(
#'   data = GNIPDataDE,
#'   split_by = "month",
#'   prop_random = 0.5,
#'   random_level = "source"
#' )
#'
#' lapply(GNIPDataDE50pctsourcesMonthly, head)
#'
#' @export
prepsources <- function(data,
                        month = 1:12,
                        year,
                        long_min = -180,
                        long_max = 180,
                        lat_min = -90,
                        lat_max = 90,
                        split_by = NULL,
                        prop_random = 0,
                        random_level = "source",
                        col_source_value = "source_value",
                        col_source_ID = "source_ID",
                        col_lat = "lat",
                        col_long = "long",
                        col_elev = "elev",
                        col_month = "month",
                        col_year = "year") {
  ## Some checks
  if (any(month %% 1 != 0) || any(month < 1) || any(month > 12)) {
    stop("Months must be provided as a vector of integers and should be between 1 and 12.")
  }

  if (prop_random > 1) {
    stop("The value you entered for prop_random is > 1. It must be a proportion (so between 0 and 1)!")
  }

  ### Check that source_IDs correspond to unique locations
  data$.location <- paste(data[, col_lat, drop = TRUE], data[, col_long, drop = TRUE], data[, col_elev, drop = TRUE])
  test_unique_locations1 <- tapply(data$.location, as.character(data[, col_source_ID, drop = TRUE]), \(x) length(unique(x)) == 1)
  if (!all(test_unique_locations1)) {
    issues <- names(test_unique_locations1[!test_unique_locations1])
    warning(c(paste("Different combinations of latitude, longitude and elevation seem to share the same source_ID. Please check and fix the data for the IDs:\n"),
              paste(issues, collapse  = ", ")))
    rm(issues)
  }
  test_unique_locations2 <- tapply(as.character(data[, col_source_ID, drop = TRUE]), data$.location, \(x) length(unique(x)) == 1)
  if (!all(test_unique_locations2)) {
    issues_locations <- names(test_unique_locations2[!test_unique_locations2])
    issues_ID <- unique(as.character(data[, col_source_ID, drop = TRUE])[data$.location %in% issues_locations])
    warning(c(paste("The same combination of latitude, longitude and elevation seem to correspond to different source_ID. Please check and fix the data for the IDs:\n"),
              paste(issues_ID, collapse  = ", "),
              paste("\nThese IDs correspond to the following sets of", col_lat, col_long, col_elev, ":\n"),
              paste(issues_locations, collapse  = ", ")))
    rm(issues_ID)
    rm(issues_locations)
  }
  data$.location <- NULL

  ## Handle missing data
  if (missing("year")) year <- sort(unique(data[, col_year, drop = TRUE], na.rm = TRUE))

  ## Handle the month column and convert all months to numbers
  data[, col_month] <- .converts_months_to_numbers(data[, col_month, drop = TRUE])

  ## Prepare selection
  month_select <- data[, col_month, drop = TRUE] %in% month
  year_select <- data[, col_year, drop = TRUE] %in% year
  long_select <- data[, col_long, drop = TRUE] >= long_min & data[, col_long, drop = TRUE] <= long_max
  lat_select <- data[, col_lat, drop = TRUE] >= lat_min & data[, col_lat, drop = TRUE] <= lat_max
  all_select <- month_select & year_select & long_select & lat_select

  ## Apply selection
  query_data <- data[all_select, , drop = TRUE]

  ## Defining function returning unique values with test
  unique2 <- function(x, key) {
    u_x <- unique(x)
    if (length(u_x) == 1) {
      return(u_x)
    }
    warning(paste(c("Some", key, "values are not unique but should be, so the first element was taken among:", u_x, "."), collapse = " "))
    return(u_x[1])
  }

  ## Defining function for aggregation
  aggregate_data <- function(d, split_by = split_by) {
    d <- droplevels(d)

    ## Create the variable used for the split
    if (is.null(split_by)) {
      split <- d[, col_source_ID, drop = TRUE]
    } else if (split_by == "month") {
      split <- paste(d[, col_source_ID, drop = TRUE], d[, col_month, drop = TRUE], sep = "_")
    } else if (split_by == "year") {
      split <- paste(d[, col_source_ID, drop = TRUE], d[, col_year, drop = TRUE], sep = "_")
    } else {
      stop("The argument used for 'split_by' is unknown.")
    }

    ## Perform the aggregation
    df <- data.frame(
      split = factor(c(tapply(as.character(split), split, unique2, key = "split"))),
      source_ID = factor(c(tapply(as.character(d[, col_source_ID, drop = TRUE]), split, unique2, key = "source_ID"))),
      mean_source_value = c(tapply(d[, col_source_value, drop = TRUE], split, mean, na.rm = TRUE)),
      var_source_value = c(tapply(d[, col_source_value, drop = TRUE], split, stats::var, na.rm = TRUE)),
      n_source_value = c(tapply(d[, col_source_ID, drop = TRUE], split, length)),
      lat = c(tapply(d[, col_lat, drop = TRUE], split, unique2, key = "latitude")),
      long = c(tapply(d[, col_long, drop = TRUE], split, unique2, key = "longitude")),
      elev = c(tapply(d[, col_elev, drop = TRUE], split, unique2, key = "elevation"))
    )
    ## Note that above the c() prevent the creation of 1d arrays that are troublesome in spaMM

    null.var <- !is.na(df$var_source_value) & df$var_source_value == 0
    if (sum(null.var) > 0) {
      df$var_source_value[null.var] <- 0.01
      warnings(paste(length(null.var), "Null variances were obtained during aggregation. They were changed to 0.01 assuming that the actual variance cannot be smaller than the measurement error variance."))
    }

    ## Retrieve the relevant splitting information
    if (is.null(split_by)) {
      df <- df[order(df$source_ID), ]
    } else if (split_by == "month") {
      df$month <- as.numeric(unlist(lapply(strsplit(as.character(df$split), split = "_", fixed = TRUE), function(i) i[2])))
      df <- df[order(df$source_ID, df$month), ]
    } else if (split_by == "year") {
      df$year <- as.numeric(unlist(lapply(strsplit(as.character(df$split), split = "_", fixed = TRUE), function(i) i[2])))
      df <- df[order(df$source_ID, df$year), ]
    }

    ## Clean-up and output
    df$split <- NULL
    df <- droplevels(df[!is.na(df$mean_source_value), ])
    rownames(df) <- NULL
    return(df)
  }

  ## Return aggregated data if no random selection is needed
  if (prop_random == 0) {
    return(aggregate_data(query_data, split_by = split_by))
  }

  ## Random draw of observations
  if (random_level == "obs") {
    howmanylines <- round(prop_random * nrow(query_data))
    whichlines <- sample(x = nrow(query_data), size = howmanylines, replace = FALSE)
    selected_data <- query_data[whichlines, ]
    remaining_data <- query_data[-whichlines, ]
    return(list(
      selected_data = aggregate_data(selected_data, split_by = split_by),
      remaining_data = aggregate_data(remaining_data, split_by = split_by)
    ))
  }

  ## Random draw of source_ID
  if (random_level == "source") {
    howmanysources <- round(prop_random * length(unique(query_data[, col_source_ID])))
    whichssources <- sample(x = unique(query_data[, col_source_ID]), size = howmanysources, replace = FALSE)
    dolines <- query_data[, col_source_ID] %in% whichssources
    selected_data <- query_data[dolines, ]
    remaining_data <- query_data[!dolines, ]
    return(list(
      selected_data = aggregate_data(selected_data, split_by = split_by),
      remaining_data = aggregate_data(remaining_data, split_by = split_by)
    ))
  }

  ## Display error if no return encountered before
  stop("The argument you chose for random_level is unknown.")
}
