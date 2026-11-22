# Filter and aggregate the raw source dataset

This function prepares the available dataset to be used for creating the
isoscape (e.g.
[`GNIPDataDE`](https://courtiol.github.io/IsoriX/reference/GNIPDataDE.md)).
This function allows the trimming of data by months, years and location,
and for the aggregation of selected data per location, location:month
combination or location:year combination. The function can also be used
to randomly exclude some observations.

## Usage

``` r
prepsources(
  data,
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
  col_year = "year"
)
```

## Arguments

- data:

  A *dataframe* containing raw isotopic measurements of sources

- month:

  A *numeric vector* indicating the months to select from. Should be a
  vector of round numbers between 1 and 12. The default is 1:12
  selecting all months.

- year:

  A *numeric vector* indicating the years to select from. Should be a
  vector of round numbers. The default is to select all years available.

- long_min:

  A *numeric* indicating the minimum longitude to select from. Should be
  a number between -180 and 180 (default = -180).

- long_max:

  A *numeric* indicating the maximal longitude to select from. Should be
  a number between -180 and 180 (default = 180). To select an area
  crossing the antimeridian (i.e. the International Date Line, 180
  degrees), use a value of `long_max` smaller than `long_min` (e.g.
  `long_min = 70` and `long_max = -150` selects longitudes from 70 to
  180 and from -180 to -150) or, equivalently, a value of `long_max`
  above 180 (e.g. `long_max = 210`).

- lat_min:

  A *numeric* indicating the minimum latitude to select from. Should be
  a number between -90 and 90 (default = -90).

- lat_max:

  A *numeric* indicating the maximal latitude to select from (default =
  90).

- split_by:

  A *string* indicating whether data should be aggregated per location
  (`split_by = NULL`, the default), per location:month combination
  (`split_by = "month"`), or per location:year combination
  (`split_by = "year"`).

- prop_random:

  A *numeric* indicating the proportion of observations or sampling
  locations (depending on the argument for `random_level`) that will be
  kept. If `prop_random` is greater than 0, then the function will
  return a list containing two dataframes: one containing the selected
  data, called `selected_data`, and one containing the remaining data,
  called `remaining_data`.

- random_level:

  A *string* indicating the level at which random draws can be
  performed. The two possibilities are `"obs"`, which indicates that
  observations are randomly drawn taken independently of their location,
  or "source" (default), which indicates that observations are randomly
  drawn at the level of sampling locations.

- col_source_value:

  A *string* indicating the column containing the isotopic measurements

- col_source_ID:

  A *string* indicating the column containing the ID of each sampling
  location

- col_lat:

  A *string* indicating the column containing the latitude of each
  sampling location

- col_long:

  A *string* indicating the column containing the longitude of each
  sampling location

- col_elev:

  A *string* indicating the column containing the elevation of each
  sampling location. This column is optional: if it is absent from
  `data`, the output will not contain any elevation (which is fine as
  long as elevation is not used as a predictor in
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)).

- col_month:

  A *string* indicating the column containing the month of sampling.
  This column is only required if `month` is provided or if
  `split_by = "month"`.

- col_year:

  A *string* indicating the column containing the year of sampling. This
  column is only required if `year` is provided or if
  `split_by = "year"`.

## Value

This function returns a *dataframe* containing the filtered data
aggregated by sampling location, or a *list*, see above argument
`prop_random`. For each sampling location the mean and variance sample
estimates are computed.

## Details

This function aggregates the data as required for the IsoriX workflow.
Three aggregation schemes are possible for now. The most simple one,
used as default, aggregates the data so to obtained a single row per
sampling location. Datasets prepared in this way can be readily fitted
with the function
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) to
build an isoscape. It is also possible to aggregate data in a different
way in order to build sub-isoscapes representing temporal variation in
isotope composition, or in order to produce isoscapes weighted by the
amount of precipitation (for isoscapes on precipitation data only). The
two possible options are to either split the data from each location by
month or to split them by year. This is set with the `split_by` argument
of the function. Datasets prepared in this way should be fitted with the
function
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md).

The function also allows the user to filter the sampling locations based
on time (years and/ or months) and space (locations given in geographic
coordinates, i.e. longitude and latitude) to calculate tailored
isoscapes matching e.g. the time of sampling and speeding up the model
fit by cropping/clipping a certain area. The dataframe produced by this
function can be used as input to fit the isoscape (see
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) and
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)).

## Examples

``` r
## Create a processed dataset for Germany
GNIPDataDEagg <- prepsources(data = GNIPDataDE)

head(GNIPDataDEagg)
#>       source_ID mean_source_value var_source_value n_source_value   lat  long
#> 1        ARKONA         -60.99231         247.8767            134 54.67 13.43
#> 2        ARTERN         -61.00653         510.5720            199 51.37 11.29
#> 3 BAD SALZUFLEN         -53.80770         310.1046            408 52.10  8.75
#> 4        BERLIN         -57.53477         395.6484            419 52.46 13.40
#> 5  BRAUNSCHWEIG         -52.73350         339.4752            420 52.29 10.44
#> 6      CUXHAVEN         -49.25271         221.6594            420 53.87  8.70
#>   elev
#> 1   42
#> 2  164
#> 3  135
#> 4   48
#> 5   81
#> 6    5

## Create a processed dataset for Germany per month
GNIPDataDEmonthly <- prepsources(
  data = GNIPDataDE,
  split_by = "month"
)

head(GNIPDataDEmonthly)
#>   source_ID mean_source_value var_source_value n_source_value   lat  long elev
#> 1    ARKONA         -69.13636        253.07997             11 54.67 13.43   42
#> 2    ARKONA         -73.05455        238.47221             11 54.67 13.43   42
#> 3    ARKONA         -65.60455        312.38313             11 54.67 13.43   42
#> 4    ARKONA         -63.04636        214.81135             11 54.67 13.43   42
#> 5    ARKONA         -51.00000         83.21076             11 54.67 13.43   42
#> 6    ARKONA         -46.87583        118.54494             12 54.67 13.43   42
#>   month
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
#> 6     6

## Create a processed dataset for Germany per year
GNIPDataDEyearly <- prepsources(
  data = GNIPDataDE,
  split_by = "year"
)

head(GNIPDataDEyearly)
#>   source_ID mean_source_value var_source_value n_source_value   lat  long elev
#> 1    ARKONA         -60.43429         223.2125              7 54.67 13.43   42
#> 2    ARKONA         -60.05917         218.0238             12 54.67 13.43   42
#> 3    ARKONA         -68.32500         362.1893             12 54.67 13.43   42
#> 4    ARKONA         -61.16583         152.8339             12 54.67 13.43   42
#> 5    ARKONA         -63.82917         395.7411             12 54.67 13.43   42
#> 6    ARKONA         -61.15083         260.2932             12 54.67 13.43   42
#>   year
#> 1 1997
#> 2 1998
#> 3 1999
#> 4 2000
#> 5 2001
#> 6 2002

## Create isoscape-dataset for warm months in germany between 1995 and 1996
GNIPDataDEwarm <- prepsources(
  data = GNIPDataDE,
  month = 5:8,
  year = 1995:1996
)

head(GNIPDataDEwarm)
#>                source_ID mean_source_value var_source_value n_source_value
#> 1          BAD SALZUFLEN         -49.57500        123.61929              8
#> 2                 BERLIN         -50.63125        132.25067              8
#> 3           BRAUNSCHWEIG         -36.10000         85.51143              8
#> 4               CUXHAVEN         -45.90000        156.42000              8
#> 5               EMMERICH         -42.67500        208.13071              8
#> 6 GARMISCH-PARTENKIRCHEN         -61.80000        304.84000              8
#>     lat  long elev
#> 1 52.10  8.75  135
#> 2 52.46 13.40   48
#> 3 52.29 10.44   81
#> 4 53.87  8.70    5
#> 5 51.83  6.25   43
#> 6 47.48 11.06  719


## Create a dataset with 90% of obs
GNIPDataDE90pct <- prepsources(
  data = GNIPDataDE,
  prop_random = 0.9,
  random_level = "obs"
)

lapply(GNIPDataDE90pct, head) # show beginning of both datasets
#> $selected_data
#>       source_ID mean_source_value var_source_value n_source_value   lat  long
#> 1        ARKONA         -60.99500         227.8885            122 54.67 13.43
#> 2        ARTERN         -59.83315         502.7186            178 51.37 11.29
#> 3 BAD SALZUFLEN         -53.85866         318.9852            367 52.10  8.75
#> 4        BERLIN         -57.83168         397.1308            388 52.46 13.40
#> 5  BRAUNSCHWEIG         -53.08577         344.1448            381 52.29 10.44
#> 6      CUXHAVEN         -48.75277         200.6369            383 53.87  8.70
#>   elev
#> 1   42
#> 2  164
#> 3  135
#> 4   48
#> 5   81
#> 6    5
#> 
#> $remaining_data
#>       source_ID mean_source_value var_source_value n_source_value   lat  long
#> 1        ARKONA         -60.96500         490.2803             12 54.67 13.43
#> 2        ARTERN         -70.95238         489.4836             21 51.37 11.29
#> 3 BAD SALZUFLEN         -53.35146         236.3626             41 52.10  8.75
#> 4        BERLIN         -53.81871         374.3032             31 52.46 13.40
#> 5  BRAUNSCHWEIG         -49.29205         288.3140             39 52.29 10.44
#> 6      CUXHAVEN         -54.42784         420.7041             37 53.87  8.70
#>   elev
#> 1   42
#> 2  164
#> 3  135
#> 4   48
#> 5   81
#> 6    5
#> 

## Create a dataset with half the weather sources
GNIPDataDE50pctsources <- prepsources(
  data = GNIPDataDE,
  prop_random = 0.5,
  random_level = "source"
)

lapply(GNIPDataDE50pctsources, head)
#> $selected_data
#>                source_ID mean_source_value var_source_value n_source_value
#> 1                 ARTERN         -61.00653         510.5720            199
#> 2          BAD SALZUFLEN         -53.80770         310.1046            408
#> 3           BRAUNSCHWEIG         -52.73350         339.4752            420
#> 4               CUXHAVEN         -49.25271         221.6594            420
#> 5                DRESDEN         -61.40682         358.4125             44
#> 6 GARMISCH-PARTENKIRCHEN         -81.81309         868.4164            431
#>     lat  long elev
#> 1 51.37 11.29  164
#> 2 52.10  8.75  135
#> 3 52.29 10.44   81
#> 4 53.87  8.70    5
#> 5 51.05 13.73  113
#> 6 47.48 11.06  719
#> 
#> $remaining_data
#>          source_ID mean_source_value var_source_value n_source_value   lat
#> 1           ARKONA         -60.99231         247.8767            134 54.67
#> 2           BERLIN         -57.53477         395.6484            419 52.46
#> 3         EMMERICH         -50.07772         238.7878            413 51.83
#> 4          FEHMARN         -54.48990         263.7677            198 54.52
#> 5         GOERLITZ         -61.62424         495.2380            198 51.16
#> 6 HOHENPEISSENBERG         -81.44665         646.7237            448 47.80
#>    long elev
#> 1 13.43   42
#> 2 13.40   48
#> 3  6.25   43
#> 4 11.06    3
#> 5 14.95  238
#> 6 11.01  977
#> 


## Create a dataset with half the weather sources split per month
GNIPDataDE50pctsourcesMonthly <- prepsources(
  data = GNIPDataDE,
  split_by = "month",
  prop_random = 0.5,
  random_level = "source"
)

lapply(GNIPDataDE50pctsourcesMonthly, head)
#> $selected_data
#>   source_ID mean_source_value var_source_value n_source_value   lat  long elev
#> 1    ARKONA         -69.13636        253.07997             11 54.67 13.43   42
#> 2    ARKONA         -73.05455        238.47221             11 54.67 13.43   42
#> 3    ARKONA         -65.60455        312.38313             11 54.67 13.43   42
#> 4    ARKONA         -63.04636        214.81135             11 54.67 13.43   42
#> 5    ARKONA         -51.00000         83.21076             11 54.67 13.43   42
#> 6    ARKONA         -46.87583        118.54494             12 54.67 13.43   42
#>   month
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
#> 6     6
#> 
#> $remaining_data
#>   source_ID mean_source_value var_source_value n_source_value   lat  long elev
#> 1    ARTERN         -79.14375         505.3986             16 51.37 11.29  164
#> 2    ARTERN         -77.76875         366.7183             16 51.37 11.29  164
#> 3    ARTERN         -71.66875         309.9410             16 51.37 11.29  164
#> 4    ARTERN         -57.36250         166.8225             16 51.37 11.29  164
#> 5    ARTERN         -49.46875         124.1410             16 51.37 11.29  164
#> 6    ARTERN         -40.32353         105.3369             17 51.37 11.29  164
#>   month
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
#> 6     6
#> 
```
