# Prepare the structural raster

This function prepares the structural raster for the follow-up analyses.
The size and extent of the structural raster defines the resolution at
which the isoscapes and the assignments are defined.

## Usage

``` r
prepraster(
  raster,
  isofit = NULL,
  margin_pct = 5,
  aggregation_factor = 0L,
  aggregation_fn = mean,
  manual_crop = NULL,
  values_to_zero = c(-Inf, 0),
  verbose = interactive()
)
```

## Arguments

- raster:

  The structural raster (*SpatRaster*)

- isofit:

  The fitted isoscape model returned by the function
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)

- margin_pct:

  The percentage representing by how much the area should extend outside
  the area used for cropping (default = 5, corresponding to 5%). Set to
  0 if you want exact cropping.

- aggregation_factor:

  The number of neighbouring cells (*integer*) to merge during
  aggregation

- aggregation_fn:

  The *function* used to aggregate cells

- manual_crop:

  A vector of four coordinates (*numeric*) for manual cropping, e.g. the
  spatial extent

- values_to_zero:

  A *numeric vector* of length two specifying the range of values for
  the structural raster that must be turned into 0. Default is
  `c(-Inf, 0)` which for an elevation raster brings all seas to an
  elevation of zero. For using IsoriX for marine organisms, you should
  use `c(0, Inf)` instead.

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session, and
  `FALSE` otherwise.

## Value

The prepared structural raster of class *SpatRaster*

## Details

This functions allows the user to crop a raster according to either the
extent of the isoscape or manually. If a fitted isoscape object is
provided (see
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)), the
function extracts the observed locations of isotopic sources from the
model object and crops the structural raster accordingly. Alternatively,
`manual_crop` allows you to crop the structural raster to a desired
extent. If no model and no coordinates for manual cropping are provided,
no crop will be performed. Importantly, cropping is recommended as it
prevents extrapolations outside the latitude/longitude range of the
source data. Predicting outside the range of the source data may lead to
highly unreliable predictions.

Aggregation changes the spatial resolution of the raster, making
computation faster and using less memory (this can affect the
assignment; see note below). An aggregation factor of zero (or one)
keeps the resolution constant (default).

This function relies on calls to the functions
[`terra::aggregate`](https://rspatial.github.io/terra/reference/aggregate.html)
and
[`terra::crop`](https://rspatial.github.io/terra/reference/crop.html)
from the package terra.

Areas crossing the antimeridian (i.e. the International Date Line, 180
degrees), such as areas centred on the Pacific, are supported provided
that the structural raster covers all longitudes (e.g. a world elevation
raster downloaded using
[`getelev`](https://courtiol.github.io/IsoriX/reference/getelev.md)).
Such areas can be defined using `manual_crop` with a first longitude
greater than the second one (e.g. `manual_crop = c(70, -150, -50, 70)`)
or with a second longitude above 180 (e.g.
`manual_crop = c(70, 210, -50, 70)`). If cropping is done according to a
fitted isoscape model, crossing the antimeridian is automatic when the
sources straddle it (see
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)). In
all these cases, the longitudes of the prepared raster are expressed
continuously (e.g. from 70 to 210) instead of within -180 and 180. All
the functions of IsoriX handle this representation, including the
plotting functions which automatically shift country borders, masks and
points accordingly. If you add layers yourself on such maps, you may
need to shift them using
[`terra::shift`](https://rspatial.github.io/terra/reference/shift.html)
(see example below).

## Note

Aggregating the raster may lead to different results for the assignment,
because the values of raster cells changes depending on the aggregation
function (see example below), which in turn affects model predictions.

## See also

[`ElevRasterDE`](https://courtiol.github.io/IsoriX/reference/ElevRasterDE.md)
for information on elevation rasters, which can be used as structural
rasters.

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 30) {
  ## We fit the models for Germany
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  GermanFit <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  ### Let's explore the difference between aggregation schemes

  ## We aggregate and crop using different settings
  ElevationRaster1 <- prepraster(
    raster = ElevRasterDE,
    isofit = GermanFit,
    margin_pct = 0,
    aggregation_factor = 0
  )

  ElevationRaster2 <- prepraster(
    raster = ElevRasterDE,
    isofit = GermanFit,
    margin_pct = 5,
    aggregation_factor = 5
  )

  ElevationRaster3 <- prepraster(
    raster = ElevRasterDE,
    isofit = GermanFit,
    margin_pct = 10,
    aggregation_factor = 5, aggregation_fn = max
  )

  ## We plot the outcome of the 3 different aggregation schemes using terra

  oripar <- par(mfrow = c(1, 3)) ## display 3 plots side-by-side

  plot(ElevationRaster1, main = "Original small raster")
  polys(CountryBorders)
  polys(OceanMask, col = "blue")

  plot(ElevationRaster2, main = "Small raster aggregated (by mean)")
  polys(CountryBorders)
  polys(OceanMask, col = "blue")

  plot(ElevationRaster3, main = "Small raster aggregated (by max)")
  polys(CountryBorders)
  polys(OceanMask, col = "blue")

  par(oripar) ## restore graphical settings
}

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 10) {
  ### Let's create a raster centered around the pacific

  ## We first create an empty raster
  EmptyRaster <- rast(matrix(0, ncol = 360, nrow = 180))
  ext(EmptyRaster) <- c(-180, 180, -90, 90)
  crs(EmptyRaster) <- "+proj=longlat +datum=WGS84"

  ## We crop it around the pacific
  PacificA <- prepraster(EmptyRaster, manual_crop = c(110, -70, -90, 90))
  ext(PacificA) # note that the extent has changed!

  ## We plot (note the use of the function shift()!)
  plot(PacificA, col = "blue", legend = FALSE)
  polys(CountryBorders, col = "black")
  polys(shift(CountryBorders, dx = 360), col = "black")
}
```
