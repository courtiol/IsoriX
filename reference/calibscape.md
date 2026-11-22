# Builds calibrated isoscapes, i.e. isoscapes of samples (e.g. tissues)

This function uses a calibration fit to convert the mean isoscape of
source isotopic values (e.g. precipitation) into a calibrated isoscape,
i.e. the mean isoscape of the isotopic values expected in the samples
(e.g. animal tissues).

## Usage

``` r
calibscape(isoscape, calibfit)
```

## Arguments

- isoscape:

  The output of the function
  [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
  or
  [`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
  (i.e. an object of class *ISOSCAPE*)

- calibfit:

  The output of the function
  [`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
  (i.e. an object of class *CALIBFIT*)

## Value

This function returns a *list* of class *ISOSCAPE* and *CALIBSCAPE*
containing a *SpatRaster* with a single layer (`mean`) storing the
predicted mean isotopic values of the samples, and the location of the
sources and of the calibration samples (except for the calibration
method "desk") as spatial points.

## Details

For assignments, IsoriX converts the isotopic values of the samples to
assign (e.g. animal tissues) into isotopic values of the sources (e.g.
precipitation), and not the other way around (see
[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md)).
Yet, it can be useful to visualise or study what the isoscape would look
like in terms of sample values. This is what `calibscape()` does.

The calibration fit (see
[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md))
models the relationship
`sample_value = intercept + slope * source_value`, whatever the
calibration method used (for the method `"desk_inverse"`, the inversion
of the regression is already performed by
[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md)).
The mean isoscape is thus simply calibrated by computing, in each raster
cell, `intercept + slope * mean`, using the estimates of the intercept
and of the slope stored in the calibration fit.

Only the mean isoscape is converted. The variances (prediction, residual
and response variances) are not converted since their conversion would
require to account for the uncertainty of the calibration fit, which
depends on the calibration method being used. The isoscapes related to
the residual dispersion are not returned either.

The output can be plotted using
[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)
(with `which = "mean"` only; by default, the plot shows the location of
the calibration samples instead of that of the sources), but it cannot
be used for assignments
([`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md)) or
to simulate data
([`create_aliens`](https://courtiol.github.io/IsoriX/reference/create_aliens.md)),
since these functions require isoscapes of source isotopic values.

## See also

[`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
and
[`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
for the functions building the isoscapes of sources

[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
for the function fitting the calibration

[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)
for the function plotting isoscapes

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 30) {
  ## We fit the models
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)
  GermanFit <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  ## We build the isoscapes of sources (precipitation)
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  ## We define a calibration fit from published estimates
  CalibDataDesk <- data.frame(intercept = -10, slope = 0.8)
  CalibDesk <- calibfit(data = CalibDataDesk, method = "desk")

  ## We build the calibrated isoscape (i.e. the isoscape of tissues)
  GermanCalibScape <- calibscape(isoscape = GermanScape, calibfit = CalibDesk)

  GermanCalibScape
  plot(GermanCalibScape)
}
```
