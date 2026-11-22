# Predicts the average spatial distribution of isotopic values over months, years...

This function is the counterpart of
[`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
for the objects created with
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md).
It creates the isoscapes for each strata (e.g. month) defined by
`split_by` during the call to
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)
and the aggregate them. The function can handle weighting for the
aggregation process and may thus be used to predict annual averages
precipitation weighted isoscapes.

## Usage

``` r
isomultiscape(
  raster,
  isofit,
  weighting = NULL,
  Ncpu = getOption_IsoriX("Ncpu"),
  verbose = interactive()
)
```

## Arguments

- raster:

  The structural raster (*SpatRaster*) such as an elevation raster
  created using
  [`prepelev`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)

- isofit:

  The fitted isoscape created by
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)

- weighting:

  An optional RasterBrick containing the weights

- Ncpu:

  An *integer* specifying the number of CPUs to use, i.e. the number of
  isoscapes built in parallel (default set by the global package option
  `Ncpu`, see
  [`options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md)).
  Use `Ncpu = 1` to build the isoscapes sequentially (see **Parallel
  computing** in Details).

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session and
  `FALSE` otherwise.

## Value

This function returns a *list* of class *ISOSCAPE* containing a set of
all 8 raster layers mentioned above (all being of class *SpatRaster*),
and the location of the sources as spatial points.

## Details

**Parallel computing**

The isoscapes of the different strata are built independently from each
other. They can therefore be built in parallel, using several CPUs,
which is done when `Ncpu` is larger than 1 (by default `Ncpu` is set by
the global option `Ncpu` of IsoriX, see
[`options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md)).
As for
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
parallel computing relies on socket clusters created by the base package
parallel, which works on Windows, macOS and Linux. Each of the `Ncpu`
CPUs used runs a new R session, which takes a few seconds to start and
requires its own memory: for large rasters, the memory needed is roughly
multiplied by `Ncpu`, so you may have to decrease `Ncpu` (or the global
option `spaMM_blocksize`, see
[`options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md))
if you run out of memory. The isoscapes are identical whether they are
produced sequentially or in parallel.

## See also

[`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
for details on the function used to compute the isoscapes for each
strata
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)
for the function fitting the isoscape

[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)
for the function plotting the isoscape model

[`IsoriX`](https://courtiol.github.io/IsoriX/reference/IsoriX-package.md)
for the complete work-flow

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 180) {
  ## We prepare the data and split them by month:

  GNIPDataDEmonthly <- prepsources(
    data = GNIPDataDE,
    split_by = "month"
  )

  dim(GNIPDataDEmonthly)

  ## We fit the isoscapes:#'
  GermanMultiFit <- isomultifit(
    data = GNIPDataDEmonthly,
    mean_model_fix = list(elev = TRUE, lat.abs = TRUE)
  )

  ## We build the annual isoscapes by simple averaging (equal weighting):
  GermanMultiscape <- isomultiscape(
    raster = ElevRasterDE,
    isofit = GermanMultiFit
  )

  ## We build the annual isoscapes with a weighting based on precipitation amount:
  GermanMultiscapeWeighted <- isomultiscape(
    raster = ElevRasterDE,
    isofit = GermanMultiFit,
    weighting = PrecipBrickDE
  )

  ## We plot the mean isoscape of the averaging with equal weighting:
  plot(x = GermanMultiscape, which = "mean")

  ## We plot the mean isoscape of the averaging with precipitation weighting:
  plot(x = GermanMultiscapeWeighted, which = "mean")

  ## We build the isoscapes for a given month (here January):
  GermanScapeJan <- isoscape(
    raster = ElevRasterDE,
    isofit = GermanMultiFit$multi_fits[["month_1"]]
  )

  ## We plot the mean isoscape for January:
  plot(x = GermanScapeJan, which = "mean")
}
```
