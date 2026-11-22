# Map the aggregated source data

This function draws maps showing, for each source location (e.g. weather
station), the mean, the standard deviation and the number of isotopic
measurements stored in aggregated source data (as produced by
[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)).
Such maps help to explore the source data before fitting the isoscape:
e.g. to see where sources are, which areas are poorly covered, which
locations rely on few measurements, or which values look unusual.

## Usage

``` r
plot_sources(
  data,
  which = c("mean", "sd", "n"),
  labels = FALSE,
  points = list(pch = 21, cex = 1, lwd = 0.5, col = "black", digits = 1),
  borders = list(borders = NA, lwd = 0.5, col = "darkgrey"),
  palette = list(step = NA, range = c(NA, NA), n_labels = 11, digits = 2, fn = NA),
  plot = TRUE,
  ask = length(which) > 1 && grDevices::dev.interactive()
)
```

## Arguments

- data:

  A *dataframe* containing aggregated source data, as produced by
  [`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
  (without splitting the data, i.e. with a single row per source
  location)

- which:

  A *vector of strings* indicating which statistics to map, among
  `"mean"` (mean of the isotopic values), `"sd"` (standard deviation of
  the isotopic values) and `"n"` (number of measurements). By default,
  all three are mapped, one after the other.

- labels:

  A *logical* indicating whether the values should be written on the
  maps instead of being represented by coloured points (default =
  `FALSE`)

- points:

  A *list* containing information for the display of the points or
  labels (see details)

- borders:

  A *list* containing information for the display of borders (see
  details)

- palette:

  A *list* containing information for the display of the colours (see
  details)

- plot:

  A *logical* indicating whether the maps should be drawn (default =
  `TRUE`); if `FALSE`, the maps are only returned

- ask:

  A *logical* indicating whether the user should be asked to press
  before each new map. By default, this is the case when several maps
  are drawn on an interactive graphic device.

## Value

A named *list* of maps (objects of class *trellis*, one per statistic),
returned invisibly. Each map can be drawn again using
[`print`](https://rdrr.io/r/base/print.html).

## Details

Each statistic is shown on its own map. When several maps are drawn in
an interactive session, you are asked to press before each new map (as
for [`plot()`](https://courtiol.github.io/IsoriX/reference/plots.md)
called on the output of [`stats::lm`](https://rdrr.io/r/stats/lm.html));
see the argument `ask`. By default, the value of each source location is
represented by the colour of a point (see the colour key on the right
side of each map). For datasets containing few source locations, you can
instead write the values on the maps by setting `labels = TRUE`.

The arguments `points`, `borders` and `palette` are lists which can be
used to fine-tune the maps, in the same way as for the other plotting
functions of IsoriX (see
[`plots`](https://courtiol.github.io/IsoriX/reference/plots.md)). The
argument `points` controls the display of the points (`pch`, `cex`,
`lwd`, and `col` for the colour of their outline) or of the labels
(`cex`, `col` and `digits` for the number of decimals). The argument
`borders` controls the display of the borders (`borders`, `lwd`, `col`):
by default, the country borders provided with IsoriX
([`CountryBorders`](https://courtiol.github.io/IsoriX/reference/CountryBorders.md))
are drawn, use `borders = list(borders = NULL)` to draw none. The
argument `palette` controls the colours (`fn`, `step`, `range`,
`n_labels`, `digits`), as for
[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md);
each map uses its own colour scale.

Source locations straddling the International Date Line are drawn on a
continuous map (e.g. from 70 to 210 degrees of longitude).

## See also

[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
for the function aggregating the source data

[`plots`](https://courtiol.github.io/IsoriX/reference/plots.md) for the
other plotting functions of IsoriX

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 10) {
  ## Aggregate the source data for Germany
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## Map the mean, standard deviation and number of measurements
  plot_sources(GNIPDataDEagg)

  ## Same, writing the values instead (better for few source locations)
  plot_sources(GNIPDataDEagg, labels = TRUE)

  ## Map only the number of measurements, with another palette
  plot_sources(GNIPDataDEagg, which = "n", palette = list(fn = NULL))

  ## Map the means for the European data
  plot_sources(GNIPDataEUagg, which = "mean")
}
```
