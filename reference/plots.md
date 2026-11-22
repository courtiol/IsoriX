# Plotting functions for IsoriX

These functions plot objects created by IsoriX (with the exception of
plot method for SpatRaster created using
[`terra::terra`](https://rspatial.github.io/terra/reference/terra-package.html).
All plotting functions are based on the powerful package lattice. If
instead you want to use ggplot2, please follow the instructions on the
[online
tutorial](https://courtiol-isorix.share.connect.posit.cloud/advanced.html#ggplot).

## Usage

``` r
# S3 method for class 'ISOSCAPE'
plot(
  x,
  which = "mean",
  y_title = list(which = TRUE, title = getOption_IsoriX("title_delta_notation")),
  sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
  calibs = list(draw = TRUE, cex = 0.5, pch = 4, lwd = 1, col = "blue"),
  borders = list(borders = NA, lwd = 0.5, col = "black"),
  mask = list(mask = NA, lwd = 0, col = "black", fill = "black"),
  palette = list(step = NA, range = c(NA, NA), n_labels = 11, digits = 2, fn = NA),
  plot = TRUE,
  sphere = list(build = FALSE, keep_image = TRUE),
  ...
)

# S3 method for class 'ISOFIND'
plot(
  x,
  who = "group",
  cutoff = list(draw = TRUE, level = 0.05, col = "#909090"),
  sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
  calibs = list(draw = TRUE, cex = 0.5, pch = 4, lwd = 1, col = "blue"),
  assigns = list(draw = TRUE, cex = 0.5, pch = 5, lwd = 1, col = "white"),
  borders = list(borders = NA, lwd = 0.5, col = "black"),
  mask = list(mask = NA, lwd = 0, col = "black", fill = "black"),
  mask2 = list(mask = NA, lwd = 0, col = "purple", fill = "purple"),
  palette = list(step = NA, range = c(0, 1), n_labels = 11, digits = 2, fn = NA),
  plot = TRUE,
  sphere = list(build = FALSE, keep_image = TRUE),
  ...
)

# S3 method for class 'ISOFIT'
plot(x, cex_scale = 0.2, ...)

# S3 method for class 'CALIBFIT'
plot(
  x,
  pch = 1,
  col = "black",
  xlab = "Isotopic value in the environment",
  ylab = "Isotopic value in the calibration sample",
  xlim = NULL,
  ylim = NULL,
  line = list(show = TRUE, col = "blue"),
  CI = list(show = TRUE, col = "blue"),
  plot = TRUE,
  ...
)

# S3 method for class 'CALIBFIT'
points(
  x,
  pch = 2,
  col = "red",
  line = list(show = TRUE, col = "red"),
  CI = list(show = TRUE, col = "red"),
  plot = TRUE,
  ...
)

# S3 method for class 'SpatRaster'
plot(x, ...)

plot(x, y, ...)
```

## Arguments

- x:

  The return object of a call to
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md),
  [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md),
  [`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md),
  [`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md),
  or
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)

- which:

  A *string* indicating the name of the raster to be plotted (see
  details)

- y_title:

  A *list* containing information for the display of the title (see
  details)

- sources:

  A *list* containing information for the display of the location of the
  sources (see details)

- calibs:

  A *list* containing information for the display of the location of the
  calibration sampling location (see details)

- borders:

  A *list* containing information for the display of borders (e.g.
  country borders) (see details)

- mask:

  A *list* containing information for the display of a mask (e.g. an
  ocean mask) (see details)

- palette:

  A *list* containing information for the display of the colours for the
  isoscape (see details)

- plot:

  A *logical* indicating whether the plot shall be plotted or just
  returned

- sphere:

  A *list* containing information whether the raster should be returned
  as a rotating sphere and if the image created during the process
  should be saved in your current working directory. The default
  settings are FALSE and TRUE, respectively.

- ...:

  Additional arguments (only in use in plot.CALIBFIT and
  plot.SpatRaster)

- who:

  Either "group", or a vector of indices (e.g. 1:3) or names of the
  individuals (e.g. c("Mbe_1", "Mbe_3")) to be considered in assignment
  plots

- cutoff:

  A *list* containing information for the display of the region outside
  the prediction interval (see details)

- assigns:

  A *list* containing information for the display of the location of the
  assignment sampling location (see details)

- mask2:

  A *list* containing information for the display of a mask (e.g. a
  distribution mask) (see details)

- cex_scale:

  A *numeric* giving a scaling factor for the points in the plots

- pch:

  The argument pch as in [`par`](https://rdrr.io/r/graphics/par.html)
  for plot.CALIBFIT and points.CALIBFIT

- col:

  The argument col as in [`par`](https://rdrr.io/r/graphics/par.html)
  for plot.CALIBFIT and points.CALIBFIT

- xlab:

  A *string* the x-axis label in plot.CALIBFIT

- ylab:

  A *string* the y-axis label in plot.CALIBFIT

- xlim:

  A range defining the extreme coordinates for the the x-axis in
  plot.CALIBFIT

- ylim:

  A range defining the extreme coordinates for the the y-axis in
  plot.CALIBFIT

- line:

  A *list* containing two elements: `show`, a *logical* indicating
  whether to show the regression line or not; and `col`, a *string* or
  *integer* indicating the colour for plotting the regression line

- CI:

  A *list* containing two elements: `show`, a *logical* indicating
  whether to show the confidence interval or not; and `col`, a *string*
  or *integer* indicating the colour for plotting the confidence
  interval

- y:

  Not used by the plotting methods defined in IsoriX (see
  [`terra::plot`](https://rspatial.github.io/terra/reference/plot.html)
  for the generic function).

## Details

**General**

When called upon an object of class *ISOFIT*, the plot function draws
diagnostic information for the fits of the isoscape geostatistical
model.

When called upon an object of class *CALIBFIT*, the plot function draws
the fitted calibration function.

When called upon an object of class *ISOSCAPE*, the plot function draws
a fine-tuned plot of the isoscape.

When called upon an object of class *SpatRaster*, the plot function
displays the raster (just for checking things fast and dirty). In this
case, the function is a simple shortcut to
[`rasterVis::levelplot`](https://rdrr.io/pkg/lattice/man/levelplot.html).

**Plotting isoscapes**

When used on a fitted isoscape, the user can choose between plotting the
predictions (`which = "mean"`; default), the prediction variance
(`which = "mean_predVar"`), the residual variance
(`which = "mean_residVar"`), or the response variance
(`which = "mean_respVar"`) for the mean model; or the corresponding
information for the residual dispersion variance model (`"disp"`,
`"disp_predVar"`, `"disp_residVar"`, or `"disp_respVar"`).

When used on a calibrated isoscape (i.e. converted into sample values)
produced with the function
[`calibscape`](https://courtiol.github.io/IsoriX/reference/calibscape.md),
only the mean isotopic value can be plotted (`which = "mean"`). By
default, the location of the calibration samples is then drawn (if
available, i.e. not for the calibration method "desk") instead of the
location of the sources (use `sources = list(draw = TRUE)` to draw the
latter and `calibs = list(draw = FALSE)` to hide the former).

When used on a simulated isoscape produced with the function `isosim`
(currently dropped due to the package RandomFields being temporarily
retired from CRAN), the user can choose between plotting the mean
isotopic value (`which = "mean"`) or the residual dispersion
(`which = "disp"`).

**Plotting assignments**

When called upon an object of class *ISOFIND*, the plot function draws a
fine-tuned plot of the assignment. You can use the argument `who` to
choose between plotting the assignment for the group or for some
individuals (check the [online
tutorial](https://courtiol-isorix.share.connect.posit.cloud/) for
examples).

**Info on parameters influencing the rendering of maps**

The argument `y_title` is a list that can be tweaked to customise the
title of isoscapes. Within this list, the element `which` is a logical
indicating if the name of the layer should be displayed or not. The
element `title` is a string or a call used to define the rest of the
title, typically the delta notation of the isotope considered (e.g.
`bquote(delta^2*H)` for hydrogen). By default, no isotope is mentioned
since IsoriX cannot know which one you are working with. If you want to
define it for all plots, see
[`options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md)
(which also explains what to do if the delta symbol is not rendered).

The arguments `cutoff`, `sources`, `calibs`, `assigns`, `borders`,
`mask`, and `mask2` are used to fine-tune additional layers that can be
added to the main plot to embellish it. These arguments must be lists
that provide details on how to draw, respectively, the area outside the
prediction interval (for assignment plots), the locations of sources
(for both isoscape and assignment plots), the locations of the
calibration samples (for assignment plots), the locations of the
assignment samples (for assignment plots), the borders (for both types
of plots), and the mask (again, for both). For assignment maps, an extra
mask can be used (mask2), as one may want to add a mask covering the
area outside the biological range of the species. Within these lists,
the elements `lwd`, `col`, `cex`, `pch` and `fill` influences their
respective objects as in traditional R plotting functions (see
[`par`](https://rdrr.io/r/graphics/par.html) for details). The element
`draw` should be a *logical* that indicates whether the layer must be
created or not. The argument `borders` (within the list borders) expects
an object of the class *SpatVector*, such as the object
[`CountryBorders`](https://courtiol.github.io/IsoriX/reference/CountryBorders.md)
provided with this package. The argument `mask` (within the list mask)
also expects an object of the class *SpatVector*, such as the object
[`OceanMask`](https://courtiol.github.io/IsoriX/reference/OceanMask.md)
provided with this package (see examples).

The argument `palette` is used to define how to colour the isoscape and
assignment plot. Within this list, `step` defines the number of units on
the z-scale that shares a given colour; `range` can be used to constrain
the minimum and/or maximum values to be drawn (e.g. range = c(0, 1))
(this latter argument is useful if one wants to create several plots
with the same z-scale); `n_labels` allows for the user to approximately
define the maximum number of numbers plotted on the z-scale; `digits`
defines the number of digits displayed for the numbers used as labels;
and `fn` is used to specify the function that is used to sample the
colours. If `fn` is NULL (default) the palette functions derived from
[`isopalette1`](https://courtiol.github.io/IsoriX/reference/isopalette2.md)
and
[`isopalette2`](https://courtiol.github.io/IsoriX/reference/isopalette2.md)
are used when plotting isoscape and assignments, respectively. If `fn`
is NA the function used is the palette
[`viridisLite::viridis`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html).

**Default symbols used on maps**

Under the default settings, we chose to represent:

- the source data by little red triangles.

- the calibration data by little blue crosses.

- the locations where the samples to assign were collected by white
  diamonds. When individual assignments are plotted, each map only shows
  the location of the sample it corresponds to; the map of the group
  assignment shows the locations of all samples.

These symbols can be changed as explained above.

## See also

[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) for
the function fitting the isoscape

[`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
for the function building the isoscape

[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
for the function fitting the calibration function

[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md) for
the function performing the assignment

## Examples

``` r
## See ?isoscape or ?isofind for examples
```
