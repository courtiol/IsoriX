# Predicts the spatial distribution of source isotopic values

This function produces the set of isoscapes, i.e. the spatial prediction
(i.e. maps) of the distribution of source isotopic values, as well as
several variances around such predictions. The predictions are computed
using the fitted geostatistical models for each raster cell of a
structural raster. All shape files can be exported and loaded into any
Geographic Information System (GIS) if needed (see online tutorials).

## Usage

``` r
isoscape(raster, isofit, verbose = interactive())
```

## Arguments

- raster:

  The structural raster (*SpatRaster*) such as an elevation raster
  created using
  [`prepelev`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)

- isofit:

  The fitted isoscape created by
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)

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

This function computes the predictions (`mean`), prediction variances
(`mean_predVar`), residual variances (`mean_residVar`) and response
variances (`mean_respVar`) for the isotopic values at a resolution equal
to the one of the structural raster. It also computes the same
information for the residual dispersion variance (`disp_pred`,
`disp_predVar`, `disp_residVar`, or `disp_respVar`).

The predictions of isotopic values across the landscape are performed by
calling the function
[`spaMM::predict`](https://rdrr.io/pkg/spaMM/man/predict.html) from the
package spaMM on the fitted isoscape produced by
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md).

Let us summarize the meaning of `mean`, `mean_predVar`, `mean_residVar`
and `mean_respVar` (see Courtiol & Rousset 2017 and Courtiol et al. 2019
for more details):

Our model assumes that that there is a single true unknown isoscape,
which is fixed but which is represented by the mixed-effect model as a
random draw from possible realizations of isoscapes (random draws of the
Matérn-correlated process and of the uncorrelated random effects if
considered). We infer this realized isoscape by fitting the model to a
limited amount of data, with some uncertainty since different random
draws of the unknown isoscape may give the same observed data. There is
thus a conditional distribution of possible true isoscapes given the
data. For linear mixed-effects models, the mean prediction is the mean
of this conditional distribution. The prediction variance is ideally the
mean square difference between the true unknown value of the linear
predictor and the mean prediction at a given location. The residual
variance is simply the prediction of the variance in isotopic value at a
given location. Its exact meaning depends on the aggregation scheme used
in
[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md),
but by default, it would correspond to the temporal variation between
months and across years. The response variance estimates the variance of
new observations drawn from the true unknown isoscape at a given
location. The response variance is simply equal to the sum of the
prediction variance and the residual variance (note that the residual
variance considered assume that a single observation is being observed
per location).

The isoscape can be plotted using the function
[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)
(see examples).

## References

Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
models. <https://www.biorxiv.org/content/10.1101/207662v1>

Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson
KA, Wassenaar LI & Kramer-Schadt S (2019). Isoscape computation and
inference of spatial origins with mixed models using the R package
IsoriX. In Hobson KA & Wassenaar LI (eds.), Tracking Animal Migration
with Stable Isotopes, second edition. Academic Press, London, pp.
207-236.
[doi:10.1016/B978-0-12-814723-8.00009-X](https://doi.org/10.1016/B978-0-12-814723-8.00009-X)
.

## See also

[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) for
the function fitting the isoscape

[`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)
for the function plotting the isoscape model

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 30) {
  ## We prepare the data
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## We fit the models
  GermanFit <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  ## We build the isoscapes
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  GermanScape
  plot(GermanScape)

  ## We build more plots
  PlotMean <- plot(x = GermanScape, which = "mean", plot = FALSE)

  PlotMeanPredVar <- plot(x = GermanScape, which = "mean_predVar", plot = FALSE)

  PlotMeanResidVar <- plot(x = GermanScape, which = "mean_residVar", plot = FALSE)

  PlotMeanRespVar <- plot(x = GermanScape, which = "mean_respVar", plot = FALSE)

  ## We display the plots
  print(PlotMean, split = c(1, 1, 2, 2), more = TRUE)
  print(PlotMeanPredVar, split = c(2, 1, 2, 2), more = TRUE)
  print(PlotMeanResidVar, split = c(1, 2, 2, 2), more = TRUE)
  print(PlotMeanRespVar, split = c(2, 2, 2, 2), more = FALSE)

  ## We build a sphere with our isoscape
  plot(x = GermanScape, which = "mean", plot = FALSE, sphere = list(build = TRUE))

  ## We can save a rotating sphere with the isoscape as a .gif-file.
  ## This file will be located inside your working directory.
  ## Make sure your current rgl device (from the previous step) is still open
  ## and that you have both the packages 'rgl' and 'magick' installed.
  ## The building of the .gif implies to create temporarily many .png
  ## but those will be removed automatically once the .gif is done.
  ## Uncomment to proceed (after making sure you have rgl, magick & webshot2 installed)
  # if(require("rgl") && require("magick") && require("webshot2")) {
  #   movie3d(spin3d(axis = c(0, 0, 1), rpm = 2), duration = 30, dir = getwd())
  # }
}
```
