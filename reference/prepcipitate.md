# Prepare the raster brick containing the precipitation data

This functions turns the WorldClim data downloaded using the function
[`getprecip`](https://courtiol.github.io/IsoriX/reference/getprecip.md)
into a *SpatRaster* of same resolution and extent as the structural
raster. This function is designed to be used with
[`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md).

## Usage

``` r
prepcipitate(path = NULL, raster, verbose = interactive())
```

## Arguments

- path:

  A *string* indicating the path where the WorldClim data have been
  downloaded. If the path is null (the default) the function will assume
  that the folder containing the precipitation data is in the current
  directory

- raster:

  A *raster* containing the structural raster

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session, and
  `FALSE` otherwise.

## See also

[`getprecip`](https://courtiol.github.io/IsoriX/reference/getprecip.md)
to download the relevant precipitation data

[`PrecipBrickDE`](https://courtiol.github.io/IsoriX/reference/PrecipBrickDE.md)
for the stored precipitation data for Germany

[`prepelev`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
to prepare an elevation raster

## Examples

``` r

## The following example takes some time and download a large amount of data (~ 1 Gb).
## It will therefore not be run unless you uncomment it

### We fit the models for Germany:
# GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#
# GermanFit <- isofit(data = GNIPDataDEagg,
#                     mean_model_fix = list(elev = TRUE, lat.abs = TRUE))
#
### We prepare the structural raster:
# StrRaster <- prepraster(raster = ElevRasterDE,
#                         isofit = GermanFit,
#                         aggregation_factor = 0)
#
### We download the precipitation data:
# temp_folder <- tempdir()
# getprecip(path = temp_folder)
#
### We prepare the raster brick with all the precipitation data:
# PrecipitationBrick <- prepcipitate(path = temp_folder,
#                                    raster = StrRaster)
#
### We plot the precipitation data:
# levelplot(PrecipitationBrick)
```
