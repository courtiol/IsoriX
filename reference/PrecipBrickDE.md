# Raster data: monthly precipitation amounts in Germany

This brick of rasters contains the monthly precipitation amounts (in mm)
for Germany with a resolution of approximately 30 square-km.

## Format

A *SpatRaster* with 12 layers

## Source

<https://worldclim.org/data/worldclim21.html>

## Details

The data are derived from "precipitation (mm) WorldClim Version2" which
can be downloaded using the function
[`getprecip`](https://courtiol.github.io/IsoriX/reference/getprecip.md).

## See also

[`prepcipitate`](https://courtiol.github.io/IsoriX/reference/prepcipitate.md)
to prepare this raster

## Examples

``` r

## The following example requires to download
## a large precipitation rasters with the function getprecip()
## and will therefore not run unless you uncomment it

## How did we create this file?

## Uncomment the following to create the file as we did
# getprecip() ## Download the tif files (~ 1 Gb compressed)
# PrecipBrickDE <- prepcipitate(raster = ElevRasterDE)
# terra::saveRDS(PrecipBrickDE, file = "PrecipBrickDE.rds", compress = "xz")
```
