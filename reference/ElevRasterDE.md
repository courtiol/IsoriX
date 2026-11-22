# Raster data: elevation in Germany

This raster contains the elevation of the surface of Germany (meters
above sea level) with a resolution of approximately 40 square-km.

## Format

A *SpatRaster* object

## Source

<https://topotools.cr.usgs.gov/gmted_viewer/viewer.htm>

## Details

This raster contains elevation data of Germany in a highly aggregated
form corresponding to a resolution of approximately one elevation value
per 40 square-km. This is only for the purpose of having a small and
easy-to-handle file to practice, but it should not be used to perform
real assignments!

## See also

[`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
to crop and/or aggregate this raster

## Examples

``` r

## Compute crudely the resolution (approximative size of cells in km2)
median(values(cellSize(ElevRasterDE, unit = "km")))
#> [1] 39.65434

## How did we create this file (without IsoriX) ?

## Uncomment the following to create the file as we did

# ElevRasterDE <- elevatr::get_elev_raster(locations = data.frame(
#                              x = c(5.5, 15.5), y = c(47, 55.5)),
#                              prj = "+proj=longlat +datum=WGS84 +no_defs",
#                              clip = "bbox", z = 3)
#
# ElevRasterDE <- terra::rast(ElevRasterDE)


## How to create a similar file with IsoriX ?
#
# ## Download the tif file (see ?getelev)
# getelev(file = "~/ElevRasterDE.tif",
#         z = 3,
#         long_min = 5.5, long_max = 15.5, lat_min = 47, lat_max = 55.5)

# ## Convert the tif into R raster format
# ElevRasterDE <- rast('~/ElevRasterDE.tif')
```
