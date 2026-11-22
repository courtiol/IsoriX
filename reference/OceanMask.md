# Polygon data: mask of world oceans

This dataset contains a polygon SpatVector (from terra). It can be used
to mask large bodies of water. It is the complement of the polygons
stored in
[`CountryBorders`](https://courtiol.github.io/IsoriX/reference/CountryBorders.md)
within their extent.

## Format

A *SpatVector* object

## Source

See example for details on how we created the dataset.

## See also

- [`CountryBorders`](https://courtiol.github.io/IsoriX/reference/CountryBorders.md)
  for another polygon used to embellish the plots

## Examples

``` r

plot(OceanMask, col = "blue")


## How did we create this file?

## Uncomment the following to create the file as we did
## (after having created CountryBorders, see ?CountryBorders)
# if (require(terra)) {
#   worldlimit <- as.polygons(ext(CountryBorders))
#   crs(worldlimit) <- crs(CountryBorders)
#   OceanMask <- worldlimit - CountryBorders
#   #terra::saveRDS(OceanMask, file = "inst/extdata/OceanMask.rds", compress = "xz")
# }
```
