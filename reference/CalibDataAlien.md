# Calibration data: simulated hydrogen delta values

This dataset contains simulated hydrogen delta values for corresponding
locations based on an assumed linear relationship between the animal
tissue value and the hydrogen delta values in the environment. The data
can be used as an example to fit a calibration model using the function
[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md).

## Format

A *dataframe* with x observations and 6 variables:

|              |             |                                        |
|--------------|-------------|----------------------------------------|
|              |             |                                        |
| site_ID      | (*factor*)  | Identification of the sampling site    |
| long         | (*numeric*) | Longitude coordinate (decimal degrees) |
| lat          | (*numeric*) | Latitude coordinate (decimal degrees)  |
| elev         | (*numeric*) | Elevation asl (m)                      |
| sample_ID    | (*factor*)  | Identification of the sampled animal   |
| tissue.value | (*numeric*) | Hydrogen delta value of the tissue     |

## Details

Users who wish to use their own dataset for calibration should create a
*dataframe* of similar structure than this one. The columns should
possess the same names as the ones described above. If the elevation is
unknown at the sampling sites, elevation information can be extracted
from a high resolution elevation raster using the function
[`terra::extract`](https://rspatial.github.io/terra/reference/extract.html).
In this dataset, we retrieved elevations from the Global
Multi-resolution Terrain Elevation Data 2010.

## See also

[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md) to
fit a calibration model

## Examples

``` r

head(CalibDataAlien)
#>   site_ID     long      lat elev sample_ID sample_value
#> 1    2263 14.04153 53.95819 6.26   Alien_1    -32.34100
#> 2    2263 14.04153 53.95819 6.26   Alien_2    -25.79168
#> 3    2263 14.04153 53.95819 6.26   Alien_3    -28.19276
#> 4    2263 14.04153 53.95819 6.26   Alien_4    -25.08740
#> 5    2263 14.04153 53.95819 6.26   Alien_5    -26.21117
#> 6    2263 14.04153 53.95819 6.26   Alien_6    -25.20650
str(CalibDataAlien)
#> 'data.frame':    500 obs. of  6 variables:
#>  $ site_ID     : int  2263 2263 2263 2263 2263 2263 2263 2263 2263 2263 ...
#>  $ long        : num  14 14 14 14 14 ...
#>  $ lat         : num  54 54 54 54 54 ...
#>  $ elev        : num  6.26 6.26 6.26 6.26 6.26 6.26 6.26 6.26 6.26 6.26 ...
#>  $ sample_ID   : Factor w/ 500 levels "Alien_1","Alien_10",..: 1 112 223 334 445 457 468 479 490 2 ...
#>  $ sample_value: num  -32.3 -25.8 -28.2 -25.1 -26.2 ...

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 30) {
  ## We prepare the precipitation data
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## We fit the models for Germany
  GermanFit <- isofit(data = GNIPDataDEagg)

  ## We build the isoscape
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  ## We create a simulated dataset with 50 site and 10 observations per site
  set.seed(2L)
  CalibDataAlien <- create_aliens(
    calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
    isoscape = GermanScape,
    raster = ElevRasterDE,
    n_sites = 50,
    min_n_samples = 10,
    max_n_samples = 10
  )
  plot(sample_value ~ source_value, data = CalibDataAlien)
  abline(3, 0.5)

  CalibDataAlien$source_value <- NULL

  ## Uncomment the following to store the file as we did
  # save(CalibDataAlien, file = "CalibDataAlien.rda", compress = "xz")
}
```
