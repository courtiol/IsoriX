# Assignment data: simulated hydrogen delta values

This dataset contains simulated hydrogen delta values. The data can be
used as an example to perform assignments using the function
[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md).

## Format

A *dataframe* with 10 observations and 2 variables:

|              |             |                                    |
|--------------|-------------|------------------------------------|
|              |             |                                    |
| sample_ID    | (*factor*)  | Identification of the sample       |
| sample_value | (*numeric*) | Hydrogen delta value of the tissue |

## See also

[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md) to
perform assignments

## Examples

``` r

head(AssignDataAlien)
#>   sample_ID     long      lat sample_value
#> 1   Alien_1 13.52134 52.50598    -28.03596
#> 2   Alien_2 13.52134 52.50598    -25.61706
#> 3   Alien_3 13.52134 52.50598    -22.47987
#> 4   Alien_4 13.52134 52.50598    -28.55800
#> 5   Alien_5 13.52134 52.50598    -26.20985
#> 6   Alien_6  8.49914 52.03485    -23.58241
str(AssignDataAlien)
#> 'data.frame':    10 obs. of  4 variables:
#>  $ sample_ID   : Factor w/ 10 levels "Alien_1","Alien_10",..: 1 3 4 5 6 7 8 9 10 2
#>  $ long        : num  13.5 13.5 13.5 13.5 13.5 ...
#>  $ lat         : num  52.5 52.5 52.5 52.5 52.5 ...
#>  $ sample_value: num  -28 -25.6 -22.5 -28.6 -26.2 ...

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 30) {
  ## The following describes how we created such dataset

  ### We prepare the precipitation data
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ### We fit the models for Germany
  GermanFit <- isofit(data = GNIPDataDEagg)

  ### We build the isoscape
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  ### We create a simulated dataset with 1 site and 10 observations
  set.seed(1L)
  Aliens <- create_aliens(
    calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
    isoscape = GermanScape,
    raster = ElevRasterDE,
    coordinates = data.frame(
      site_ID = "Berlin",
      long = 13.52134,
      lat = 52.50598
    ),
    n_sites = 1,
    min_n_samples = 10,
    max_n_samples = 10
  )
  AssignDataAlien <- Aliens[, c("sample_ID", "sample_value")]

  ### Uncomment the following to store the file as we did
  # save(AssignDataAlien, file = "AssignDataAlien.rda", compress = "xz")
}
```
