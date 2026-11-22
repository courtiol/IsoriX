# Calibration data: hydrogen delta values of sedentary bats (Popa-Lisseanu et al. 2012)

These datasets contain hydrogen delta values of fur keratin from
sedentary bat species captured between 2005 and 2009 from Popa-Lisseanu
et al. (2012). These data can be used as an example to fit a calibration
model using the function
[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md).
The difference between `CalibDataBat2` and `CalibDataBat2Rev` is that in
the latter the bat fur isotope values were corrected to align with the
current delta values for deuterium for keratin reference materials (Soto
et al. 2017, https://doi.org/10.1002/rcm.7893) ensuring comparability
between formerly and more recently normalized datasets of delta values
for deuterium.

## Format

Two *dataframes* with 178 observations and 6 variables:

|              |             |                                        |
|--------------|-------------|----------------------------------------|
|              |             |                                        |
| site_ID      | (*factor*)  | Identification of the sampling site    |
| long         | (*numeric*) | Longitude coordinate (decimal degrees) |
| lat          | (*numeric*) | Latitude coordinate (decimal degrees)  |
| elev         | (*numeric*) | Elevation asl (m)                      |
| sample_ID    | (*factor*)  | Identification of the sampled animal   |
| sample_value | (*numeric*) | Hydrogen delta value of the tissue     |

## Source

data provided by Popa-Lisseanu AG et al.

## Details

Users who wish to use their own dataset for calibration should create a
*dataframe* of similar structure than these ones (only the column
'species' can be dropped). The columns should possess the same names as
the ones described above. If the elevation is unknown at the sampling
sites, elevation information can be extracted from a high resolution
elevation raster using the function
[`terra::extract`](https://rspatial.github.io/terra/reference/extract.html)
(see **Examples**). Note that the original study used a different source
of elevation data.

## References

Popa-Lisseanu AG, Soergel K, Luckner A, Wassenaar LI, Ibanez C,
Kramer-Schadt S, Ciechanowski M, Goerfoel T, Niermann I, Beuneux G,
Myslajek RW, Juste J, Fonderflick J, Kelm D & Voigt CC (2012). A triple
isotope approach to predict the breeding origins of European bats. PLoS
ONE 7(1):e30388.
[doi:10.1371/journal.pone.0030388](https://doi.org/10.1371/journal.pone.0030388)
.

Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of
the hydrogen stable isotopic composition of keratin calibration
standards for wildlife and forensic science applications. Rapid Commun
Mass Spectrom. 31(14):1193-1203.
[doi:10.1002/rcm.7893](https://doi.org/10.1002/rcm.7893) . PMID:
28475227.

## See also

[`CalibDataBat`](https://courtiol.github.io/IsoriX/reference/CalibDataBat.md)
for another (related) calibration dataset

[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md) to
fit a calibration model

## Examples

``` r

head(CalibDataBat2)
#>   site_ID      long      lat elev sample_ID sample_value
#> 1  site_1 -5.590503 36.52158  800  Ese_2337        -36.4
#> 2  site_1 -5.590503 36.52158  800  Ese_2338        -43.9
#> 3  site_2 -5.977033 37.51861    8  Eis_2614        -43.8
#> 4  site_2 -5.977033 37.51861    8  Eis_2615        -59.8
#> 5  site_2 -5.977033 37.51861    8  Eis_2616        -49.0
#> 6  site_2 -5.977033 37.51861    8  Eis_2617        -51.1
str(CalibDataBat2)
#> 'data.frame':    178 obs. of  6 variables:
#>  $ site_ID     : Factor w/ 45 levels "site_1","site_2",..: 1 1 2 2 2 2 2 2 2 2 ...
#>  $ long        : num  -5.59 -5.59 -5.98 -5.98 -5.98 ...
#>  $ lat         : num  36.5 36.5 37.5 37.5 37.5 ...
#>  $ elev        : int  800 800 8 8 8 8 8 8 8 8 ...
#>  $ sample_ID   : Factor w/ 178 levels "Bba_1017","Bba_1018",..: 42 43 16 17 18 19 20 21 22 23 ...
#>  $ sample_value: num  -36.4 -43.9 -43.8 -59.8 -49 -51.1 -51.4 -50.6 -51.4 -45.5 ...

## The following example require to have downloaded
## an elevation raster with the function getelev()
## and will therefore not run unless you uncomment it

# if (require(terra)){
#    ## We delete the elevation data
#    CalibDataBat2$elev <- NULL
#
#    ## We reconstruct the elevation data using an elevation raster
#    getelev(file = "elevBats.tif", z = 6,
#            lat_min = min(CalibDataBat2$lat),
#            lat_max = max(CalibDataBat2$lat),
#            long_min = min(CalibDataBat2$long),
#            long_max = max(CalibDataBat2$long))
#    ElevationRasterBig <- rast("elevBats.tif")
#    CalibDataBat2$elev <- extract(
#        ElevationRasterBig,
#        cbind(CalibDataBat2$long, CalibDataBat2$lat))
#    head(CalibDataBat2)
# }
```
