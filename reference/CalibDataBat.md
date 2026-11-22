# Calibration data: hydrogen delta values of sedentary bats (Voigt & Lehnert 2019)

These datasets contain hydrogen delta values of fur keratin from 6
sedentary bat species. They correspond to the combination of several
studies as detailed in Voigt & Lehnert 2019. `CalibDataBat` is the
dataset used in Courtiol et al. 2019. The data can be used as an example
to fit a calibration model using the function
[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md).
`CalibDataBatRev` is the same data but the bat fur isotope values were
corrected to align with the current delta values for deuterium for
keratin reference materials (Soto et al. 2017,
https://doi.org/10.1002/rcm.7893) ensuring comparability between
formerly and more recently normalized datasets of delta values for
deuterium.

## Format

Two *dataframes* with 335 observations and 7 variables:

|              |             |                                        |
|--------------|-------------|----------------------------------------|
|              |             |                                        |
| site_ID      | (*factor*)  | Identification of the sampling site    |
| long         | (*numeric*) | Longitude coordinate (decimal degrees) |
| lat          | (*numeric*) | Latitude coordinate (decimal degrees)  |
| elev         | (*numeric*) | Elevation asl (m)                      |
| sample_ID    | (*factor*)  | Identification of the sampled animal   |
| species      | (*factor*)  | A code for the species                 |
| sample_value | (*numeric*) | Hydrogen delta value of the tissue     |

## Source

data provided by Voigt CC & Lehnert L.

## Details

Users who wish to use their own dataset for calibration should create a
*dataframe* of similar structure than these ones (only the column
'species' can be dropped). The columns should possess the same names as
the ones described above. If the elevation is unknown at the sampling
sites, elevation information can be extracted from a high resolution
elevation raster using the function
[`terra::extract`](https://rspatial.github.io/terra/reference/extract.html)
(see **Examples** in
[`CalibDataBat2`](https://courtiol.github.io/IsoriX/reference/CalibDataBat2.md)).

## References

Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial
mammals using stable isotopes. In Hobson KA & Wassenaar LI (eds.),
Tracking Animal Migration with Stable Isotopes, second edition. Academic
Press, London, pp. 117-136.
[doi:10.1016/B978-0-12-814723-8.00005-2](https://doi.org/10.1016/B978-0-12-814723-8.00005-2)
.

Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson
KA, Wassenaar LI & Kramer-Schadt S (2019). Isoscape computation and
inference of spatial origins with mixed models using the R package
IsoriX. In Hobson KA & Wassenaar LI (eds.), Tracking Animal Migration
with Stable Isotopes, second edition. Academic Press, London, pp.
207-236.
[doi:10.1016/B978-0-12-814723-8.00009-X](https://doi.org/10.1016/B978-0-12-814723-8.00009-X)
.

Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of
the hydrogen stable isotopic composition of keratin calibration
standards for wildlife and forensic science applications. Rapid Commun
Mass Spectrom. 31(14):1193-1203.
[doi:10.1002/rcm.7893](https://doi.org/10.1002/rcm.7893) . PMID:
28475227.

## See also

[`CalibDataBat2`](https://courtiol.github.io/IsoriX/reference/CalibDataBat2.md)
for another (related) calibration dataset

[`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md) to
fit a calibration model

## Examples

``` r

head(CalibDataBat)
#>   site_ID     long      lat elev sample_ID species sample_value
#> 1 site_29 18.87101 46.19747   90         1       4        -71.8
#> 2 site_29 18.87101 46.19747   90         2       4       -103.4
#> 3 site_29 18.87101 46.19747   90         3       4        -68.9
#> 4 site_29 18.87101 46.19747   90         4       4       -103.3
#> 5 site_32 19.10489 48.53405  522         5       4        -92.3
#> 6 site_32 19.10489 48.53405  522         6       4       -102.0
str(CalibDataBat)
#> 'data.frame':    335 obs. of  7 variables:
#>  $ site_ID     : Factor w/ 72 levels "site_1","site_10",..: 22 22 22 22 26 26 26 4 4 4 ...
#>  $ long        : num  18.9 18.9 18.9 18.9 19.1 ...
#>  $ lat         : num  46.2 46.2 46.2 46.2 48.5 ...
#>  $ elev        : int  90 90 90 90 522 522 522 20 20 20 ...
#>  $ sample_ID   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ species     : Factor w/ 6 levels "1","2","3","4",..: 4 4 4 4 4 4 4 4 4 4 ...
#>  $ sample_value: num  -71.8 -103.4 -68.9 -103.3 -92.3 ...
```
