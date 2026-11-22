# Assignment data: hydrogen delta values of common noctule bats (Voigt & Lehnert 2019)

These datasets contain data from Voigt & Lehnert (2019). They contain
hydrogen delta values of fur keratin from common noctule bats (*Nyctalus
noctula*) killed at wind turbines in northern Germany. These data can be
used as an example to perform assignments using the function
[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md). The
difference between `AssignDataBat` and `AssignDataBatRev` is that in the
latter the bat fur isotope values were corrected to align with the
current delta values for deuterium for keratin reference materials (Soto
et al. 2017, https://doi.org/10.1002/rcm.7893) ensuring comparability
between formerly and more recently normalized datasets of delta values
for deuterium.

## Format

Two *dataframes* with 14 observations and 4 variables:

|              |             |                                        |
|--------------|-------------|----------------------------------------|
|              |             |                                        |
| sample_ID    | (*factor*)  | Identification of the animal           |
| lat          | (*numeric*) | Latitude coordinate (decimal degrees)  |
| long         | (*numeric*) | Longitude coordinate (decimal degrees) |
| sample_value | (*numeric*) | Hydrogen delta value of the tissue     |

## Source

data provided by Voigt CC & Lehnert L.

## References

Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial
mammals using stable isotopes. In Hobson KA & Wassenaar LI (eds.),
Tracking Animal Migration with Stable Isotopes, second edition. Academic
Press, London, pp. 117-136.
[doi:10.1016/B978-0-12-814723-8.00005-2](https://doi.org/10.1016/B978-0-12-814723-8.00005-2)
.

Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of
the hydrogen stable isotopic composition of keratin calibration
standards for wildlife and forensic science applications. Rapid Commun
Mass Spectrom. 31(14):1193-1203.
[doi:10.1002/rcm.7893](https://doi.org/10.1002/rcm.7893) . PMID:
28475227.

## See also

[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md) to
perform assignments

## Examples

``` r

head(AssignDataBat)
#>   sample_ID      lat     long sample_value
#> 1    Nnoc_1 51.72323 13.67630      -86.585
#> 2    Nnoc_2 51.72323 13.67630      -93.967
#> 3    Nnoc_3 51.72323 13.67630      -88.716
#> 4    Nnoc_4 51.69520 14.64945      -84.857
#> 5    Nnoc_5 51.70366 14.63915     -104.219
#> 6    Nnoc_7 51.69520 14.64945      -85.583
str(AssignDataBat)
#> 'data.frame':    14 obs. of  4 variables:
#>  $ sample_ID   : Factor w/ 14 levels "Nnoc_1","Nnoc_10",..: 1 8 9 10 11 12 13 14 2 3 ...
#>  $ lat         : num  51.7 51.7 51.7 51.7 51.7 ...
#>  $ long        : num  13.7 13.7 13.7 14.6 14.6 ...
#>  $ sample_value: num  -86.6 -94 -88.7 -84.9 -104.2 ...
```
