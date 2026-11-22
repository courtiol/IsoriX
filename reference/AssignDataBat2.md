# Assignment data: hydrogen delta values of Myotis bats (Voigt, Lehmann & Greif 2015)

These datasets contain data from Voigt, Lehmann & Greif (2015). It
contains hydrogen delta values of fur keratin from bats captured in
2008, 2009 and 2013 from their roosting sites in Bulgaria. We only
retained the bats of the genus Myotis from the original study. These
data can be used as an example to perform assignments using the function
[`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md). The
difference between `AssignDataBat2` and `AssignDataBat2Rev` is that in
the latter the bat fur isotope values were corrected to align with the
current delta values for deuterium for keratin reference materials (Soto
et al. 2017, https://doi.org/10.1002/rcm.7893) ensuring comparability
between formerly and more recently normalized datasets of delta values
for deuterium.

## Format

Two *dataframes* with 244 observations and 3 variables:

|              |             |                                    |
|--------------|-------------|------------------------------------|
|              |             |                                    |
| sample_ID    | (*factor*)  | Identification of the animal       |
| species      | (*factor*)  | Animal species name                |
| sample_value | (*numeric*) | Hydrogen delta value of the tissue |

## Source

data provided by Voigt CC, Lehmann D & Greif S.

## References

Voigt CC, Lehmann D & Greif S (2015). Stable isotope ratios of hydrogen
separate mammals of aquatic and terrestrial food webs. Methods in
Ecology and Evolution 6(11):1332-1340.
[doi:10.1111/2041-210X.12414](https://doi.org/10.1111/2041-210X.12414) .

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

head(AssignDataBat2)
#>   sample_ID            species sample_value
#> 1     Mbe_1 Myotis_bechsteinii        -68.8
#> 2     Mbe_2 Myotis_bechsteinii        -64.3
#> 3     Mbe_3 Myotis_bechsteinii        -72.9
#> 4     Mbe_4 Myotis_bechsteinii        -76.1
#> 5     Mbe_5 Myotis_bechsteinii        -69.9
#> 6     Mbe_6 Myotis_bechsteinii        -60.9
str(AssignDataBat2)
#> 'data.frame':    244 obs. of  3 variables:
#>  $ sample_ID   : Factor w/ 244 levels "Mbe_1","Mbe_10",..: 1 3 4 5 6 7 8 9 10 2 ...
#>  $ species     : chr  "Myotis_bechsteinii" "Myotis_bechsteinii" "Myotis_bechsteinii" "Myotis_bechsteinii" ...
#>  $ sample_value: num  -68.8 -64.3 -72.9 -76.1 -69.9 -60.9 -63.4 -68 -72.9 -60.4 ...
```
