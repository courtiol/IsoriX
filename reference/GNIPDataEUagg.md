# Source data: hydrogen delta values in precipitation water in Europe and in the world (aggregated per location)

These datasets contain the mean and variance of hydrogen delta value
from precipitation water sampled at weather stations between 1953 and
2015 in Europe (`GNIPDataEUagg`) and in the entire world
(`GNIPDataALLagg`). These data have been extracted from the
International Atomic Energy Agency IAEA in Vienna (GNIP Project: Global
Network of Isotopes in Precipitation) and processed by us using the
function
[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md).
The data are aggregated per location (across all month-year
combinations). We no longer provide the full non-aggregate GNIP dataset
in the package as it is not free to reuse. You can still download the
complete GNIP dataset for free, but you will have to proceed to a
registration process with GNIP and use their downloading interface WISER
(<https://nucleus.iaea.org/wiser>).

## Format

The *dataframe*s include many observations and the following variables:

|  |  |  |
|----|----|----|
|  |  |  |
| source_ID | (*factor*) | The unique identifier of the weather station |
| mean_source_value | (*numeric*) | Average of the aggregate of hydrogen delta values (per thousand) |
| var_source_value | (*numeric*) | Variance of the aggregate of hydrogen delta values (per thousand^2) |
| n_source_value | (*numeric*) | Number of hydrogen delta values aggregated |
| lat | (*numeric*) | Latitude coordinate (decimal degrees) |
| long | (*numeric*) | Longitude coordinate (decimal degrees) |
| elev | (*numeric*) | Elevation asl (m) |

## Source

data provided by the IAEA and processed by us.

## Details

These datasets have been aggregated and can thus be directly used for
fitting isoscapes.

If you want to use your own dataset, you must format your data as these
datasets.

## References

GNIP Project IAEA Global Network of Isotopes in Precipitation:
<https://www.iaea.org>

## See also

[`GNIPDataDE`](https://courtiol.github.io/IsoriX/reference/GNIPDataDE.md)
for a non-aggregated dataset.

## Examples

``` r

head(GNIPDataALLagg)
#>            source_ID mean_source_value var_source_value n_source_value    lat
#> 1      ADAK (ALASKA)        -63.903279         202.6467             61  51.88
#> 2              ADANA        -23.860089         273.2702            336  36.98
#> 3        ADDIS ABABA          9.752075         353.5850            347   9.00
#> 4 ADDIS ABABA (WEST)         -7.450000         764.4050              2   8.97
#> 5           ADELAIDE        -19.804839         321.7162            124 -34.93
#> 6         ADU BARIYE          7.334000         426.9245              5  12.54
#>      long elev
#> 1 -176.65    4
#> 2   35.30   73
#> 3   38.73 2360
#> 4   38.57 2300
#> 5  138.58   43
#> 6   39.76 1832
dim(GNIPDataALLagg)
#> [1] 921   7
head(GNIPDataEUagg)
#>   source_ID mean_source_value var_source_value n_source_value   lat   long elev
#> 1     ADANA         -23.86009        273.27022            336 36.98  35.30   73
#> 2   AGUALVA         -16.21667         48.86015             12 38.77 -27.19  260
#> 3  AL BAQAA         -26.89000        128.79878             10 32.09  35.77  700
#> 4   AL-JAFR           0.48000        721.87200              5 30.33  36.14  870
#> 5    ALAGOA         -14.96667         41.24697             12 38.79 -27.19   70
#> 6    ALEPPO         -32.13900        383.20099             10 36.18  37.21  410
dim(GNIPDataEUagg)
#> [1] 327   7
```
