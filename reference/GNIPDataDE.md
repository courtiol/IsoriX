# Source data: hydrogen delta values in precipitation water in Germany

This dataset contains the hydrogen delta value from precipitation water
sampled at weather stations between 1961 and 2013 in Germany. These data
have been kindly provided by Christine Stumpp and processed by the
International Atomic Energy Agency IAEA in Vienna (GNIP Project: Global
Network of Isotopes in Precipitation). These data are free to reuse
provided the relevant citations (see references). These data represent a
small sample of the much larger dataset compiled by the GNIP. We no
longer provide larger GNIP dataset in the package as those are not free
to reuse (but we do provide aggregated versions of it; see
[`GNIPDataEUagg`](https://courtiol.github.io/IsoriX/reference/GNIPDataEUagg.md)).
You can still download the complete GNIP dataset for free, but you will
have to proceed to a registration process with GNIP and use their
downloading interface WISER (<https://nucleus.iaea.org/wiser>).

## Format

The *dataframe* includes 8591 observations and the following variables:

|              |             |                                              |
|--------------|-------------|----------------------------------------------|
|              |             |                                              |
| lat          | (*numeric*) | Latitude coordinate (decimal degrees)        |
| long         | (*numeric*) | Longitude coordinate (decimal degrees)       |
| elev         | (*numeric*) | Elevation asl (m)                            |
| source_value | (*numeric*) | hydrogen delta value (per thousand)          |
| year         | (*numeric*) | Year of sampling                             |
| month        | (*numeric*) | Month of sampling                            |
| source_ID    | (*factor*)  | The unique identifier of the weather station |

## Source

data provided by the IAEA.

## Details

The dataset contains non-aggregated data for 27 weather stations across
Germany.

This dataset is the raw data source and should not be directly used for
fitting isoscapes.

Please use
[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
to filter the dataset by time and location.

If you want to use your own dataset, you must format your data as those
produced by the function
[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md).

## References

GNIP Project IAEA Global Network of Isotopes in Precipitation:
<https://www.iaea.org>

Stumpp, C., Klaus, J., & Stichler, W. (2014). Analysis of long-term
stable isotopic composition in German precipitation. Journal of
Hydrology, 517, 351-361.
[doi:10.1016/j.jhydrol.2014.05.034](https://doi.org/10.1016/j.jhydrol.2014.05.034)
.

Klaus, J., Chun, K. P., & Stumpp, C. (2015). Temporal trends in d18O
composition of precipitation in Germany: insights from time series
modelling and trend analysis. Hydrological Processes, 29(12), 2668-2680.
[doi:10.1002/hyp.10395](https://doi.org/10.1002/hyp.10395) .

## See also

[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
to prepare the dataset for the analyses and to filter by time and
location.

## Examples

``` r

head(GNIPDataDE)
#>   source_ID   lat long elev year month source_value
#> 1 SCHLESWIG 54.52 9.54   43 1997     6        -59.0
#> 2 SCHLESWIG 54.52 9.54   43 1997     7        -56.0
#> 3 SCHLESWIG 54.52 9.54   43 1997     8        -60.8
#> 4 SCHLESWIG 54.52 9.54   43 1997     9        -51.0
#> 5 SCHLESWIG 54.52 9.54   43 1997    10        -58.7
#> 6 SCHLESWIG 54.52 9.54   43 1997    11        -74.6
```
