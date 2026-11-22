# Download rasters of monthly precipitation from internet

The function `getprecip` allows for the download of rasters of monthly
precipitation from internet. It downloads the "precipitation (mm)
WorldClim Version 2.1" at a spatial resolution of 30 seconds (~1 km2).
After download, the function also unzip the file. The function
`getprecip` uses the generic function `downloadfile` that can also be
used to download directly other files. This raster needs further
processing with the function
[`prepcipitate`](https://courtiol.github.io/IsoriX/reference/prepcipitate.md).
It can then be used to predict annual averages precipitation weighted
isoscapes with the function
[`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md).

## Usage

``` r
getprecip(path = NULL, overwrite = FALSE, verbose = interactive())
```

## Source

<https://worldclim.org/data/worldclim21.html>

## Arguments

- path:

  A *string* indicating where to store the file on the hard drive
  (without the file name!). Default = current directory.

- overwrite:

  A *logical* indicating if an existing file should be re-downloaded

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session and
  `FALSE` otherwise. If a *numeric* is provided instead, additional
  information about the download will be provided if the number is
  greater than 1.

## Value

This function returns the path of the folder where the files have been
stored

## Details

In the argument "path" is not provided, the file will be stored in the
current working directory. The functions can create new directories, so
you can also indicate a new path. The integrity of the elevation raster
is tested after a call to `getprecip`. In case of corruption, try
downloading the file again, specifying overwrite = TRUE to overwrite the
corrupted file.

## Examples

``` r

## To download the monthly precipitation
## in a temporary directory
## directory, just type:
## temp_folder <- tempdir()
## getprecip(path = temp_folder)
## Mind that the file weights ca. 1GB!
## For real use, replace temp_folder by your selected computer path
```
