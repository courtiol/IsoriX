# Download files and check their binary integrity

This function is the internal function used in IsoriX to download the
large files from internet and it could be useful to download anything
from within R. We created this function to make sure that the downloaded
files are valid. Downloads can indeed result in files that are
corrupted, so we tweaked the options to reduce this possibility and the
function runs a check if the signature of the file is provided to the
argument `md5sum`.

## Usage

``` r
downloadfile(
  address = NULL,
  filename = NULL,
  path = NULL,
  overwrite = FALSE,
  md5sum = NULL,
  verbose = interactive()
)
```

## Arguments

- address:

  A *string* indicating the address of the file on internet

- filename:

  A *string* indicating the name under which the file must be stored

- path:

  A *string* indicating where to store the file on the hard drive
  (without the file name!). Default = current directory.

- overwrite:

  A *logical* indicating if an existing file should be re-downloaded

- md5sum:

  A *string* indicating the md5 signature of the valid file as created
  with [`tools::md5sum`](https://rdrr.io/r/tools/md5sum.html)

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session and
  `FALSE` otherwise. If a *numeric* is provided instead, additional
  information about the download will be provided if the number is
  greater than 1.

## Value

The complete path of the downloaded file (invisibly)

## Note

Users should directly use the function
[`getelev`](https://courtiol.github.io/IsoriX/reference/getelev.md) and
[`getprecip`](https://courtiol.github.io/IsoriX/reference/getprecip.md).

## See also

[`getelev`](https://courtiol.github.io/IsoriX/reference/getelev.md),
[`getprecip`](https://courtiol.github.io/IsoriX/reference/getprecip.md)
