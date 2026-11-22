# Save and read objects produced by IsoriX using RDS files

Because files created with IsoriX contain
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
and
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
objects, they cannot be saved using
[`base::saveRDS`](https://rdrr.io/r/base/readRDS.html) or
[`base::save`](https://rdrr.io/r/base/save.html) functions. The reason
is that objects created with
[`terra::terra`](https://rspatial.github.io/terra/reference/terra-package.html)
point to data stored in memory which are not contained in the R objects
themselves. Adapting the approach implemented in the
[`terra::terra`](https://rspatial.github.io/terra/reference/terra-package.html)
package, we provide a wrapper for
[`base::saveRDS`](https://rdrr.io/r/base/readRDS.html) and
[`base::readRDS`](https://rdrr.io/r/base/readRDS.html) functions, which
allows one to save and read objects produced with IsoriX by simply using
`saveRDS()` and `readRDS()`.

## Usage

``` r
saveRDS_IsoriX(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S3 method for class 'ISOSCAPE'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S3 method for class 'CALIBFIT'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S3 method for class 'ISOFIND'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S3 method for class 'character'
readRDS(file, refhook = NULL)

# S4 method for class 'ISOSCAPE'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S4 method for class 'CALIBFIT'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S4 method for class 'ISOFIND'
saveRDS(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)

# S4 method for class 'character'
readRDS(file, refhook = NULL)
```

## Arguments

- object:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) R object to
  serialize.

- file:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) a connection
  or the name of the file where the R object is saved to or read from.

- ascii:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) a logical. If
  `TRUE` or `NA`, an ASCII representation is written; otherwise
  (default), a binary one is used. See the comments in the help for
  [`base::save`](https://rdrr.io/r/base/save.html).

- version:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) the workspace
  format version to use. `NULL` specifies the current default version
  (3). The only other supported value is `2`, the default from R 1.4.0
  to R 3.5.0.

- compress:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) a logical
  specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2" or "xz" to indicate the type of
  compression to be used. Ignored if file is a connection.

- refhook:

  (definition copied from
  [`base::readRDS`](https://rdrr.io/r/base/readRDS.html):) a hook
  function for handling reference objects.

## Value

For `saveRDS`, `NULL` invisibly.

For `readRDS`, an R object.

## Details

[`base::saveRDS`](https://rdrr.io/r/base/readRDS.html) and
[`base::readRDS`](https://rdrr.io/r/base/readRDS.html) are standard S3
functions. So in order to be able to have a specific behaviour for
objects produced with IsoriX, we imported `saveRDS` and `readRDS` S4
generics from
[`terra::terra`](https://rspatial.github.io/terra/reference/terra-package.html)
to dispatch both S3 and S4 IsoriX-specific methods (see Methods_for_S3).
The S3 implementation is consistent with the rest of the package and
presents all usual benefits associated with S3 methods (e.g. simple
access to the code). The S4 implementation makes IsoriX methods
compatible with the use of
[`terra::saveRDS`](https://rspatial.github.io/terra/reference/serialize.html)
and
[`terra::readRDS`](https://rspatial.github.io/terra/reference/serialize.html).

## Functions

- `saveRDS_IsoriX()`: S3 function to save IsoriX objects into a RDS file

- `saveRDS(ISOSCAPE)`: S3 method to save an `ISOSCAPE` object into a RDS
  file

- `saveRDS(CALIBFIT)`: S3 method to save a `CALIBFIT` object into a RDS
  file

- `saveRDS(ISOFIND)`: S3 method to save an `ISOFIND` object into a RDS
  file

- `readRDS(character)`: S3 method to read an object produced with IsoriX
  (or other) stored in a RDS file

- `saveRDS(ISOSCAPE)`: S4 method to save an `ISOSCAPE` object into a RDS
  file

- `saveRDS(CALIBFIT)`: S4 method to save an `CALIBFIT` object into a RDS
  file

- `saveRDS(ISOFIND)`: S4 method to save an `ISOFIND` object into a RDS
  file

- `readRDS(character)`: S4 method to read an object produced with IsoriX
  (or other) stored in a RDS file

## Examples

``` r
if (getOption_IsoriX("example_maxtime") > 30) {
  ## We prepare the data
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## We fit the models
  GermanFit <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  ## We build the isoscapes
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

  ## Saving as RDS
  filename <- tempfile(fileext = ".rds") # or whatever names you want
  saveRDS(GermanScape, file = filename)

  ## Reading RDS
  GermanScape2 <- readRDS(filename)
  GermanScape2
}
```
