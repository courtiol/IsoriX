# Download an elevation raster from internet

The function `getelev` downloads an elevation raster from internet. It
is a wrapper that 1) calls the function
[`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
to download the data and 2) saves the downloaded raster on the hard
drive (so that you don't have to keep downloading the same file over and
over again). The file saved on the disk is a \*.tif file which you can
directly read using the function
[`terra::rast`](https://rspatial.github.io/terra/reference/rast.html).

## Usage

``` r
getelev(
  file = "~/elevation_world_z5.tif",
  z = 5,
  long_min = -180,
  long_max = 180,
  lat_min = -90,
  lat_max = 90,
  margin_pct = 5,
  override_size_check = FALSE,
  overwrite = FALSE,
  Ncpu = getOption_IsoriX("Ncpu"),
  verbose = interactive(),
  ...
)
```

## Arguments

- file:

  A *string* indicating where to store the file on the hard drive
  (default = `~/elevation_world_z5.tif`)

- z:

  An *integer* between 1 and 14 indicating the resolution of the file do
  be downloaded (1 = lowest, 14 = highest; default = 5)

- long_min:

  A *numeric* indicating the minimum longitude to select from. Should be
  a number between -180 and 180 (default = -180).

- long_max:

  A *numeric* indicating the maximal longitude to select from. Should be
  a number between -180 and 180 (default = 180). To select an area
  crossing the antimeridian (i.e. the International Date Line, 180
  degrees), use a value of `long_max` smaller than `long_min` (e.g.
  `long_min = 70` and `long_max = -150` selects longitudes from 70 to
  180 and from -180 to -150) or, equivalently, a value of `long_max`
  above 180 (e.g. `long_max = 210`).

- lat_min:

  A *numeric* indicating the minimum latitude to select from. Should be
  a number between -90 and 90 (default = -90).

- lat_max:

  A *numeric* indicating the maximal latitude to select from (default =
  90).

- margin_pct:

  The percentage representing by how much the area should extend outside
  the area used for cropping (default = 5, corresponding to 5%). Set to
  0 if you want exact cropping.

- override_size_check:

  A *logical* indicating whether or not to override size checks (default
  = `FALSE`)

- overwrite:

  A *logical* indicating if an existing file should be re-downloaded

- Ncpu:

  An *integer* specifying the number of CPU's to use when downloading
  AWS tiles (default set by global package options).

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session and
  `FALSE` otherwise.

- ...:

  Other parameters to be passed to the function
  [`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)

## Value

This function returns the full path where the file has been stored

## Details

By default (and to keep with the spirit of the former implementations of
`getelev` in IsoriX, which did not rely on
[`elevatr::elevatr`](https://rdrr.io/pkg/elevatr/man/elevatr.html)), an
elevation raster of the whole world is downloaded with a resolution
correspond to ca. 0.6 km2 per raster cell. You can increase the
resolution by increasing the value of the argument `z`. You can also
restrict the area to be downloaded using the arguments `long_min`,
`long_max`, `lat_min` & `lat_max`.

Note that when using
[`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
you will be able to reduce the resolution and restrict the boundaries of
this elevation raster, but you won't be able to increase the resolution
or expend the boundaries. As a consequence, it is probably a good idea
to overshoot a little when using `getelev` and download data at a
resolution slightly higher than you need and for a extent larger than
your data.

Areas crossing the antimeridian (i.e. the International Date Line, 180
degrees) can be downloaded by using a value for `long_max` smaller than
the one for `long_min` (e.g. `long_min = 70, long_max = -150`). Since
[`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
only handles longitudes between -180 and 180, the area is then
downloaded in two parts (on each side of the antimeridian), which are
assembled into a single raster with continuous longitudes (e.g. from 70
to 210), as produced by
[`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md).

You can customise further what you download by using other parameters of
[`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
(via the elipsis `...`).

Please refer to the documentation of
[`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
for information on the sources and follows link in there to know how to
cite them.

## Examples

``` r

## To download the high resolution
## elevation raster in the current folder, just type:
## getelev()
```
