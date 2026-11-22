# Setting and displaying the options of the package

\*\* Information on the settings for the delta notation \*\*

## Usage

``` r
options_IsoriX(...)

getOption_IsoriX(x = NULL)
```

## Arguments

- ...:

  A named value or a list of named values. The following values, with
  their defaults, are used:

  title_delta_notation

  :   `NULL` (default) or a name, call, or expression used in the titles
      of isoscape plots to refer to the delta notation (see above).

  example_maxtime

  :   The number of seconds allowed for a given example to run. It is
      used to control whether the longer examples should be run or not
      based on the comparison between this option and the approximate
      running time of the example on our computers.

  Ncpu

  :   An *integer* (default `2L`) corresponding to the number of CPUs to
      be used by the functions that can handle parallel processing:
      [`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
      [`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md),
      [`getelev`](https://courtiol.github.io/IsoriX/reference/getelev.md)
      and
      [`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md).
      Use `1L` to prevent parallel processing.

  dont_ask

  :   A *logical* indicating if the user prompt during interactive
      session during plotting must be inactivated (for development
      purposes only).

  spaMM_blocksize

  :   A positive *integer* (default `16000L`) giving the number of
      locations per slice used by spaMM to compute the prediction
      variances in
      [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
      (argument `blockSize` of
      [`spaMM::predict`](https://rdrr.io/pkg/spaMM/man/predict.html)).
      Larger values are faster but use more memory; lower it if
      [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
      runs out of memory on large rasters.

  spaMM_debugmod

  :   A *logical* indicating if the warnings and errors produced by the
      spaMM package should stopped being turned into messages (for
      development purposes only).

- x:

  A character string holding an option name.

## Value

The options are invisibly returned in an object called
`IsoriX:::.data_IsoriX$options`

## Details

By default, the titles of isoscape plots do not mention any isotope
since IsoriX cannot know which isotope you are working with (the default
value of `title_delta_notation` is `NULL`). You can define the delta
notation to be used in the titles of all plots by setting this option,
e.g. `options_IsoriX(title_delta_notation = bquote(delta^2*H))` for
hydrogen, or `options_IsoriX(title_delta_notation = bquote(delta^18*O))`
for oxygen.

If the delta symbol is not rendered on your plots (i.e. the title only
shows the superscript and the element), the problem comes from the font
used by the graphic device to draw Greek letters, not from IsoriX. This
happens for example with `CairoPNG()` from the package Cairo on some
systems. You can then:

- tell Cairo which font to use for symbols, using a font installed on
  your system that contains Greek letters, e.g.
  `Cairo::CairoFonts(symbol = "DejaVu Sans", usePUA = FALSE)`;

- use another graphic device, e.g. `png(type = "cairo")`;

- or use the Unicode character for delta, e.g.
  `options_IsoriX(title_delta_notation = bquote(italic("\u03B4")^2*H))`.
  Note that this last solution does not work with the device
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html).

## Examples

``` r
OldOptions <- options_IsoriX()
OldOptions
#> $title_delta_notation
#> NULL
#> 
#> $example_maxtime
#> [1] 5
#> 
#> $Ncpu
#> [1] 2
#> 
#> $dont_ask
#> [1] FALSE
#> 
#> $spaMM_blocksize
#> [1] 16000
#> 
#> $spaMM_debug
#> [1] FALSE
#> 

## defining the delta notation used in the titles of isoscape plots,
## e.g. for hydrogen:
options_IsoriX(title_delta_notation = bquote(delta^2*H))
getOption_IsoriX("title_delta_notation")
#> delta^2 * H

## or for oxygen:
options_IsoriX(title_delta_notation = bquote(delta^18*O))
getOption_IsoriX("title_delta_notation")
#> delta^18 * O

getOption_IsoriX("example_maxtime")
#> [1] 5
options_IsoriX(example_maxtime = 30)
options_IsoriX()
#> $title_delta_notation
#> delta^18 * O
#> 
#> $example_maxtime
#> [1] 30
#> 
#> $Ncpu
#> [1] 2
#> 
#> $dont_ask
#> [1] FALSE
#> 
#> $spaMM_blocksize
#> [1] 16000
#> 
#> $spaMM_debug
#> [1] FALSE
#> 

## restoring the original options:
options_IsoriX(OldOptions)
options_IsoriX()
#> $title_delta_notation
#> NULL
#> 
#> $example_maxtime
#> [1] 5
#> 
#> $Ncpu
#> [1] 2
#> 
#> $dont_ask
#> [1] FALSE
#> 
#> $spaMM_blocksize
#> [1] 16000
#> 
#> $spaMM_debug
#> [1] FALSE
#> 
```
