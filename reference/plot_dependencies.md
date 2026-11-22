# Plot the dependencies of IsoriX

This function plots the packages that IsoriX directly requires (i.e. the
packages it imports), as well as the dependencies among these packages.
This can help to understand where an installation problem comes from,
since if one of these packages cannot be installed, IsoriX cannot be
installed either. By default, the packages suggested by IsoriX, which
are only needed for some extra functionalities, are also displayed.

## Usage

``` r
plot_dependencies(suggests = TRUE, seed = 1L)
```

## Arguments

- suggests:

  A *logical* indicating whether the packages suggested by IsoriX should
  also be shown (default = `TRUE`).

- seed:

  An *integer* used to set the seed of the random number generator that
  controls the layout of the plot (the state of the random number
  generator is restored afterwards, so that it is not altered by the
  call). If some packages or arrows overlap, try another value to get
  another layout.

## Value

The graph of the dependencies (an object of class *igraph*), returned
invisibly.

## Details

The dependencies are those of the installed version of IsoriX and of the
installed versions of the other packages, so no internet connection is
required. Base packages (e.g. stats) are not shown.

In the plot, an arrow going from a package to another means that the
first package depends on the second one. When suggested packages are
shown, dashed arrows go from IsoriX to its suggested packages. If some
suggested packages are not installed, they are shown in white: since
they are not installed, their own dependencies are not known and thus
not displayed.

This function requires the package igraph to be installed.

## See also

[`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html),
which this function uses to find the dependencies.

## Examples

``` r

if (requireNamespace("igraph", quietly = TRUE)) {
  ## packages required or suggested by IsoriX:
  plot_dependencies()

  ## packages required by IsoriX only:
  plot_dependencies(suggests = FALSE)
}


```
