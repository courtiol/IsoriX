#' Setting and displaying the options of the package
#'
#' ** Information on the settings for the delta notation **
#'
#' By default, the titles of isoscape plots do not mention any isotope since
#' IsoriX cannot know which isotope you are working with (the default value of
#' `title_delta_notation` is `NULL`). You can define the delta notation to be
#' used in the titles of all plots by setting this option, e.g.
#' `options_IsoriX(title_delta_notation = bquote(delta^2*H))` for hydrogen, or
#' `options_IsoriX(title_delta_notation = bquote(delta^18*O))` for oxygen.
#'
#' If the delta symbol is not rendered on your plots (i.e. the title only
#' shows the superscript and the element), the problem comes from the font used by
#' the graphic device to draw Greek letters, not from IsoriX. This happens for
#' example with `CairoPNG()` from the package \pkg{Cairo} on some systems. You
#' can then:
#'  - tell \pkg{Cairo} which font to use for symbols, using a font installed on
#'  your system that contains Greek letters, e.g.
#'  `Cairo::CairoFonts(symbol = "DejaVu Sans", usePUA = FALSE)`;
#'  - use another graphic device, e.g. `png(type = "cairo")`;
#'  - or use the Unicode character for delta, e.g.
#'  `options_IsoriX(title_delta_notation = bquote(italic("\u03B4")^2*H))`.
#'  Note that this last solution does not work with the device `pdf()`.
#'
#' @name options
#' @aliases options options_IsoriX getOption_IsoriX
#' @param ... A named value or a list of named values. The following values, with their defaults, are used:
#' \describe{
#'   \item{title_delta_notation}{`NULL` (default) or a name, call, or expression used in the titles of isoscape plots to refer to the delta notation (see above).}
#'   \item{example_maxtime}{The number of seconds allowed for a given example to run. It is used to control whether the longer examples should be run or not based on the comparison between this option and the approximate running time of the example on our computers.}
#'   \item{Ncpu}{An *integer* corresponding to the number of cores to be used (in functions that can handle parallel processing).}
#'   \item{dont_ask}{A *logical* indicating if the user prompt during interactive session during plotting must be inactivated (for development purposes only).}
#'   \item{spaMM_blocksize}{A positive *integer* (default `16000L`) giving the number of locations per slice used by \pkg{spaMM} to compute the prediction variances in [`isoscape`] (argument `blockSize` of [`spaMM::predict`]). Larger values are faster but use more memory; lower it if [`isoscape`] runs out of memory on large rasters.}
#'   \item{spaMM_debugmod}{A *logical* indicating if the warnings and errors produced by the spaMM package should stopped being turned into messages (for development purposes only).}
#' }
#'
#' @return The options are invisibly returned in an object called `IsoriX:::.data_IsoriX$options`
#' @export
#'
#' @examples
#' OldOptions <- options_IsoriX()
#' OldOptions
#'
#' ## defining the delta notation used in the titles of isoscape plots,
#' ## e.g. for hydrogen:
#' options_IsoriX(title_delta_notation = bquote(delta^2*H))
#' getOption_IsoriX("title_delta_notation")
#'
#' ## or for oxygen:
#' options_IsoriX(title_delta_notation = bquote(delta^18*O))
#' getOption_IsoriX("title_delta_notation")
#'
#' getOption_IsoriX("example_maxtime")
#' options_IsoriX(example_maxtime = 30)
#' options_IsoriX()
#'
#' ## restoring the original options:
#' options_IsoriX(OldOptions)
#' options_IsoriX()
options_IsoriX <- function(...) { ## as in spaMM
  if (nargs() == 0) {
    return(.data_IsoriX$IsoriX_options)
  }
  current <- .data_IsoriX$IsoriX_options
  temp <- list(...)
  if (length(temp) == 1 && is.null(names(temp))) {
    arg <- temp[[1]]
    switch(mode(arg),
      list = temp <- arg,
      character = return(.data_IsoriX$IsoriX_options[arg]),
      stop("invalid argument: ", sQuote(arg))
    )
  }
  if (length(temp) == 0) {
    return(current)
  }
  n <- names(temp)
  if (is.null(n)) stop("options must be given by name")
  current[n] <- temp
  .data_IsoriX$IsoriX_options <- current
  invisible(current)
}


#' @rdname options
#' @param x A character string holding an option name.
#' @export

getOption_IsoriX <- function(x = NULL) {
  if (length(x) == 0) {
    return(options_IsoriX())
  }
  if (length(x) > 1L) {
    stop("argument 'x' must be of length 1 maximum")
  }
  if (x == "Ncpu" && (options_IsoriX(x)[["Ncpu"]] > parallel::detectCores())) {
    warning(paste(
      options_IsoriX(x)[["Ncpu"]],
      "CPUs were requested, but the maximum number of CPU on this machine is",
      parallel::detectCores(),
      "so the Ncpu option of this package has been corrected"
    ))
    options_IsoriX(Ncpu = parallel::detectCores())
  }
  if (x == "Ncpu") {
    return(options_IsoriX(x)[["Ncpu"]])
  }
  if (x == "title_delta_notation") {
    return(options_IsoriX(x)[["title_delta_notation"]])
  }
  if (x == "example_maxtime") {
    return(options_IsoriX(x)[["example_maxtime"]])
  }
  if (x == "dont_ask") {
    return(options_IsoriX(x)[["dont_ask"]])
  }
  if (x == "spaMM_blocksize") {
    blocksize <- options_IsoriX(x)[["spaMM_blocksize"]]
    if (!is.numeric(blocksize) || length(blocksize) != 1L || is.na(blocksize) ||
      blocksize < 1 || blocksize != round(blocksize)) {
      stop("the option 'spaMM_blocksize' must be a single positive integer (e.g. `options_IsoriX(spaMM_blocksize = 16000L)`)")
    }
    return(as.integer(blocksize))
  }
  if (x == "spaMM_debug") {
    return(options_IsoriX(x)[["spaMM_debug"]])
  }
  stop("option not found")
}

## Setting default package options
.data_IsoriX <- new.env(parent = emptyenv())
.data_IsoriX$IsoriX_options <- list(title_delta_notation = NULL, example_maxtime = 5, Ncpu = 2L, dont_ask = FALSE, spaMM_blocksize = 16000L, spaMM_debug = FALSE) ## put example_maxtime = 500 to check all examples
## otherwise put 5
