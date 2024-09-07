#' Setting and displaying the options of the package
#'
#' ** Information on the settings for the delta notation **
#'
#' Note that if the delta notation is not successfully rendered on
#' your plots (which can happen for various reasons related to fonts, encoding
#' settings, graphic devices and perhaps more), you may try to use e.g.
#' `options_IsoriX(title_delta_notation = bquote(italic("\u03B4")**2*H[p]))`
#' to override the default for all plots. The default does correspond to
#' `options_IsoriX(title_delta_notation = bquote(delta**2*H))`. If you
#' are working with oxygen (rather than with deuterium), modifying the
#' global option is also a good place to do so. You may do:
#' `options_IsoriX(title_delta_notation = bquote(delta**18*O))`.
#'
#' @name options
#' @aliases options options_IsoriX getOption_IsoriX
#' @param ... A named value or a list of named values. The following values, with their defaults, are used:
#' \describe{
#'   \item{title_delta_notation}{a name, call, or expression used as default in titles to refer to the delta notation}
#'   \item{example_maxtime}{The number of seconds allowed for a given example to run. It is used to control whether the longer examples should be run or not based on the comparison between this option and the approximate running time of the example on our computers.}
#'   \item{Ncpu}{An *integer* corresponding to the number of cores to be used (in functions that can handle parallel processing)}
#'   \item{dont_ask}{A *logical* indicating if the user prompt during interactive session during plotting must be inactivated (for development purposes only)}
#'   \item{spaMM_debugmod}{A *logical* indicating if the warnings and errors produced by the spaMM package should stopped being turned into messages (for development purposes only)}
#' }
#'
#' @return The options are invisibly returned in an object called `IsoriX:::.data_IsoriX$options`
#' @export
#'
#' @examples
#' OldOptions <- options_IsoriX()
#' OldOptions
#' getOption_IsoriX("title_delta_notation")
#' getOption_IsoriX("example_maxtime")
#' options_IsoriX(example_maxtime = 30)
#' options_IsoriX()
#' options_IsoriX(example_maxtime = OldOptions$example_maxtime)
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
  if (x == "spaMM_debug") {
    return(options_IsoriX(x)[["spaMM_debug"]])
  }
  stop("option not found")
}

## Setting default package options
.data_IsoriX <- new.env(parent = emptyenv())
.data_IsoriX$IsoriX_options <- list(title_delta_notation = bquote(delta**2 * H), example_maxtime = 5, Ncpu = 2L, dont_ask = FALSE, spaMM_debug = FALSE) ## put example_maxtime = 500 to check all examples
## otherwise put 5
