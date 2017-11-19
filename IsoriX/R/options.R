
#' Setting and displaying the options of the package
#'
#' @name options
#' @aliases options options_IsoriX getOption_IsoriX
#' @param ... A named value or a list of named values. The following values, with their defaults, are used:
#' \describe{
#'   \item{example_maxtime}{The number of seconds allowed for a given example to run. It is used to control whether the longer examples should be run or not based on the comparison between this option and the approximate running time of the example on our computers.}
#'   \item{Ncpu}{An \var{integer} corresponding to the number of cores to be used (for future versions)}
#'   \item{dont_ask}{A \var{logical} indicating if the user prompt during interactive session during plotting must be inactivated (for development purposes only)}
#' }
#'
#' @return The options are invisibly returned in an object called \code{IsoriX:::.data_IsoriX$options}
#' @export
#'
#' @examples
#' options_IsoriX()
#' getOption_IsoriX("example_maxtime")
#'
#' \dontrun{
#' options_IsoriX(example_maxtime = 30)
#' options_IsoriX()
#' }

options_IsoriX <- function(...) { ## as in spaMM
  if (nargs() == 0) return(.data_IsoriX$IsoriX_options)
  current <- .data_IsoriX$IsoriX_options
  temp <- list(...)
  if (length(temp) == 1 && is.null(names(temp))) {
    arg <- temp[[1]]
    switch(mode(arg),
           list = temp <- arg,
           character = return(.data_IsoriX$IsoriX_options[arg]),
           stop("invalid argument: ", sQuote(arg)))
  }
  if (length(temp) == 0) return(current)
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
    warning(paste(options_IsoriX(x)[["Ncpu"]],
                  "CPUs were requested, but the maximum number of CPU on this machine is",
                  parallel::detectCores(),
                  "so the Ncpu option of this package has been corrected"))
    options_IsoriX(Ncpu = parallel::detectCores())
  }
  if (x == "Ncpu") {
    return(options_IsoriX(x)[["Ncpu"]])
  }
  if (x == "example_maxtime") {
    return(options_IsoriX(x)[["example_maxtime"]])
  }
  if (x == "dont_ask") {
    return(options_IsoriX(x)[["dont_ask"]])
  }
  stop("option not found")
}

## Setting default package options
.data_IsoriX <- new.env(parent = emptyenv())
.data_IsoriX$IsoriX_options <- list(example_maxtime = 500, Ncpu = 2L, dont_ask = FALSE)  ## put example_maxtime = 500 to check all examples
                                                                                  ## otherwise put 5

