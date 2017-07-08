
#' Setting and displaying the options of the package
#'
#' @name options
#' @aliases options IsoriX.options IsoriX.getOption
#' @param ... A named value or a list of named values. The following values, with their defaults, are used:
#' \describe{
#'   \item{example_maxtime}{The number of seconds allowed for a given example to run. It is used to control whether the longer examples should be run or not based on the comparison between this option and the approximate running time of the example on our computers.}
#'   \item{Ncpu}{An \var{integer} corresponding to the number of cores to be used (for future version)}
#' }
#'
#' @return The options invisibly in an object called \code{.IsoriX.data$options}
#' @export
#'
#' @examples
#' IsoriX.options()
#' IsoriX.getOption("example_maxtime")
#'
#' \dontrun{
#' IsoriX.options(example_maxtime = 30)
#' }

IsoriX.options <- function(...) { ## as in spaMM
  if (nargs() == 0) return(.IsoriX.data$options)
  current <- .IsoriX.data$options
  temp <- list(...)
  if (length(temp) == 1 && is.null(names(temp))) {
    arg <- temp[[1]]
    switch(mode(arg),
           list = temp <- arg,
           character = return(.IsoriX.data$options[arg]),
           stop("invalid argument: ", sQuote(arg)))
  }
  if (length(temp) == 0) return(current)
  n <- names(temp)
  if (is.null(n)) stop("options must be given by name")
  current[n] <- temp
  .IsoriX.data$options <- current
  invisible(current)
}


#' @rdname options
#' @param x A character string holding an option name.
#' @export

IsoriX.getOption <- function(x) {
  if (x == "Ncpu" && (IsoriX.options(x)[[1]] > parallel::detectCores())) {
    warning(paste(IsoriX.options(x)[[1]],
                  "CPUs were requested, but the maximum number of CPU on this machine is",
                  parallel::detectCores(),
                  "so the Ncpu option of this package has been corrected"))
    IsoriX.options(Ncpu = parallel::detectCores())
  }
  IsoriX.options(x)[[1]]
}

## Setting default package options
.IsoriX.data <- new.env(parent = emptyenv())
.IsoriX.data$options <- list(example_maxtime = 5, Ncpu = 2L)  ## put example_maxtime = 500 to check all examples

