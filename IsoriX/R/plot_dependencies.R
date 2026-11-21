#' Plot the dependencies of IsoriX
#'
#' This function plots the packages that \pkg{IsoriX} directly requires (i.e.
#' the packages it imports), as well as the dependencies among these packages.
#' This can help to understand where an installation problem comes from, since
#' if one of these packages cannot be installed, \pkg{IsoriX} cannot be
#' installed either.
#'
#' The dependencies are those of the installed version of \pkg{IsoriX} and of the
#' installed versions of the packages it imports, so no internet connection is
#' required. Base packages (e.g. \pkg{stats}) are not shown.
#'
#' In the plot, an arrow going from a package to another means that the first
#' package depends on the second one.
#'
#' This function requires the package \pkg{igraph} to be installed.
#'
#' @param plot A *logical* indicating whether the graph should be plotted
#'   (default) or only returned.
#' @param seed An *integer* used to set the seed of the random number generator
#'   that controls the layout of the plot (the state of the random number
#'   generator is restored afterwards, so that it is not altered by the call).
#'
#' @return The graph of the dependencies (an object of class *igraph*),
#'   returned invisibly.
#' @seealso [`tools::package_dependencies`], which this function uses to find
#'   the dependencies.
#' @export
#' @examples
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   plot_dependencies()
#' }
#'
plot_dependencies <- function(plot = TRUE, seed = 1L) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("You must install the package igraph for this function to run: `install.packages('igraph')`")
  }

  ## information on the installed packages (if a package is installed in several
  ## libraries, we keep the version found first, i.e. the one R would load)
  db <- utils::installed.packages()
  db <- db[!duplicated(db[, "Package"]), , drop = FALSE]
  pkg_base <- rownames(utils::installed.packages(priority = "base"))

  ## the core packages: IsoriX and the (non-base) packages it imports
  pkg_imports <- tools::package_dependencies("IsoriX", db = db, which = "Imports")$IsoriX
  pkg_core <- c("IsoriX", sort(setdiff(pkg_imports, pkg_base)))

  ## the dependencies of each core package, restricted to the core packages
  deps_core <- tools::package_dependencies(pkg_core, db = db)
  edges <- do.call(rbind, lapply(pkg_core, function(pkg) {
    deps <- intersect(deps_core[[pkg]], pkg_core)
    if (length(deps) > 0) data.frame(from = pkg, to = deps)
  }))
  graph <- igraph::graph_from_data_frame(edges, vertices = pkg_core)

  if (plot) {
    ## the layout is random, so we use a seed while restoring the state of the
    ## random number generator afterwards
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
    igraph::plot.igraph(graph,
      vertex.color = ifelse(igraph::V(graph)$name == "IsoriX", "orange", "lightblue"),
      vertex.size = 38, vertex.label.color = "black", vertex.label.cex = 0.75,
      edge.arrow.size = 0.4
    )
  }

  invisible(graph)
}
