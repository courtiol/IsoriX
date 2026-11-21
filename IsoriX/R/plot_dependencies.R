#' Plot the dependencies of IsoriX
#'
#' This function plots the packages that \pkg{IsoriX} directly requires (i.e.
#' the packages it imports), as well as the dependencies among these packages.
#' This can help to understand where an installation problem comes from, since
#' if one of these packages cannot be installed, \pkg{IsoriX} cannot be
#' installed either. The packages suggested by \pkg{IsoriX}, which are only
#' needed for some extra functionalities, can also be displayed.
#'
#' The dependencies are those of the installed version of \pkg{IsoriX} and of the
#' installed versions of the other packages, so no internet connection is
#' required. Base packages (e.g. \pkg{stats}) are not shown.
#'
#' In the plot, an arrow going from a package to another means that the first
#' package depends on the second one. When suggested packages are shown, dashed
#' arrows go from \pkg{IsoriX} to its suggested packages. If some suggested
#' packages are not installed, they are shown in white: since they are not
#' installed, their own dependencies are not known and thus not displayed.
#'
#' This function requires the package \pkg{igraph} to be installed.
#'
#' @param suggests A *logical* indicating whether the packages suggested by
#'   \pkg{IsoriX} should also be shown (default = `FALSE`).
#' @param seed An *integer* used to set the seed of the random number generator
#'   that controls the layout of the plot (the state of the random number
#'   generator is restored afterwards, so that it is not altered by the call).
#'   If some packages or arrows overlap, try another value to get another
#'   layout.
#'
#' @return The graph of the dependencies (an object of class *igraph*),
#'   returned invisibly.
#' @seealso [`tools::package_dependencies`], which this function uses to find
#'   the dependencies.
#' @export
#' @examples
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   ## packages required by IsoriX:
#'   plot_dependencies()
#'
#'   ## packages required or suggested by IsoriX:
#'   plot_dependencies(suggests = TRUE)
#' }
#'
plot_dependencies <- function(suggests = FALSE, seed = 1L) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("You must install the package igraph for this function to run: `install.packages('igraph')`")
  }

  ## information on the installed packages (if a package is installed in several
  ## libraries, we keep the version found first, i.e. the one R would load)
  db <- utils::installed.packages()
  db <- db[!duplicated(db[, "Package"]), , drop = FALSE]
  pkg_base <- rownames(utils::installed.packages(priority = "base"))

  ## the packages to show: IsoriX, the (non-base) packages it imports and,
  ## possibly, the packages it suggests
  direct_deps <- function(which) {
    sort(setdiff(tools::package_dependencies("IsoriX", db = db, which = which)$IsoriX, pkg_base))
  }
  pkg_imports <- direct_deps("Imports")
  pkg_suggests <- if (suggests) setdiff(direct_deps("Suggests"), pkg_imports) else character(0)
  pkg_shown <- c("IsoriX", pkg_imports, pkg_suggests)
  pkg_installed <- pkg_shown %in% db[, "Package"]

  ## the edges: from IsoriX to the packages it imports or suggests, and between
  ## the other packages shown (using their dependencies, if they are installed)
  edges_IsoriX <- data.frame(
    from = "IsoriX",
    to = c(pkg_imports, pkg_suggests),
    type = rep(c("imports", "suggests"), c(length(pkg_imports), length(pkg_suggests)))
  )
  pkg_others <- setdiff(pkg_shown[pkg_installed], "IsoriX")
  deps_others <- tools::package_dependencies(pkg_others, db = db)
  edges_others <- do.call(rbind, lapply(pkg_others, function(pkg) {
    deps <- intersect(deps_others[[pkg]], pkg_shown)
    if (length(deps) > 0) data.frame(from = pkg, to = deps, type = "imports")
  }))
  graph <- igraph::graph_from_data_frame(rbind(edges_IsoriX, edges_others), vertices = pkg_shown)

  ## styling
  status <- ifelse(pkg_shown == "IsoriX", "IsoriX",
    ifelse(pkg_shown %in% pkg_imports, "imported",
      ifelse(pkg_installed, "suggested", "suggested (not installed)")
    )
  )
  colours <- c("IsoriX" = "orange", "imported" = "lightblue", "suggested" = "lightyellow", "suggested (not installed)" = "white")
  many <- length(pkg_shown) > 10

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
    vertex.color = colours[status],
    vertex.size = if (many) 24 else 38,
    vertex.label.color = "black",
    vertex.label.cex = if (many) 0.6 else 0.75,
    edge.arrow.size = if (many) 0.3 else 0.4,
    edge.lty = ifelse(igraph::E(graph)$type == "suggests", 2, 1)
  )

  if (suggests) {
    legend_status <- intersect(names(colours), status)
    graphics::legend("bottomleft",
      legend = c(legend_status, "depends on", "suggests"),
      pch = c(rep(21, length(legend_status)), NA, NA),
      pt.bg = c(colours[legend_status], NA, NA),
      pt.cex = 1.5,
      lty = c(rep(NA, length(legend_status)), 1, 2),
      col = c(rep("black", length(legend_status)), "grey40", "grey40"),
      bty = "n", cex = 0.7
    )
  }

  invisible(graph)
}
