test_that("plot_dependencies() returns the graph of the dependencies of IsoriX (#171)", {
  skip_if_not_installed("igraph")
  graph <- plot_dependencies(plot = FALSE)
  expect_s3_class(graph, "igraph")

  ## vertices: IsoriX and the non-base packages it imports
  pkg_base <- rownames(utils::installed.packages(priority = "base"))
  imports <- tools::package_dependencies("IsoriX", db = utils::installed.packages(), which = "Imports")$IsoriX
  expected <- c("IsoriX", sort(setdiff(imports, pkg_base)))
  expect_setequal(igraph::V(graph)$name, expected)
  expect_true("spaMM" %in% igraph::V(graph)$name)
  expect_false("stats" %in% igraph::V(graph)$name)

  ## IsoriX depends on all the other vertices
  edges <- igraph::as_data_frame(graph, what = "edges")
  expect_setequal(edges$to[edges$from == "IsoriX"], setdiff(expected, "IsoriX"))
})

test_that("plot_dependencies() plots without altering the random number generator", {
  skip_if_not_installed("igraph")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  ## with an existing seed
  set.seed(42)
  seed_before <- .Random.seed
  expect_no_error(plot_dependencies())
  expect_identical(.Random.seed, seed_before)

  ## without any seed
  rm(".Random.seed", envir = globalenv())
  plot_dependencies()
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
