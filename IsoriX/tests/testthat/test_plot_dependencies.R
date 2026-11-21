## draws the plots of a test on a null device, closed at the end of the test
local_null_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off(), envir = env)
}

pkg_base <- rownames(utils::installed.packages(priority = "base"))
direct_deps <- function(which) {
  deps <- tools::package_dependencies("IsoriX", db = utils::installed.packages(), which = which)$IsoriX
  sort(setdiff(deps, pkg_base))
}

test_that("plot_dependencies() can show only the packages required by IsoriX (#171)", {
  skip_if_not_installed("igraph")
  local_null_device()
  graph <- plot_dependencies(suggests = FALSE)
  expect_s3_class(graph, "igraph")

  ## vertices: IsoriX and the non-base packages it imports
  expected <- c("IsoriX", direct_deps("Imports"))
  expect_setequal(igraph::V(graph)$name, expected)
  expect_true("spaMM" %in% igraph::V(graph)$name)
  expect_false("stats" %in% igraph::V(graph)$name)

  ## IsoriX depends on all the other vertices
  edges <- igraph::as_data_frame(graph, what = "edges")
  expect_setequal(edges$to[edges$from == "IsoriX"], setdiff(expected, "IsoriX"))
  expect_true(all(edges$type == "imports"))
})

test_that("plot_dependencies() shows the suggested packages by default", {
  skip_if_not_installed("igraph")
  local_null_device()
  graph <- plot_dependencies()
  imports <- direct_deps("Imports")
  suggests <- setdiff(direct_deps("Suggests"), imports)
  expect_setequal(igraph::V(graph)$name, c("IsoriX", imports, suggests))
  expect_true("testthat" %in% igraph::V(graph)$name)

  ## edges from IsoriX: "imports" towards imported packages, "suggests" towards suggested ones
  edges <- igraph::as_data_frame(graph, what = "edges")
  from_IsoriX <- edges[edges$from == "IsoriX", ]
  expect_setequal(from_IsoriX$to[from_IsoriX$type == "imports"], imports)
  expect_setequal(from_IsoriX$to[from_IsoriX$type == "suggests"], suggests)
})

test_that("plot_dependencies() does not alter the random number generator", {
  skip_if_not_installed("igraph")
  local_null_device()

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
