## Measure how long the tests of IsoriX take depending on the number of R
## processes used to run the test files in parallel, with a single thread for
## linear algebra (BLAS) in each process, or optionally with the default
## threading of the BLAS library too.
##
## This is not a release check: it is a diagnostic to run occasionally, e.g. to
## choose the number of processes to use on a given computer, or to check
## whether running the tests in parallel still pays off after changes to the
## tests (see the README of the repository, section "For the maintainer").
##
## Usage, from the root of the repository:
##   Rscript release_checks/benchmark_test_processes.R
##   Rscript release_checks/benchmark_test_processes.R 1,2,3,4,6,8 2       ## processes, repetitions
##   Rscript release_checks/benchmark_test_processes.R 1,2,4 1 --both-blas ## also with the default BLAS threading
##
## Each run uses a fresh R session (so that nothing is already loaded) and loads
## IsoriX from the source folder, as devtools::test() does. A run with 1 process
## runs the test files one after the other.
##
## The results are written to release_checks/output/benchmark-<date-time>/
## (ignored by git): results.csv and plot.png.
##
## Requires the packages callr (installed with testthat) and pkgload.

args <- commandArgs(trailingOnly = TRUE)
both_blas <- "--both-blas" %in% args
args <- setdiff(args, "--both-blas")
n_cpus <- if (length(args) >= 1) as.integer(strsplit(args[1], ",")[[1]]) else c(1:6, seq(8, 16, by = 2))
n_reps <- if (length(args) >= 2) as.integer(args[2]) else 1L

if (!file.exists("DESCRIPTION") || !dir.exists("release_checks")) {
  stop("Please run this script from the root of the repository.")
}
for (pkg in c("callr", "pkgload")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("The package ", pkg, " is required: install.packages('", pkg, "')")
}

## the variables limiting linear algebra libraries to a single thread (as in tests/testthat.R)
single_thread <- c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

time_tests <- function(n, blas) {
  env <- c(callr::rcmd_safe_env(),
    NOT_CRAN = "true",
    TESTTHAT_PARALLEL = if (n == 1) "false" else "true",
    TESTTHAT_CPUS = as.character(n)
  )
  if (blas == "single thread") {
    env <- c(env, single_thread)
  } else {
    env <- env[!names(env) %in% names(single_thread)] ## default threading of the BLAS library
  }
  time <- system.time(
    res <- callr::r(function() {
      as.data.frame(testthat::test_local(".", reporter = "silent", stop_on_failure = FALSE))
    }, env = env)
  )[["elapsed"]]
  data.frame(blas = blas, processes = n, seconds = round(time, 1), failed = sum(res$failed))
}

blas_settings <- if (both_blas) c("single thread", "default threading") else "single thread"
already_set <- names(single_thread)[Sys.getenv(names(single_thread)) != ""]
if (both_blas && length(already_set) > 0) {
  stop(
    "The variable(s) ", paste(already_set, collapse = ", "), " are already defined (e.g. in your .Renviron), ",
    "so the tests cannot be run with the default threading of the BLAS library: ",
    "remove them before using --both-blas."
  )
}
runs <- expand.grid(rep = seq_len(n_reps), processes = n_cpus, blas = blas_settings, stringsAsFactors = FALSE)

message("BLAS/LAPACK used by R: ", paste(extSoftVersion()[["BLAS"]], La_library(), sep = " / "))
message("Timing ", nrow(runs), " runs of the tests...")

results <- do.call(rbind, lapply(seq_len(nrow(runs)), \(i) {
  r <- time_tests(runs$processes[i], runs$blas[i])
  message(sprintf("%-17s %2d process(es): %6.1f s (failed: %d)", r$blas, r$processes, r$seconds, r$failed))
  r
}))

## outputs
out_dir <- file.path("release_checks", "output", paste0("benchmark-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))
dir.create(out_dir, recursive = TRUE)
utils::write.csv(results, file.path(out_dir, "results.csv"), row.names = FALSE)

grDevices::png(file.path(out_dir, "plot.png"), width = 7, height = 5, units = "in", res = 150)
cols <- c("single thread" = "#1b7837", "default threading" = "#762a83")
plot(seconds ~ processes, data = results, type = "n", ylim = c(0, max(results$seconds)), xaxt = "n",
  xlab = "Number of processes running the test files", ylab = "Duration of the tests (s)"
)
axis(1, at = sort(unique(results$processes)))
for (b in unique(results$blas)) {
  d <- results[results$blas == b, ]
  means <- tapply(d$seconds, d$processes, mean)
  points(d$processes, d$seconds, col = cols[[b]], pch = 16)
  lines(as.numeric(names(means)), means, col = cols[[b]])
}
legend("bottomright", legend = paste("BLAS:", unique(results$blas)), col = cols[unique(results$blas)], lwd = 1, pch = 16, bty = "n")
invisible(grDevices::dev.off())

print(results, row.names = FALSE)
message("Results written to ", out_dir)
