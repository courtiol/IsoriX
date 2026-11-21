## Compare all plots (and some key numbers) produced by the full IsoriX workflow
## between a previous version of IsoriX and a candidate version, before a release.
##
## Usage, from the root of the repository:
##   Rscript release_checks/compare_versions.R [previous] [candidate]
##
##   previous:  "cran" (default) to use the version currently on CRAN,
##              or a git reference (e.g. a tag such as "v0.9.4", a branch or a commit)
##   candidate: path to the source of the candidate version (default: "IsoriX",
##              i.e. the current state of the working directory)
##
## Both versions are installed in separate temporary libraries and the workflow
## (release_checks/workflow.R) is run once with each version, in a fresh R session.
## All other packages come from your usual library, so both runs use exactly the
## same dependencies on the same computer: all differences in the outputs come
## from IsoriX itself.
##
## The output is written to release_checks/output/<date-time>/ (ignored by git):
## open report.html in a browser to review all plots side by side.
##
## Requires the packages magick (to compare images) and, for previous = "cran",
## an internet connection.

args <- commandArgs(trailingOnly = TRUE)
previous <- if (length(args) >= 1) args[1] else "cran"
candidate <- if (length(args) >= 2) args[2] else "IsoriX"

if (!file.exists(file.path(candidate, "DESCRIPTION"))) {
  stop("Please run this script from the root of the repository (no package found in '", candidate, "').")
}
if (!requireNamespace("magick", quietly = TRUE)) {
  stop("The package magick is required: install.packages('magick')")
}

out_dir <- file.path("release_checks", "output", format(Sys.time(), "%Y%m%d-%H%M%S"))
dir.create(out_dir, recursive = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
rcmd <- file.path(R.home("bin"), "R")

## Installation of the two versions ----------------------------------------------

install_source <- function(path, lib) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  status <- system2(rcmd, c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(path)),
    stdout = FALSE, stderr = FALSE
  )
  if (status != 0) stop("Installation of IsoriX from '", path, "' failed.")
}

lib_previous <- file.path(tempdir(), "lib_previous")
lib_candidate <- file.path(tempdir(), "lib_candidate")

message("Installing the previous version (", previous, ")...")
if (tolower(previous) == "cran") {
  dir.create(lib_previous, recursive = TRUE, showWarnings = FALSE)
  utils::install.packages("IsoriX", lib = lib_previous, repos = "https://cloud.r-project.org", dependencies = FALSE, quiet = TRUE)
} else {
  src_previous <- file.path(tempdir(), "src_previous")
  dir.create(src_previous, showWarnings = FALSE)
  tar_file <- file.path(tempdir(), "previous.tar")
  if (system2("git", c("archive", "--format=tar", "-o", shQuote(tar_file), shQuote(previous), "IsoriX")) != 0) {
    stop("Could not extract the folder IsoriX from the git reference '", previous, "'.")
  }
  utils::untar(tar_file, exdir = src_previous)
  install_source(file.path(src_previous, "IsoriX"), lib_previous)
}

message("Installing the candidate version (", candidate, ")...")
install_source(candidate, lib_candidate)

## Running the workflow with each version ----------------------------------------

run_workflow <- function(lib, label) {
  dir <- file.path(out_dir, label)
  dir.create(dir, showWarnings = FALSE)
  message("Running the workflow with the ", label, " version (this takes a few minutes)...")
  ## R_LIBS puts the temporary library first, before your usual libraries
  system2(rscript, c(shQuote(file.path("release_checks", "workflow.R")), shQuote(dir)),
    env = paste0("R_LIBS=", lib), stdout = file.path(dir, "console.txt"), stderr = file.path(dir, "console.txt")
  )
  dir
}

dir_previous <- run_workflow(lib_previous, "previous")
dir_candidate <- run_workflow(lib_candidate, "candidate")

read_version <- function(dir) {
  s <- tryCatch(readLines(file.path(dir, "summaries.txt")), error = function(e) character())
  i <- grep("^## IsoriX version", s)
  if (length(i)) gsub('^\\[1\\] |"', "", s[i + 1]) else "unknown (see console.txt)"
}
version_previous <- read_version(dir_previous)
version_candidate <- read_version(dir_candidate)

## Comparison of the plots -------------------------------------------------------

dir.create(file.path(out_dir, "diff"), showWarnings = FALSE)
plots <- sort(union(
  list.files(file.path(dir_previous, "plots"), pattern = "\\.png$"),
  list.files(file.path(dir_candidate, "plots"), pattern = "\\.png$")
))

compare_plot <- function(file) {
  f_prev <- file.path(dir_previous, "plots", file)
  f_cand <- file.path(dir_candidate, "plots", file)
  if (!file.exists(f_prev)) {
    return(data.frame(plot = file, status = "only in candidate", pct_diff = NA))
  }
  if (!file.exists(f_cand)) {
    return(data.frame(plot = file, status = "only in previous", pct_diff = NA))
  }
  img_prev <- magick::image_read(f_prev)
  img_cand <- magick::image_read(f_cand)
  info_prev <- magick::image_info(img_prev)
  info_cand <- magick::image_info(img_cand)
  if (info_prev$width != info_cand$width || info_prev$height != info_cand$height) {
    return(data.frame(plot = file, status = "size changed", pct_diff = NA))
  }
  comparison <- magick::image_compare(img_cand, img_prev, metric = "AE")
  n_diff <- attr(comparison, "distortion")
  if (n_diff > 0) magick::image_write(comparison, file.path(out_dir, "diff", file))
  data.frame(
    plot = file,
    status = if (n_diff > 0) "changed" else "identical",
    pct_diff = round(100 * n_diff / (info_prev$width * info_prev$height), 3)
  )
}

results <- do.call(rbind, lapply(plots, compare_plot))
results <- results[order(results$status == "identical", results$plot), ]
utils::write.csv(results, file.path(out_dir, "comparison.csv"), row.names = FALSE)

## Comparison of the logs and summaries ------------------------------------------

read_log <- function(dir) {
  tryCatch(utils::read.csv(file.path(dir, "log.csv")), error = function(e) {
    data.frame(step = "(workflow did not complete: see console.txt)", status = "error", seconds = NA, message = "")
  })
}
log_previous <- read_log(dir_previous)
log_candidate <- read_log(dir_candidate)
steps <- merge(log_previous, log_candidate, by = "step", all = TRUE, suffixes = c("_previous", "_candidate"), sort = FALSE)

summaries_previous <- tryCatch(readLines(file.path(dir_previous, "summaries.txt")), error = function(e) character())
summaries_candidate <- tryCatch(readLines(file.path(dir_candidate, "summaries.txt")), error = function(e) character())
summaries_identical <- identical(summaries_previous[-(1:2)], summaries_candidate[-(1:2)]) ## (first lines = versions)

## HTML report -------------------------------------------------------------------

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
img <- function(path) {
  if (file.exists(file.path(out_dir, path))) {
    sprintf('<a href="%s"><img src="%s" loading="lazy"></a>', path, path)
  } else {
    "<em>none</em>"
  }
}

n_changed <- sum(results$status != "identical")
n_failed <- c(previous = sum(log_previous$status == "error"), candidate = sum(log_candidate$status == "error"))

html <- c(
  "<!doctype html><html><head><meta charset='utf-8'><title>IsoriX version comparison</title>",
  "<style>",
  "body{font-family:sans-serif;margin:24px;max-width:1500px} table{border-collapse:collapse;margin-bottom:24px}",
  "td,th{border:1px solid #ccc;padding:4px 8px;vertical-align:top;text-align:left}",
  "img{width:420px} .changed,.error{background:#fde2e2} .identical,.ok{background:#e6f4e6}",
  ".other{background:#fff3cd} pre{font-size:12px;max-height:500px;overflow:auto}",
  "</style></head><body>",
  sprintf("<h1>IsoriX: %s (previous) vs %s (candidate)</h1>", esc(version_previous), esc(version_candidate)),
  sprintf("<p>Generated on %s. Previous version from: <code>%s</code>; candidate from: <code>%s</code>.</p>", format(Sys.time()), esc(previous), esc(candidate)),
  sprintf(
    "<p><strong>%d of %d plot files differ</strong> (or exist in only one version); failed workflow steps: %d (previous), %d (candidate); key numbers: <strong>%s</strong>.</p>",
    n_changed, nrow(results), n_failed["previous"], n_failed["candidate"], if (summaries_identical) "identical" else "different"
  ),
  "<h2>Plots</h2><table><tr><th>plot</th><th>status</th><th>% pixels differing</th><th>previous</th><th>candidate</th><th>difference</th></tr>",
  vapply(seq_len(nrow(results)), function(i) {
    r <- results[i, ]
    cls <- if (r$status %in% c("changed", "identical")) r$status else "other"
    sprintf(
      "<tr class='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      cls, esc(r$plot), r$status, ifelse(is.na(r$pct_diff), "", r$pct_diff),
      img(file.path("previous", "plots", r$plot)), img(file.path("candidate", "plots", r$plot)), img(file.path("diff", r$plot))
    )
  }, character(1)),
  "</table>",
  "<h2>Workflow steps</h2><table><tr><th>step</th><th>status (previous)</th><th>seconds (previous)</th><th>status (candidate)</th><th>seconds (candidate)</th><th>error message</th></tr>",
  vapply(seq_len(nrow(steps)), function(i) {
    s <- steps[i, ]
    msg <- paste(stats::na.omit(unique(c(s$message_previous, s$message_candidate))), collapse = " | ")
    sprintf(
      "<tr><td>%s</td><td class='%s'>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td></tr>",
      esc(s$step), s$status_previous, s$status_previous, s$seconds_previous,
      s$status_candidate, s$status_candidate, s$seconds_candidate, esc(msg)
    )
  }, character(1)),
  "</table>",
  "<h2>Key numbers</h2><table><tr><th>previous</th><th>candidate</th></tr>",
  sprintf("<tr><td><pre>%s</pre></td><td><pre>%s</pre></td></tr></table>", esc(paste(summaries_previous, collapse = "\n")), esc(paste(summaries_candidate, collapse = "\n"))),
  "<p>Console outputs: <a href='previous/console.txt'>previous</a>, <a href='candidate/console.txt'>candidate</a>.</p>",
  "</body></html>"
)
writeLines(html, file.path(out_dir, "report.html"))

message(
  "\n", n_changed, " of ", nrow(results), " plot files differ; failed steps: ", n_failed["previous"], " (previous), ",
  n_failed["candidate"], " (candidate); key numbers: ", if (summaries_identical) "identical" else "different",
  ".\nReport: ", normalizePath(file.path(out_dir, "report.html"))
)
