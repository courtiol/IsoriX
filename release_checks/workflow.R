## Full IsoriX workflow used to compare two versions of IsoriX before a release.
##
## This script is not meant to be run directly: it is run by compare_versions.R
## once for each version of IsoriX, each time in a fresh R session where only
## the version of IsoriX differs.
##
## Usage: Rscript workflow.R <output_directory> [library]
##
## If `library` is given, IsoriX is loaded from that library (and the workflow
## stops if it cannot be), whatever the other libraries of the R session.
##
## It saves all plots as PNG files in <output_directory>/plots, some key numbers
## in <output_directory>/summaries.txt and a log of all steps (status, time,
## error message) in <output_directory>/log.csv.
##
## Only use features available in both versions being compared: a step that
## fails is recorded in the log and the workflow continues.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2) stop("Usage: Rscript workflow.R <output_directory> [library]")
out_dir <- args[1]
lib <- if (length(args) == 2) args[2] else NULL
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(IsoriX, lib.loc = lib))
loaded_from <- normalizePath(dirname(find.package("IsoriX")))
if (!is.null(lib) && loaded_from != normalizePath(lib)) {
  stop("IsoriX was loaded from ", loaded_from, " instead of ", normalizePath(lib), ": the comparison would not be valid.")
}
options_IsoriX(dont_ask = TRUE)
set.seed(1L)

## Helpers ---------------------------------------------------------------------

log_steps <- data.frame(step = character(), status = character(), seconds = numeric(), message = character())

## runs a step, records its status and timing, and returns its value (or NULL if it failed)
run_step <- function(name, expr) {
  message("* ", name)
  start <- Sys.time()
  status <- "ok"
  msg <- ""
  value <- tryCatch(
    suppressWarnings(suppressMessages(force(expr))),
    error = function(e) {
      status <<- "error"
      msg <<- conditionMessage(e)
      NULL
    }
  )
  seconds <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 1)
  log_steps[nrow(log_steps) + 1, ] <<- list(name, status, seconds, msg)
  invisible(value)
}

## draws a plot into PNG file(s); multi-page plots produce one file per page
save_plot <- function(name, expr, width = 900, height = 900) {
  run_step(paste("plot:", name), {
    grDevices::png(file.path(plot_dir, paste0(name, "-%d.png")), width = width, height = height, res = 110, type = "cairo")
    on.exit(grDevices::dev.off(), add = TRUE)
    p <- expr
    if (inherits(p, "trellis")) print(p) ## lattice objects returned with plot = FALSE
    invisible(NULL)
  })
}

summaries <- character()
add_summary <- function(label, x) {
  summaries <<- c(summaries, paste0("## ", label), utils::capture.output(print(x)), "")
}

raster_summary <- function(r) {
  vals <- terra::values(r, mat = TRUE)
  round(t(apply(vals, 2, function(v) c(mean = mean(v, na.rm = TRUE), sd = stats::sd(v, na.rm = TRUE), NAs = sum(is.na(v))))), 6)
}

add_summary("IsoriX version", as.character(utils::packageVersion("IsoriX")))
writeLines(c(as.character(utils::packageVersion("IsoriX")), loaded_from), file.path(out_dir, "isorix_loaded.txt"))

## Sources and isoscape ----------------------------------------------------------

GNIPDataDEagg <- run_step("prepsources", prepsources(data = GNIPDataDE))
add_summary("prepsources: dimensions", dim(GNIPDataDEagg))

GermanFit <- run_step("isofit", isofit(
  data = GNIPDataDEagg,
  mean_model_fix = list(elev = TRUE, lat_abs = TRUE),
  verbose = FALSE
))
if (!is.null(GermanFit)) {
  add_summary("isofit: fixed effects of mean_fit", round(spaMM::fixef(GermanFit$mean_fit), 6))
  add_summary("isofit: fixed effects of disp_fit", round(spaMM::fixef(GermanFit$disp_fit), 6))
  add_summary("isofit: AIC of mean_fit", round(suppressWarnings(AIC(GermanFit$mean_fit, verbose = FALSE)), 4))
  save_plot("isofit", plot(GermanFit))
}

GermanScape <- run_step("isoscape", isoscape(raster = ElevRasterDE, isofit = GermanFit, verbose = FALSE))
if (!is.null(GermanScape)) {
  add_summary("isoscape: layers", raster_summary(GermanScape$isoscapes))
  for (layer in names(GermanScape$isoscapes)) {
    save_plot(paste0("isoscape_", layer), plot(GermanScape, which = layer, plot = FALSE))
  }
  save_plot("isoscape_mean_no_mask_rainbow", plot(GermanScape, mask = list(mask = NULL), palette = list(fn = grDevices::rainbow), plot = FALSE))
}

## Calibration -------------------------------------------------------------------

CalibAlien <- run_step("calibfit wild", calibfit(data = CalibDataAlien, isofit = GermanFit, verbose = FALSE))
if (!is.null(CalibAlien)) {
  add_summary("calibfit wild: parameters", round(CalibAlien$param, 6))
  save_plot("calibfit_wild", plot(CalibAlien))
}

CalibDesk <- run_step("calibfit desk", calibfit(
  data = data.frame(intercept = 1.67, slope = 0.48, intercept_se = 1.65, slope_se = 0.03, resid_var = 3.96),
  method = "desk"
))
if (!is.null(CalibDesk)) {
  save_plot("calibfit_desk", plot(CalibDesk, xlim = c(-100, 100), ylim = c(-50, 50)))
}

## Assignment --------------------------------------------------------------------

AssignmentDry <- run_step("isofind with calibration", isofind(
  data = AssignDataAlien, isoscape = GermanScape, calibfit = CalibAlien, verbose = FALSE
))
if (!is.null(AssignmentDry)) {
  add_summary("isofind: group p-values", raster_summary(AssignmentDry$group$pv))
  add_summary("isofind: individual p-values", raster_summary(AssignmentDry$sample$pv))
  save_plot("isofind_group", plot(AssignmentDry, who = "group", plot = FALSE))
  save_plot("isofind_group_no_mask", plot(AssignmentDry, who = "group", mask = list(mask = NULL), plot = FALSE))
  save_plot("isofind_individuals_1to8",
    plot(AssignmentDry, who = 1:8, sources = list(draw = FALSE), calibs = list(draw = FALSE), plot = FALSE),
    width = 1400
  )
  save_plot("isofind_Alien_10", plot(AssignmentDry, who = "Alien_10", plot = FALSE))
}

GPIso <- GNIPDataDEagg[GNIPDataDEagg$source_ID == "GARMISCH-PARTENKIRCHEN", "mean_source_value"]
AssignedGP <- run_step("isofind without calibration", isofind(
  data = data.frame(sample_value = GPIso, sample_ID = "GARMISCH-PARTENKIRCHEN"),
  isoscape = GermanScape, calibfit = NULL, verbose = FALSE
))
if (!is.null(AssignedGP)) {
  add_summary("isofind without calibration: p-values", raster_summary(AssignedGP$sample$pv))
  save_plot("isofind_GP", plot(AssignedGP, plot = FALSE))
}

## Monthly isoscapes -------------------------------------------------------------

GNIPDataDEmonthly <- run_step("prepsources monthly", prepsources(data = GNIPDataDE, split_by = "month"))
GermanMultiFit <- run_step("isomultifit", isomultifit(
  data = GNIPDataDEmonthly,
  mean_model_fix = list(elev = TRUE, lat_abs = TRUE),
  verbose = FALSE
))
GermanMultiscape <- run_step("isomultiscape", isomultiscape(raster = ElevRasterDE, isofit = GermanMultiFit, verbose = FALSE))
if (!is.null(GermanMultiscape)) {
  add_summary("isomultiscape: layers", raster_summary(GermanMultiscape$isoscapes))
  save_plot("isomultiscape_mean", plot(GermanMultiscape, plot = FALSE))
}
GermanMultiscapeW <- run_step("isomultiscape weighted", isomultiscape(
  raster = ElevRasterDE, isofit = GermanMultiFit, weighting = PrecipBrickDE, verbose = FALSE
))
if (!is.null(GermanMultiscapeW)) {
  add_summary("isomultiscape weighted: layers", raster_summary(GermanMultiscapeW$isoscapes))
  save_plot("isomultiscape_weighted_mean", plot(GermanMultiscapeW, plot = FALSE))
}

## Outputs -----------------------------------------------------------------------

writeLines(summaries, file.path(out_dir, "summaries.txt"))
utils::write.csv(log_steps, file.path(out_dir, "log.csv"), row.names = FALSE)
message("Done: ", sum(log_steps$status == "error"), " step(s) failed.")
