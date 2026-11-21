#' Map the aggregated source data
#'
#' This function draws maps showing, for each source location (e.g. weather
#' station), the mean, the standard deviation and the number of isotopic
#' measurements stored in aggregated source data (as produced by
#' [`prepsources`]). Such maps help to explore the source data before fitting
#' the isoscape: e.g. to see where sources are, which areas are poorly covered,
#' which locations rely on few measurements, or which values look unusual.
#'
#' Each statistic is shown on its own map. By default, the value of each source
#' location is represented by the colour of a point (see the colour key on the
#' right side of each map). For datasets containing few source locations, you
#' can instead write the values on the maps by setting `labels = TRUE`.
#'
#' The arguments `points`, `borders` and `palette` are lists which can be used
#' to fine-tune the maps, in the same way as for the other plotting functions of
#' IsoriX (see [`plots`]). The argument `points` controls the display of the
#' points (`pch`, `cex`, `lwd`, and `col` for the colour of their outline) or of
#' the labels (`cex`, `col` and `digits` for the number of decimals). The
#' argument `borders` controls the display of the borders (`borders`, `lwd`,
#' `col`): by default, the country borders provided with IsoriX
#' ([`CountryBorders`]) are drawn, use `borders = list(borders = NULL)` to draw
#' none. The argument `palette` controls the colours (`fn`, `step`, `range`,
#' `n_labels`, `digits`), as for [`plot.ISOSCAPE`]; each map uses its own colour
#' scale.
#'
#' Source locations straddling the International Date Line are drawn on a
#' continuous map (e.g. from 70 to 210 degrees of longitude).
#'
#' @param data A *dataframe* containing aggregated source data, as produced by
#'   [`prepsources`] (without splitting the data, i.e. with a single row per
#'   source location)
#' @param which A *vector of strings* indicating which statistics to map, among
#'   `"mean"` (mean of the isotopic values), `"sd"` (standard deviation of the
#'   isotopic values) and `"n"` (number of measurements). By default, all three
#'   are mapped, side by side.
#' @param labels A *logical* indicating whether the values should be written on
#'   the maps instead of being represented by coloured points (default =
#'   `FALSE`)
#' @param points A *list* containing information for the display of the points
#'   or labels (see details)
#' @param borders A *list* containing information for the display of borders
#'   (see details)
#' @param palette A *list* containing information for the display of the colours
#'   (see details)
#' @param plot A *logical* indicating whether the maps should be drawn (default
#'   = `TRUE`); if `FALSE`, the maps are only returned
#'
#' @return A named *list* of maps (objects of class *trellis*, one per
#'   statistic), returned invisibly. Each map can be drawn again using
#'   [`print`].
#' @seealso [`prepsources`] for the function aggregating the source data
#'
#'   [`plots`] for the other plotting functions of IsoriX
#' @export
#' @examples
#' ## Aggregate the source data for Germany
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#' ## Map the mean, standard deviation and number of measurements
#' plot_sources(GNIPDataDEagg)
#'
#' ## Same, writing the values instead (better for few source locations)
#' plot_sources(GNIPDataDEagg, labels = TRUE)
#'
#' ## Map only the number of measurements, with another palette
#' plot_sources(GNIPDataDEagg, which = "n", palette = list(fn = NULL))
#'
#' ## Map the means for the European data
#' plot_sources(GNIPDataEUagg, which = "mean")
#'
plot_sources <- function(data,
                         which = c("mean", "sd", "n"),
                         labels = FALSE,
                         points = list(pch = 21, cex = 1, lwd = 0.5, col = "black", digits = 1),
                         borders = list(borders = NA, lwd = 0.5, col = "darkgrey"),
                         palette = list(step = NA, range = c(NA, NA), n_labels = 11, digits = 2, fn = NA),
                         plot = TRUE) {
  ## Check the inputs
  .check_source_data(data, "plot_sources")
  which <- match.arg(which, several.ok = TRUE)
  if (anyDuplicated(data$source_ID) > 0) {
    stop(
      "The argument `data` of plot_sources() must contain a single row per source location, ",
      "but some locations appear several times (e.g. because the data were split by month or year in prepsources()). ",
      "Please map a subset of the data, e.g. `plot_sources(data[data$month == 1, ])`, or aggregate the data without splitting them.",
      call. = FALSE
    )
  }

  ## Complete input with default settings
  .complete_args(plot_sources)

  ## Import the palette and the country borders if missing (as for the other plots)
  if (!is.null(palette$fn) && !is.function(palette$fn) && is.na(palette$fn)) {
    isopalette1 <- NULL ## to please R CMD check
    utils::data("isopalette1", envir = environment(), package = "IsoriX")
    palette$fn <- grDevices::colorRampPalette(isopalette1, bias = 1)
  }
  if (!is.null(borders$borders) && !inherits(borders$borders, "SpatVector") && is.na(borders$borders)) {
    borders$borders <- terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX"))
  }

  ## Express longitudes continuously if the sources straddle the International Date Line (#180)
  data <- suppressMessages(.make_long_continuous(as.data.frame(data)))

  ## Define the area shown (the area covered by the sources, plus a margin)
  margin <- function(x) max(diff(range(x)) * 0.05, 1)
  xlim <- range(data$long) + c(-1, 1) * margin(data$long)
  ylim <- range(data$lat) + c(-1, 1) * margin(data$lat)
  borders$borders <- .wrap_vector_to_extent(borders$borders, terra::ext(c(xlim, ylim)))

  ## Compute the statistics
  stats <- list(
    mean = list(values = data$mean_source_value, title = "Mean"),
    sd = list(values = sqrt(data$var_source_value), title = "Standard deviation"),
    n = list(values = data$n_source_value, title = "Number of measurements")
  )[which]

  ## Build one map per statistic
  maps <- lapply(stats, function(stat) {
    .map_sources(
      long = data$long, lat = data$lat, values = stat$values, title = stat$title,
      labels = labels, points = points, borders = borders, palette = palette,
      xlim = xlim, ylim = ylim
    )
  })

  ## Draw the maps side by side
  if (plot) {
    for (i in seq_along(maps)) {
      print(maps[[i]], split = c(i, 1, length(maps), 1), more = i < length(maps))
    }
  }

  invisible(maps)
}


.map_sources <- function(long, lat, values, title, labels, points, borders, palette, xlim, ylim) {
  ## This function should not be called by the user.
  ## It builds the map of one statistic for plot_sources().
  keep <- !is.na(values)
  long <- long[keep]
  lat <- lat[keep]
  values <- values[keep]
  whole_values <- all(values %% 1 == 0) ## e.g. counts, shown without decimals

  ## colours (not used for labels) and colour key
  colours <- if (!labels) {
    .cut_and_color(
      var = values, step = palette$step, range = palette$range, palette = palette$fn,
      n_labels = palette$n_labels, digits = if (whole_values) 0 else palette$digits
    )
  }
  legend <- if (!labels) {
    list(right = list(
      fun = lattice::draw.colorkey,
      args = list(key = list(
        col = colours$all_cols, at = colours$at,
        labels = list(at = colours$at_keys, labels = colours$at_labels)
      ))
    ))
  }

  lattice::xyplot(lat ~ long,
    xlim = xlim, ylim = ylim, aspect = "iso",
    xlab = "Longitude", ylab = "Latitude", main = title,
    legend = legend,
    panel = function(x, y, ...) {
      if (!is.null(borders$borders)) {
        lattice::lpolygon(borders$borders, border = borders$col, lwd = borders$lwd)
      }
      if (labels) {
        lattice::ltext(x, y,
          labels = formatC(values, digits = if (whole_values) 0 else points$digits, format = "f"),
          cex = points$cex, col = points$col
        )
      } else {
        lattice::lpoints(x, y,
          pch = points$pch, cex = points$cex, lwd = points$lwd,
          col = points$col, fill = colours$cols
        )
      }
    }
  )
}
