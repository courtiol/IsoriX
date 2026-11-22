## Generate the IsoriX hex logo and the pkgdown favicons
##
## Run from the root of the package:
##   Rscript material_for_maintainer/logo/make_logo.R
##
## Outputs:
##   material_for_maintainer/logo/logo.svg        (vector logo, text as outlines)
##   material_for_maintainer/logo/logo_hires.png  (2072 x 2400)
##   man/figures/logo.png                         (480 x 556, used by README and pkgdown)
##   pkgdown/favicon/*                            (browser-tab icons for the website,
##                                                 from a simplified mark: isoscape + pin)
##
## Requires the packages isoband (filled contours), systemfonts (text converted
## to outlines, so the SVG renders without the font installed), rsvg (SVG to
## PNG) and magick (favicon.ico), plus the font "Inter" (SemiBold).
##
## Elements: an isoscape (filled contours of a 2D field coloured with
## isopalette1), a bat flying to the assigned origin (pin inside a closed
## contour), and a deuterium atom (1 proton + 1 neutron, 1 electron) labelled 2H.

out_dir <- "material_for_maintainer/logo"
stopifnot("run this script from the root of the IsoriX package" = dir.exists(out_dir))

## Settings -----------------------------------------------------------------

font_family <- "Inter"
font_weight <- "semibold"
name_size <- 104    # font size of "IsoriX"
name_track <- -1    # extra spacing between letters
pin_x <- 360        # tip of the pin (inside the closed contour)
pin_y <- 300

W <- 518; H <- 600  # standard hex sticker proportions
hex <- cbind(x = c(259, 518, 518, 259, 0, 0), y = c(0, 150, 450, 600, 450, 150))

load("data/isopalette1.rda") # IsoriX::isopalette1

## Helpers ------------------------------------------------------------------

f1 <- function(v) sprintf("%.1f", v)
fmt <- function(v) format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)

poly_d <- function(x, y, close = TRUE) {
  paste0("M", paste(f1(x), f1(y), sep = ",", collapse = " L"), if (close) "Z")
}
hex_d <- paste(poly_d(hex[, "x"], hex[, "y"], close = FALSE), "Z")

## Isoscape: a 2D field (trend + relief + noise) and its filled contours -----

isoscape_svg <- function(shift_x = 0, shift_y = 0, line_width = 1.4) {
  x <- seq(-20, W + 20, by = 2)
  y <- seq(-20, H + 20, by = 2)
  X <- matrix(x + shift_x, nrow = length(y), ncol = length(x), byrow = TRUE)
  Y <- matrix(y + shift_y, nrow = length(y), ncol = length(x))
  g <- function(cx, cy, sx, sy, a, th = 0) { # (rotated) Gaussian bump
    u <- (X - cx) * cos(th) + (Y - cy) * sin(th)
    v <- -(X - cx) * sin(th) + (Y - cy) * cos(th)
    a * exp(-0.5 * ((u / sx)^2 + (v / sy)^2))
  }
  F <- Y / 600                                                   # latitudinal trend
  F <- F + 0.035 * sin(X / 70 + 0.8) + 0.03 * sin(X / 37 + Y / 53 + 2) + 0.02 * sin(Y / 31 - X / 45)
  F <- F + g(375, 300, 100, 42, -0.21, -0.35)                    # "mountain range": depleted tongue
  F <- F + g(120, 345, 60, 45, -0.10)                            # second depleted patch
  F <- F + g(300, 175, 70, 30, 0.07, 0.4)
  F <- F + g(150, 520, 80, 40, 0.06)
  F <- F + g(70, 385, 95, 32, -0.09, 0.25)
  F <- F + g(470, 395, 60, 45, 0.07)
  F <- F + g(250, 345, 55, 30, 0.05, -0.2)
  F <- F + g(259, 560, 340, 115, 0.30)                           # darker area behind the name

  levels <- stats::quantile(F, seq(0, 1, length.out = 15), names = FALSE) # equal-area bands
  levels[1] <- levels[1] - 1e-6
  levels[length(levels)] <- levels[length(levels)] + 1e-6
  n_bands <- length(levels) - 1
  cols <- isopalette1[round(seq(0, length(isopalette1) - 1, length.out = n_bands)) + 1]

  bands <- isoband::isobands(x, y, F, levels[-length(levels)], levels[-1])
  bands_svg <- vapply(seq_along(bands), function(i) {
    b <- bands[[i]]
    if (length(b$x) == 0) return("")
    rings <- split(seq_along(b$x), b$id)
    d <- paste(vapply(rings, function(r) poly_d(b$x[r], b$y[r]), ""), collapse = "")
    sprintf('<path d="%s" fill="%s" fill-rule="evenodd"/>', d, cols[i])
  }, "")

  lines <- isoband::isolines(x, y, F, levels[2:n_bands])
  lines_d <- unlist(lapply(lines, function(l) {
    if (length(l$x) == 0) return(NULL)
    segs <- split(seq_along(l$x), l$id)
    vapply(segs, function(s) poly_d(l$x[s], l$y[s], close = FALSE), "")
  }))
  lines_svg <- sprintf(
    '<path d="%s" fill="none" stroke="#ffffff" stroke-opacity="0.38" stroke-width="%s" stroke-linejoin="round"/>',
    paste(lines_d, collapse = " "), fmt(line_width))

  paste(c(bands_svg[bands_svg != ""], lines_svg), collapse = "\n")
}

## Bat (right half drawn, left half mirrored) ---------------------------------

bat_d <- function() {
  ## each row: end point (x, y) and, for quadratic curves, the control point (cx, cy)
  seg <- data.frame(
    x  = c(0, 3.5, 7, 11, 48, 88, 77, 58, 38, 15, 6, 0),
    y  = c(-11, -22, -10, -2, -34, -26, 0, 8, 14, 13, 25, 28),
    cx = c(NA, NA, NA, 10, 30, 68, 78, 67, 47, 27, 11, 3),
    cy = c(NA, NA, NA, -5, -12, -32, -14, -6, 2, 6, 20, 27)
  )
  step <- function(cx, cy, x, y) {
    if (is.na(cx)) sprintf(" L%s,%s", fmt(x), fmt(y))
    else sprintf(" Q%s,%s %s,%s", fmt(cx), fmt(cy), fmt(x), fmt(y))
  }
  d <- sprintf("M%s,%s", fmt(seg$x[1]), fmt(seg$y[1]))
  for (k in 2:nrow(seg)) d <- paste0(d, step(seg$cx[k], seg$cy[k], seg$x[k], seg$y[k]))
  for (k in nrow(seg):2) d <- paste0(d, step(-seg$cx[k], seg$cy[k], -seg$x[k - 1], seg$y[k - 1]))
  paste(d, "Z")
}
bat_x <- 300; bat_y <- 100; bat_scale <- 1.1; bat_rot <- -4

## Pin, trajectory and atom -------------------------------------------------

pin_svg <- function(transform) {
  paste0(
    sprintf('<g transform="%s">', transform),
    '<ellipse cx="0" cy="2" rx="26" ry="8" fill="none" stroke="#ffffff" stroke-width="3" stroke-opacity="0.9"/>',
    '<path d="M0,0 C-6,-16 -24,-30 -24,-48 A24,24 0 1 1 24,-48 C24,-30 6,-16 0,0 Z" fill="#c8102e" stroke="#ffffff" stroke-width="4" stroke-linejoin="round"/>',
    '<circle cx="0" cy="-48" r="9" fill="#ffffff"/></g>')
}

trajectory_d <- sprintf("M%s,%s C%s,%s %s,%s %s,%s",
                        fmt(bat_x + 4), fmt(bat_y + 36), fmt(bat_x + 10), fmt(bat_y + 110),
                        fmt(pin_x - 70), fmt(pin_y - 130), fmt(pin_x - 4), fmt(pin_y - 86))

atom_x <- 122; atom_y <- 162; orbit <- 40
atom_svg <- paste0(
  sprintf('<g transform="translate(%s,%s)">', atom_x, atom_y),
  sprintf('<circle cx="0" cy="0" r="%s" fill="none" stroke="#ffffff" stroke-width="3.2"/>', orbit),
  '<circle cx="-10" cy="2" r="14" fill="#f4f1ec" stroke="#10212b" stroke-width="2"/>',  # neutron
  '<circle cx="10" cy="-2" r="14" fill="#c8102e" stroke="#ffffff" stroke-width="2.5"/>', # proton
  sprintf('<circle cx="%s" cy="%s" r="7" fill="#ffffff" stroke="#10212b" stroke-width="2"/>', # electron
          f1(orbit * cos(150 * pi / 180)), f1(orbit * sin(150 * pi / 180))),
  '</g>')

## Text converted to outlines -------------------------------------------------

font <- systemfonts::match_fonts(font_family, weight = font_weight)
if (!grepl(font_family, basename(font$path), ignore.case = TRUE)) {
  stop("font '", font_family, "' not found (systemfonts matched ", font$path, ")")
}

text_d <- function(txt, size, centre_x, baseline, track = 0) {
  ref <- 1000 # get metrics/outlines at a large size (avoids rounding), then scale
  k <- size / ref
  chars <- intToUtf8(utf8ToInt(txt), multiple = TRUE) # split by code point (works in any locale)
  info <- systemfonts::glyph_info(chars, path = font$path, index = font$index, size = ref)
  adv <- info$x_advance * k
  x0 <- centre_x - (sum(adv) + track * (length(adv) - 1)) / 2
  offsets <- x0 + c(0, cumsum(adv + track))[seq_along(adv)]
  out <- systemfonts::glyph_outline(info$index, font$path, font$index, size = ref, tolerance = 0.05)
  paste(vapply(seq_along(adv), function(i) {
    o <- out[out$glyph == i, ]
    if (nrow(o) == 0) return("")
    rings <- split(seq_len(nrow(o)), o$contour)
    paste(vapply(rings, function(r) poly_d(offsets[i] + o$x[r] * k, baseline - o$y[r] * k), ""),
          collapse = "")
  }, ""), collapse = "")
}

name_svg <- sprintf('<path d="%s" fill="#ffffff" filter="url(#sh)"/>',
                    text_d("IsoriX", name_size, 259, 482, name_track))
label_svg <- sprintf('<path d="%s" fill="#ffffff" filter="url(#sh)"/>',
                     text_d("²H", 38, atom_x + 1, atom_y + orbit + 44))

## Assemble ---------------------------------------------------------------------

defs <- function(shadow) {
  sprintf(paste0('<defs><clipPath id="hex"><path d="%s"/></clipPath>\n',
                 '<filter id="sh" x="-20%%" y="-20%%" width="140%%" height="140%%">',
                 '<feDropShadow dx="0" dy="%s" stdDeviation="%s" flood-color="#000" flood-opacity="0.35"/></filter></defs>'),
          hex_d, shadow, shadow)
}
border <- function(width) {
  sprintf('<path d="%s" fill="none" stroke="#0a2f45" stroke-width="%s" clip-path="url(#hex)"/>', hex_d, width)
}

logo_svg <- paste(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s">', W, H, W, H),
  defs(3),
  '<g clip-path="url(#hex)">',
  isoscape_svg(),
  sprintf('<path d="%s" fill="none" stroke="#ffffff" stroke-width="4" stroke-dasharray="2 10" stroke-linecap="round"/>', trajectory_d),
  sprintf('<g filter="url(#sh)">%s</g>', pin_svg(sprintf("translate(%s,%s)", pin_x, pin_y))),
  sprintf('<g filter="url(#sh)">%s</g>', atom_svg),
  sprintf('<g transform="translate(%s,%s) rotate(%s) scale(%s)" filter="url(#sh)"><path d="%s" fill="#10212b" stroke="#ffffff" stroke-width="2.4" stroke-linejoin="round"/></g>',
          bat_x, bat_y, bat_rot, bat_scale, bat_d()),
  label_svg,
  name_svg,
  '</g>',
  border(20),
  '</svg>', sep = "\n")

## simplified mark for favicons: square canvas, field shifted so that the closed
## contour sits in the centre, large pin, thicker lines and border
mark_svg <- paste(
  '<svg xmlns="http://www.w3.org/2000/svg" width="600" height="600" viewBox="-41 0 600 600">',
  defs(6),
  '<g clip-path="url(#hex)">',
  isoscape_svg(shift_x = 100, shift_y = -60, line_width = 3),
  sprintf('<g filter="url(#sh)">%s</g>', pin_svg("translate(259,382) scale(3.2)")),
  '</g>',
  border(48),
  '</svg>', sep = "\n")

## Write files -----------------------------------------------------------------

writeLines(logo_svg, file.path(out_dir, "logo.svg"))

logo_raw <- charToRaw(logo_svg)
mark_raw <- charToRaw(mark_svg)
rsvg::rsvg_png(logo_raw, file.path(out_dir, "logo_hires.png"), width = 2072, height = 2400)
rsvg::rsvg_png(logo_raw, "man/figures/logo.png", width = 480, height = 556)

fav_dir <- "pkgdown/favicon"
dir.create(fav_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(mark_svg, file.path(fav_dir, "favicon.svg"))
mark_png <- function(px) magick::image_read(rsvg::rsvg_png(mark_raw, width = px, height = px))
magick::image_write(mark_png(96), file.path(fav_dir, "favicon-96x96.png"))
magick::image_write(mark_png(192), file.path(fav_dir, "web-app-manifest-192x192.png"))
magick::image_write(mark_png(512), file.path(fav_dir, "web-app-manifest-512x512.png"))
## iOS fills transparent areas in black, so use a white background
apple <- magick::image_background(magick::image_extent(mark_png(160), "180x180", color = "white"), "white")
magick::image_write(magick::image_flatten(apple), file.path(fav_dir, "apple-touch-icon.png"))
magick::image_write(c(mark_png(16), mark_png(32), mark_png(48)), file.path(fav_dir, "favicon.ico"), format = "ico")

message("Logo and favicons written.")
