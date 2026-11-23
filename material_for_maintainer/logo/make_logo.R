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
## contour), a fish, and small atoms scattered over the landscape: many 1H and
## a few of the heavy stable isotope 2H (placed in the brown, i.e. enriched,
## areas); 16O and 18O can be switched on in n_atoms.

out_dir <- "material_for_maintainer/logo"
stopifnot("run this script from the root of the IsoriX package" = dir.exists(out_dir))

## Settings -----------------------------------------------------------------

font_family <- "Inter"
font_weight <- "semibold"
name_size <- 104    # font size of "IsoriX"
name_track <- -1    # extra spacing between letters
name_y <- 470       # baseline of "IsoriX"
name_fill <- "#ffffff"   # colour of "IsoriX"
name_stroke <- "#000000" # outline colour around the letters (NA: none)
name_stroke_width <- 7   # outline width (half of it shows outside the letters)
pin_near_x <- 360   # the bat's pin is put at the centre (lowest point) of the
pin_near_y <- 300   # closed contour found near this location
reverse_palette <- TRUE # TRUE: brown at the top, blue at the bottom
fish_x <- 66; fish_y <- 382; fish_scale <- 0.85; fish_rot <- 18
fish_pin_near_x <- 259 # same for the fish's pin (in the sea, below the name)
fish_pin_near_y <- 552
fish_pin_scale <- 0.85

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

iso_field <- function(shift_x = 0, shift_y = 0) {
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
  F <- F + g(259, 552, 55, 20, -0.30)                            # closed contour (sea) for the fish's origin
  F <- F + g(259, 560, 340, 115, 0.30)                           # darker area behind the name
  list(x = x, y = y, F = F) # F[i, j] is the value at (x[j], y[i])
}

## location of the lowest value of the field within radius r of (x0, y0):
## the centre of a closed contour (an "island" of low values)
island_centre <- function(field, x0, y0, r = 50) {
  d2 <- outer(field$y - y0, field$x - x0, function(dy, dx) dx^2 + dy^2)
  F <- field$F
  F[d2 > r^2] <- Inf
  k <- which.min(F)
  c(x = field$x[col(F)[k]], y = field$y[row(F)[k]])
}

iso_levels <- function(field) { # 14 equal-area bands
  levels <- stats::quantile(field$F, seq(0, 1, length.out = 15), names = FALSE)
  levels[1] <- levels[1] - 1e-6
  levels[length(levels)] <- levels[length(levels)] + 1e-6
  levels
}

isoscape_svg <- function(field, levels = iso_levels(field), line_width = 1.4) {
  x <- field$x; y <- field$y; F <- pmin(pmax(field$F, levels[1]), levels[length(levels)])
  n_bands <- length(levels) - 1
  cols <- isopalette1[round(seq(0, length(isopalette1) - 1, length.out = n_bands)) + 1]
  if (reverse_palette) cols <- rev(cols)

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

## Bat (right half drawn, left half mirrored; 2 scallops under each wing) -------

bat_d <- function() {
  ## each row: end point (x, y) and, for quadratic curves, the control point (cx, cy)
  seg <- data.frame(
    x  = c(0, 3.5, 7, 11, 48, 88, 71, 44, 15, 6, 0),
    y  = c(-11, -22, -10, -2, -34, -26, 2, 12, 13, 25, 28),
    cx = c(NA, NA, NA, 10, 30, 68, 79, 58, 29, 11, 3),
    cy = c(NA, NA, NA, -5, -12, -32, -13, -5, 0, 20, 27)
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

## Fish (facing right, centred on 0,0) ----------------------------------------

fish_svg <- paste0(
  sprintf('<g transform="translate(%s,%s) rotate(%s) scale(%s)" filter="url(#sh)">', fish_x, fish_y, fish_rot, fish_scale),
  '<path d="M44,0 C38,-14 24,-22 8,-23 Q0,-36 -12,-35 Q-12,-26 -16,-19 C-24,-14 -30,-9 -36,-5 ',
  'L-54,-22 Q-48,0 -54,22 L-36,5 C-28,12 -16,18 -2,21 Q-6,28 -14,31 Q2,30 8,22 C24,21 38,14 44,0 Z" ',
  'fill="#10212b" stroke="#ffffff" stroke-width="2.4" stroke-linejoin="round"/>',
  '<path d="M17,-12 Q11,0 17,12" fill="none" stroke="#ffffff" stroke-width="2" stroke-linecap="round" stroke-opacity="0.8"/>', # gill
  '<circle cx="29" cy="-5" r="3.6" fill="#ffffff"/>',                                                                          # eye
  '</g>')

## Pin, trajectory and atom -------------------------------------------------

pin_svg <- function(transform) {
  paste0(
    sprintf('<g transform="%s">', transform),
    '<ellipse cx="0" cy="2" rx="26" ry="8" fill="none" stroke="#ffffff" stroke-width="3" stroke-opacity="0.9"/>',
    '<path d="M0,0 C-6,-16 -24,-30 -24,-48 A24,24 0 1 1 24,-48 C24,-30 6,-16 0,0 Z" fill="#c8102e" stroke="#ffffff" stroke-width="4" stroke-linejoin="round"/>',
    '<circle cx="0" cy="-48" r="9" fill="#ffffff"/></g>')
}

field <- iso_field()
pin_xy <- island_centre(field, pin_near_x, pin_near_y)
pin_x <- pin_xy[["x"]]; pin_y <- pin_xy[["y"]]
fish_pin_xy <- island_centre(field, fish_pin_near_x, fish_pin_near_y)
fish_pin_x <- fish_pin_xy[["x"]]; fish_pin_y <- fish_pin_xy[["y"]]

## dotted paths: one gentle curve from below the animal to the left of its pin's head
path_d <- function(start_x, start_y, pin_x, pin_y, pin_scale = 1) {
  end_x <- pin_x - 28 * pin_scale
  end_y <- pin_y - 40 * pin_scale
  sprintf("M%s,%s Q%s,%s %s,%s", f1(start_x), f1(start_y), f1(start_x + 14), f1(end_y + 4), f1(end_x), f1(end_y))
}
path_pts <- function(start_x, start_y, pin_x, pin_y, pin_scale = 1, n = 40) { # points along path_d()
  end_x <- pin_x - 28 * pin_scale; end_y <- pin_y - 40 * pin_scale
  cx <- start_x + 14; cy <- end_y + 4
  t <- seq(0, 1, length.out = n)
  cbind(x = (1 - t)^2 * start_x + 2 * (1 - t) * t * cx + t^2 * end_x,
        y = (1 - t)^2 * start_y + 2 * (1 - t) * t * cy + t^2 * end_y)
}
trajectory_d <- path_d(bat_x + 4, bat_y + 36, pin_x, pin_y)

## from below the fish, down the left of the name, to its pin
rot <- fish_rot * pi / 180
start_x <- fish_x + fish_scale * (-2 * cos(rot) - 36 * sin(rot)) # just below the belly
start_y <- fish_y + fish_scale * (-2 * sin(rot) + 36 * cos(rot))
fish_trajectory_d <- path_d(start_x, start_y, fish_pin_x, fish_pin_y, fish_pin_scale)

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

name_svg <- sprintf('<path d="%s" fill="%s"%s filter="url(#sh)"/>',
                    text_d("IsoriX", name_size, 259, name_y, name_track), name_fill,
                    if (is.na(name_stroke)) "" else sprintf(' stroke="%s" stroke-width="%s" stroke-linejoin="round" paint-order="stroke"', name_stroke, name_stroke_width))

## Isotopes scattered over the landscape ---------------------------------------
## light (1H, 16O) and heavy (2H, 18O) isotopes; as in IsoriX, blue means negative
## (depleted) and brown positive (enriched) delta values, so the share of heavy
## isotopes increases from the blue to the brown areas

n_atoms <- c(H1 = 13, H2 = 6, O16 = 0, O18 = 0) # oxygen switched off (set e.g. O16 = 6, O18 = 2)
heavy_weight <- function(e) e^2 # odds of a heavy isotope vs enrichment e (0 = bluest, 1 = brownest)
atoms_seed <- 4     # change to get another random arrangement
nucleon_r <- 3.6; electron_r <- 2.4; orbit_lwd <- 1.3

isotopes <- list( # protons, neutrons, orbits (radius, number of electrons)
  H1  = list(p = 1, n = 0,  orbits = list(c(12, 1))),
  H2  = list(p = 1, n = 1,  orbits = list(c(12, 1))),
  O16 = list(p = 8, n = 8,  orbits = list(c(17, 2), c(24.5, 6))),
  O18 = list(p = 8, n = 10, orbits = list(c(17, 2), c(24.5, 6))))

nucleon <- function(x, y, proton) {
  if (proton) sprintf('<circle cx="%s" cy="%s" r="%s" fill="#c8102e" stroke="#ffffff" stroke-width="0.7"/>', f1(x), f1(y), nucleon_r)
  else sprintf('<circle cx="%s" cy="%s" r="%s" fill="#f4f1ec" stroke="#10212b" stroke-width="0.6"/>', f1(x), f1(y), nucleon_r)
}
nucleus_svg <- function(p, n) {
  k <- p + n
  if (k == 1) return(nucleon(0, 0, TRUE))
  if (k == 2) return(paste0(nucleon(-0.82 * nucleon_r, 0.5, FALSE), nucleon(0.82 * nucleon_r, -0.5, TRUE)))
  i <- 0:(k - 1) # larger nuclei: nucleons packed on a sunflower spiral
  r <- 0.66 * nucleon_r * sqrt(i + 0.5)
  th <- i * 137.508 * pi / 180
  is_proton <- seq_len(k) %in% round(seq(1, k, length.out = p))
  paste0(vapply(rev(seq_len(k)), function(j) nucleon(r[j] * cos(th[j]), r[j] * sin(th[j]), is_proton[j]), ""),
         collapse = "") # outer nucleons first, so the inner ones are drawn on top
}
atom_svg <- function(iso, x, y, angle) {
  orbits <- vapply(iso$orbits, function(o) {
    a <- (angle + seq(0, 360, length.out = o[2] + 1)[-(o[2] + 1)]) * pi / 180
    paste0(sprintf('<circle cx="0" cy="0" r="%s" fill="none" stroke="#ffffff" stroke-width="%s"/>', o[1], orbit_lwd),
           paste0(sprintf('<circle cx="%s" cy="%s" r="%s" fill="#ffffff" stroke="#10212b" stroke-width="0.7"/>',
                          f1(o[1] * cos(a)), f1(o[1] * sin(a)), electron_r), collapse = ""))
  }, "")
  sprintf('<g transform="translate(%s,%s)" filter="url(#sh_small)">%s%s</g>',
          f1(x), f1(y), paste0(orbits, collapse = ""), nucleus_svg(iso$p, iso$n))
}

## random placement, avoiding the other elements of the logo: all the atoms of
## an element are placed first, then the heavy isotope is given to some of them,
## drawn with a probability increasing with the local enrichment
atoms_svg <- local({
  set.seed(atoms_seed)
  inside_hex <- function(x, y, margin) { # regular hexagon centred on (259, 300)
    a <- c(0, 60, 120, 180, 240, 300) * pi / 180
    all((x - 259) * cos(a) + (y - 300) * sin(a) <= 300 * cos(pi / 6) - margin)
  }
  boxes <- rbind( # x0, y0, x1, y1
    bat      = c(bat_x - 100, bat_y - 45, bat_x + 100, bat_y + 40),
    bat_pin  = c(pin_x - 30, pin_y - 80, pin_x + 30, pin_y + 12),
    fish     = c(fish_x - 55, fish_y - 35, fish_x + 50, fish_y + 40),
    fish_pin = c(fish_pin_x - 26, fish_pin_y - 68, fish_pin_x + 26, fish_pin_y + 10),
    name     = c(110, name_y - 82, 410, name_y + 6))
  path <- rbind(path_pts(bat_x + 4, bat_y + 36, pin_x, pin_y),
                path_pts(start_x, start_y, fish_pin_x, fish_pin_y, fish_pin_scale))
  levels <- iso_levels(field)
  n_bands <- length(levels) - 1
  enrichment_at <- function(x, y) { # 0 for the bluest band, 1 for the brownest one
    band <- findInterval(field$F[which.min(abs(field$y - y)), which.min(abs(field$x - x))], levels, all.inside = TRUE)
    if (reverse_palette) (n_bands - band) / (n_bands - 1) else (band - 1) / (n_bands - 1)
  }
  placed <- data.frame(x = numeric(), y = numeric(), r = numeric())
  out <- character()
  for (el in list(c("O16", "O18"), c("H1", "H2"))) { # larger atoms first
    n <- n_atoms[[el[1]]] + n_atoms[[el[2]]]
    if (n == 0) next
    r <- max(vapply(isotopes[el], function(iso) iso$orbits[[length(iso$orbits)]][1], 0)) + electron_r
    xy <- matrix(NA_real_, n, 2)
    for (k in seq_len(n)) {
      for (try in 1:5000) {
        x <- stats::runif(1, 0, W); y <- stats::runif(1, 0, H)
        ok <- inside_hex(x, y, r + 16) &&
          !any(x > boxes[, 1] - r - 6 & x < boxes[, 3] + r + 6 & y > boxes[, 2] - r - 6 & y < boxes[, 4] + r + 6) &&
          all((path[, "x"] - x)^2 + (path[, "y"] - y)^2 > (r + 8)^2) &&
          all((placed$x - x)^2 + (placed$y - y)^2 > (placed$r + r + 30)^2)
        if (ok) break
      }
      if (!ok) stop("could not place all the atoms; reduce n_atoms or change atoms_seed")
      placed[nrow(placed) + 1, ] <- c(x, y, r)
      xy[k, ] <- c(x, y)
    }
    e <- apply(xy, 1, function(p) enrichment_at(p[1], p[2]))
    heavy <- seq_len(n) %in% sample.int(n, n_atoms[[el[2]]], prob = heavy_weight(e) + 1e-9)
    zone <- cut(e, c(-Inf, 1/3, 2/3, Inf), labels = c("blue/depleted", "middle", "brown/enriched"))
    tab <- table(zone, ifelse(heavy, el[2], el[1]))[, el[el %in% names(n_atoms)[n_atoms > 0]], drop = FALSE]
    message(el[2], " among ", paste(el, collapse = " + "), " atoms, by zone:")
    print(tab)
    out <- c(out, vapply(seq_len(n), function(k) {
      atom_svg(isotopes[[el[1 + heavy[k]]]], xy[k, 1], xy[k, 2], stats::runif(1, 0, 360))
    }, ""))
  }
  paste(out, collapse = "\n")
})

## Assemble ---------------------------------------------------------------------

defs <- function(shadow) {
  sprintf(paste0('<defs><clipPath id="hex"><path d="%s"/></clipPath>\n',
                 '<filter id="sh" x="-20%%" y="-20%%" width="140%%" height="140%%">',
                 '<feDropShadow dx="0" dy="%s" stdDeviation="%s" flood-color="#000" flood-opacity="0.35"/></filter>\n',
                 '<filter id="sh_small" x="-30%%" y="-30%%" width="160%%" height="160%%">',
                 '<feDropShadow dx="0" dy="1" stdDeviation="1" flood-color="#000" flood-opacity="0.3"/></filter></defs>'),
          hex_d, shadow, shadow)
}
border <- function(width) {
  sprintf('<path d="%s" fill="none" stroke="#0a2f45" stroke-width="%s" clip-path="url(#hex)"/>', hex_d, width)
}

logo_svg <- paste(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s">', W, H, W, H),
  defs(3),
  '<g clip-path="url(#hex)">',
  isoscape_svg(field),
  atoms_svg,
  sprintf('<path d="%s" fill="none" stroke="#ffffff" stroke-width="4" stroke-dasharray="2 10" stroke-linecap="round"/>', trajectory_d),
  sprintf('<path d="%s" fill="none" stroke="#ffffff" stroke-width="4" stroke-dasharray="2 10" stroke-linecap="round"/>', fish_trajectory_d),
  sprintf('<g filter="url(#sh)">%s</g>', pin_svg(sprintf("translate(%s,%s)", pin_x, pin_y))),
  sprintf('<g filter="url(#sh)">%s</g>', pin_svg(sprintf("translate(%s,%s) scale(%s)", fish_pin_x, fish_pin_y, fish_pin_scale))),
  fish_svg,
  sprintf('<g transform="translate(%s,%s) rotate(%s) scale(%s)" filter="url(#sh)"><path d="%s" fill="#10212b" stroke="#ffffff" stroke-width="2.4" stroke-linejoin="round"/></g>',
          bat_x, bat_y, bat_rot, bat_scale, bat_d()),
  name_svg,
  '</g>',
  border(20),
  '</svg>', sep = "\n")

## simplified mark for favicons: square canvas, field shifted so that the bat's
## closed contour sits in the centre, large pin, thicker lines and border
mark_centre <- c(x = 259, y = 375) # where the bat's island lands in the mark
mark_field <- iso_field(shift_x = pin_x - mark_centre[["x"]], shift_y = pin_y - mark_centre[["y"]])
mark_pin <- island_centre(mark_field, mark_centre[["x"]], mark_centre[["y"]], r = 10)
mark_svg <- paste(
  '<svg xmlns="http://www.w3.org/2000/svg" width="600" height="600" viewBox="-41 0 600 600">',
  defs(6),
  '<g clip-path="url(#hex)">',
  isoscape_svg(mark_field, levels = iso_levels(field), line_width = 3), # same colours as the logo
  sprintf('<g filter="url(#sh)">%s</g>', pin_svg(sprintf("translate(%s,%s) scale(3.2)", mark_pin[["x"]], mark_pin[["y"]]))),
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
