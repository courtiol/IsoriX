# Changelog

## IsoriX (development version)

### Bug fixes

- [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)
  (and thus
  [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md))
  did not fix the residual variance of the residual dispersion model to
  its theoretical expectation (`phi = 2`) when
  `spaMM_method = list(disp_model = "HLfit")`, so this variance was
  estimated instead, contrary to what happens with the other fitting
  methods and to what the documentation states. Fits obtained with
  `"HLfit"` and `"fitme"` are now identical.
- printing (or summarising) isoscapes, fitted isoscapes, calibration
  fits and assignments no longer displays stray `NULL` lines
  ([\#220](https://github.com/courtiol/IsoriX/issues/220)).
- [`prepraster()`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
  failed to crop the raster according to the sources when given the
  output of
  [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
  since this type of object was not recognised.
- [`isomultiscape()`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
  did not check that the names of the layers of the raster used for
  weighting match those of the fits, since the check looked for an
  element that does not exist.
- [`prepraster()`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
  now respects `verbose = FALSE` (it used to always print the raster).
- the plotting functions for isoscapes and assignments no longer crash
  when the user provides their own country borders
  (`borders = list(borders = ...)`) containing more than one polygon.
- the test of spatial extrapolation performed by
  [`calibfit()`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
  no longer produces false positives: calibration sites located on the
  boundary of the convex hull formed by the source sites (e.g. sharing
  the location of a source site) are now considered to be inside,
  longitudes are handled consistently whether expressed within
  \[-180, 180) or \[0, 360) and hulls straddling the antimeridian (180
  degrees) are now handled correctly
  ([\#200](https://github.com/courtiol/IsoriX/issues/200)).
- [`?plot`](https://courtiol.github.io/IsoriX/reference/plots.md) now
  opens the documentation of the IsoriX plotting functions instead of
  the generic page listing re-exported functions
  ([\#193](https://github.com/courtiol/IsoriX/issues/193)).
- passing an empty list to a list argument
  (e.g. `isofit(data, mean_model_fix = list())`) no longer triggers an
  error; it now means that all defaults are kept
  ([\#206](https://github.com/courtiol/IsoriX/issues/206)).
- when plotting individual assignments, each map now only shows the
  location of the sample it corresponds to, instead of the locations of
  all samples ([\#103](https://github.com/courtiol/IsoriX/issues/103)).

### New features

- [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)
  and
  [`isomultiscape()`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
  can now fit the models and build the isoscapes of the different strata
  (e.g. months) in parallel, using the new argument `Ncpu` (by default
  set by the global option `Ncpu`, i.e. `2L` unless changed with
  [`options_IsoriX()`](https://courtiol.github.io/IsoriX/reference/options.md);
  use `Ncpu = 1` for sequential computations). Parallel computing relies
  on socket clusters from the base package **parallel** and thus works
  on Windows, macOS and Linux. The outputs are identical whether
  computed sequentially or in parallel, and what is printed, as well as
  messages and warnings, are still displayed. For the 12 monthly fits of
  `GNIPDataDE`, the fitting time dropped from about 33 s to 19 s using 4
  CPUs ([\#52](https://github.com/courtiol/IsoriX/issues/52)).
- new method [`AIC()`](https://rdrr.io/r/stats/AIC.html) for the outputs
  of [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)
  and
  [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
  which computes the conditional AIC of the fitted isoscapes corrected
  for the number of parameters estimated in the residual dispersion
  model (following Courtiol & Rousset 2017). This allows for the
  comparison of models differing in their residual dispersion model, and
  not only in their mean model. When several fits are given
  (e.g. `AIC(fit1, fit2)`), a table is returned which also details the
  number of parameters of each model and the uncorrected conditional AIC
  ([\#101](https://github.com/courtiol/IsoriX/issues/101)).
- new function
  [`plot_sources()`](https://courtiol.github.io/IsoriX/reference/plot_sources.md)
  mapping, for aggregated source data (as produced by
  [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)),
  the mean, standard deviation and number of measurements at each source
  location, using either coloured points or text labels; in interactive
  sessions, the user is asked to press before each new map
  ([\#195](https://github.com/courtiol/IsoriX/issues/195)).
- new function
  [`calibscape()`](https://courtiol.github.io/IsoriX/reference/calibscape.md)
  to build calibrated isoscapes, i.e. to convert an isoscape of source
  isotopic values (e.g. precipitation) into an isoscape of sample
  isotopic values (e.g. animal tissues) using a calibration fit. Only
  the mean isoscape is calibrated. The output, of class `CALIBSCAPE` (in
  addition to `ISOSCAPE`), can be plotted (`which = "mean"` only; by
  default, the plot shows the location of the calibration samples
  instead of that of the sources, see the new argument `calibs` of
  [`plot.ISOSCAPE()`](https://courtiol.github.io/IsoriX/reference/plots.md))
  but cannot be used by
  [`isofind()`](https://courtiol.github.io/IsoriX/reference/isofind.md)
  or
  [`create_aliens()`](https://courtiol.github.io/IsoriX/reference/create_aliens.md)
  ([\#191](https://github.com/courtiol/IsoriX/issues/191)).
- new function
  [`plot_dependencies()`](https://courtiol.github.io/IsoriX/reference/plot_dependencies.md)
  plotting the packages that IsoriX requires and suggests, and the
  dependencies among them, which can help with installation problems
  ([\#171](https://github.com/courtiol/IsoriX/issues/171)).
- [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
  no longer requires the columns `year` and `month` unless data are
  filtered or split by year or month, and the column `elev` is now
  optional ([\#192](https://github.com/courtiol/IsoriX/issues/192),
  [\#123](https://github.com/courtiol/IsoriX/issues/123)).
- IsoriX now handles areas crossing the International Date Line
  (antimeridian) out of the box, e.g. for studies centred on the Pacific
  ([\#180](https://github.com/courtiol/IsoriX/issues/180)):
  - [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
    can select sources across the International Date Line using
    `long_min > long_max` (e.g. `long_min = 70, long_max = -150`);
  - [`getelev()`](https://courtiol.github.io/IsoriX/reference/getelev.md)
    can download elevation across the International Date Line using
    `long_min > long_max`: since **elevatr** only handles longitudes
    within \[-180, 180\], the area is downloaded in two parts which are
    assembled into a single raster with continuous longitudes (e.g. from
    70 to 210);
  - [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)
    and
    [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)
    express longitudes within \[0, 360\] instead of \[-180, 180\] when
    the sources straddle the International Date Line, so that the area
    they cover is continuous (a message is displayed);
  - [`prepraster()`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
    crops structural rasters across the International Date Line, both
    manually (`manual_crop` with a first longitude greater than the
    second one, which no longer triggers a warning) and automatically
    from a fitted model;
  - [`isoscape()`](https://courtiol.github.io/IsoriX/reference/isoscape.md),
    [`calibfit()`](https://courtiol.github.io/IsoriX/reference/calibfit.md),
    [`isofind()`](https://courtiol.github.io/IsoriX/reference/isofind.md)
    and
    [`prepcipitate()`](https://courtiol.github.io/IsoriX/reference/prepcipitate.md)
    handle such rasters and express locations consistently (e.g. the
    ocean mask of
    [`isofind()`](https://courtiol.github.io/IsoriX/reference/isofind.md)
    now also applies beyond 180 degrees);
  - the plotting functions automatically shift country borders, masks
    and the locations of sources, calibration and assignment samples, so
    that they are drawn properly on maps crossing the International Date
    Line.

### Minor changes

- the R package is now stored at the root of the GitHub repository
  instead of in a sub-folder: the development version is thus installed
  with `remotes::install_github("courtiol/IsoriX")` (instead of
  `"courtiol/IsoriX/IsoriX"`), and tools requiring the package at the
  root of the repository, such as `rhub::rhub_setup()`, now work
  ([\#185](https://github.com/courtiol/IsoriX/issues/185)).
- [`isoscape()`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
  (and thus
  [`isomultiscape()`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md))
  is faster for large rasters: predictions are now computed in a single
  call to **spaMM** per model, letting **spaMM** slice the computation
  of the prediction variances into larger blocks, instead of calling
  **spaMM** on chunks of 1000 locations. The isoscapes produced are
  unchanged; for a raster of 600,000 cells, the computation time is
  divided by about 4. The progress bar is now the one from **spaMM**
  (shown when `verbose = TRUE`). The size of these blocks can be set
  with the new global option `spaMM_blocksize` (default `16000L`;
  e.g. `options_IsoriX(spaMM_blocksize = 4000L)` to reduce the memory
  used on very large rasters). The unused internal function
  `.futureisoscape()` has been removed
  ([\#150](https://github.com/courtiol/IsoriX/issues/150),
  [\#176](https://github.com/courtiol/IsoriX/issues/176)).
- IsoriX now requires **spaMM** \>= 4.4.0 (for the function `DoF()`).
- the main functions now check the type of the objects given to them and
  stop with an informative error message saying which function creates
  the expected object (e.g. when an isoscape is given instead of the
  output of
  [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)),
  or that source data must first be aggregated with
  [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
  ([\#164](https://github.com/courtiol/IsoriX/issues/164)).
- each reference cited in the documentation now includes a link to its
  DOI, and `citation("IsoriX")` now gives the pages and the DOI of the
  book chapter describing IsoriX.
- the titles of the help pages of all datasets follow a consistent
  pattern indicating their role in the workflow (e.g. “Calibration data:
  …”), and those of datasets from different studies are no longer
  identical ([\#97](https://github.com/courtiol/IsoriX/issues/97)).
- outdated links in the documentation were updated (e.g. the homepage of
  **spaMM** in
  [`?isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) and
  the link to the preprint in `citation("IsoriX")`), following the
  review of all links with the new script `release_checks/list_links.R`
  ([\#188](https://github.com/courtiol/IsoriX/issues/188)).
- the dataset `CountryBorders` contains again the names of the countries
  (column `name`), so that countries can be selected by name,
  e.g. `CountryBorders[CountryBorders$name == "Germany", ]`
  ([\#183](https://github.com/courtiol/IsoriX/issues/183)).
  `CountryBorders` and `OceanMask` have been recreated from the current
  version of the Natural Earth data (same resolution as before), which
  leads to minor changes in borders (e.g. Tuvalu is now included).
- the titles of isoscape plots no longer refer to hydrogen by default,
  since IsoriX cannot know which isotope is being studied: the default
  of the global option `title_delta_notation` is now `NULL`. Use
  e.g. `options_IsoriX(title_delta_notation = bquote(delta^2*H))` to
  restore the previous titles. The documentation
  ([`?options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md)
  and the bookdown) now also explains what to do when the delta symbol
  is not rendered by the graphic device
  ([\#155](https://github.com/courtiol/IsoriX/issues/155)).
- [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
  now stops with an informative message when the data have already been
  aggregated or when required columns are missing
  ([\#139](https://github.com/courtiol/IsoriX/issues/139)).
- [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)
  now stops with an informative message if elevation is used as a
  predictor but is absent from the data.
- [`prepraster()`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
  and
  [`isoscape()`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
  now stop with an informative message when the structural raster is no
  longer valid in the current R session (e.g. a raster read from a file
  and restored from a saved workspace), instead of the obscure error
  “NULL value passed as symbol address” produced by **terra**.
- the link to the IAEA WISER portal no longer points to an `.aspx` page
  ([\#186](https://github.com/courtiol/IsoriX/issues/186)).
- [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md)
  now displays a message, once per R session, when the default model
  (without any fixed-effect predictor) is fitted, to encourage users to
  compare alternative models; a new section of the online documentation
  explains how to do so
  ([\#204](https://github.com/courtiol/IsoriX/issues/204)).

### Internal (geeky) changes

- the tests now use the third edition of **testthat** and run in
  parallel (using 2 processes by default, or the number given by the
  environment variable `TESTTHAT_CPUS`), which makes them about twice as
  fast. During checks, each test process uses a single thread for linear
  algebra, since multithreaded libraries such as OpenBLAS otherwise make
  the processes compete for the cores (the README of the GitHub
  repository shows how to run `devtools::test()` this way).
- **tibble** is now a suggested package, so that the support of tibbles
  is tested using genuine tibbles.
- the documentation is now generated using **roxygen2** 8.1.1.
- GitHub issue templates have been added
  ([\#178](https://github.com/courtiol/IsoriX/issues/178)).

## IsoriX 0.9.4

CRAN release: 2026-01-13

### Main release goal

- the documentation had to be migrated to another website, so all links
  were updated.

### Minor changes

- bookdown.org is being discontinued so the bookdown has been migrated
  to “<https://courtiol-isorix.share.connect.posit.cloud>”.

### Internal (geeky) changes

- now explicitly depends on R (\>= 4.1.0) since we use pipes (`|>`).

## IsoriX 0.9.3

CRAN release: 2024-09-08

### New features

- a new global option `title_delta_notation` is available to modify the
  delta notation for all plotted isoscapes. The can be use, for example,
  to fix some issues with the display of the delta character, or for
  using another stable isotope than deuterium. See
  [`?options_IsoriX`](https://courtiol.github.io/IsoriX/reference/options.md)
  for details.
- new datasets `CalibDataBatRev`, `CalibDataBat2Rev`, `AssignDataRev`
  and `AssignData2Rev` which are revised version of
  CalibDataBat`,`CalibDataBat2`,`AssignData`and`AssignData2\`
  (respectively). The bat fur isotope values were corrected to align
  with the current delta values for keratin reference materials (Soto et
  al. 2017, <https://doi.org/10.1002/rcm.7893>) ensuring comparability
  between formerly and more recently normalized datasets of delta values
  for deuterium.

### Breaking changes

- the dataset `CalibDataBat2` now contains slightly different elevation
  values (the one reported by field workers as opposed to those
  extracted from a specific elevation map).
- the dataset `CalibDataBat` now contains different values for the
  column “site_ID”.

### Major changes

- the functions
  [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md),
  [`calibfit()`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
  and
  [`isofind()`](https://courtiol.github.io/IsoriX/reference/isofind.md)
  emit warnings if some locations share the same location IDs or if
  several locations IDs correspond to a unique location. This should
  help users to spot issues in their data (fixes
  [\#133](https://github.com/courtiol/IsoriX/issues/133)).

### Minor changes

- the functions
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  and
  [`lattice::lpolygon()`](https://rdrr.io/pkg/lattice/man/llines.html)
  now re-exported and `lattice::panel.points())` no longer is.

### Small fixes

- the URL for WorldClim data was outdated so
  [`getprecip()`](https://courtiol.github.io/IsoriX/reference/getprecip.md)
  was no longer working.
- some internal code had arguments misspelt. It was working as a result
  of R’s ability to do partial matching, but this was not good practice.
  It is now fixed (fixes
  [\#181](https://github.com/courtiol/IsoriX/issues/181)).
- some links in the documentation were incomplete and flagged is some
  online CRAN checks (“Found the following Rd file(s) with Rd targets
  missing package”). This should now be fixed.
- function names should now be consistently displayed as computer code
  in the documentation.

### Internal (geeky) changes

- the project is now being setup so that the development using VScode is
  possible.
- the format of the NEWS file has been changed to allow for using
  `usethis::use_github_release()` to create new releases (fixes
  [\#177](https://github.com/courtiol/IsoriX/issues/177)).
- the description of datasets now relies on the markdown syntax.

## IsoriX 0.9.2

CRAN release: 2023-11-14

### Main release goal

- This release restores some key features that had been lost after
  dropping the direct dependencies **raster** and **sp** in v0.9.1.:
  - the possibility to save and reload objects created by IsoriX (now
    via
    [`saveRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
    and
    [`readRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)).
    ([\#172](https://github.com/courtiol/IsoriX/issues/172))
  - the possibility to plot oceans and other masks containing “holes”
    (thanks to changes in **lattice** and **rasterVis**).
    ([\#169](https://github.com/courtiol/IsoriX/issues/169),
    [\#170](https://github.com/courtiol/IsoriX/issues/170))

### Breaking changes

As compared to IsoriX versions \< 0.9.1, the following changes may break
existing code:

- code for plots:

  ``` r

  layer(sp.polygons(CountryBorders, col = "white")) +
  layer(sp.polygons(OceanMask, col = "white", fill = "lightgreen"))
  ```

  now needs to be replaced by:

  ``` r

  layer(lpolygon(CountryBorders, border = "white")) +
  layer(lpolygon(OceanMask, border = "white", col = "lightgrey"))
  ```

  Notice both the change in the function used to plot polygons and the
  change in the arguments used to control the colour of the borders and
  the colour of the fill. Similarly, the function `sp.points()` should
  be replaced by `lpoints()` and so on.

- saving and reloading objects
  [`save()`](https://rdrr.io/r/base/save.html) &
  [`load()`](https://rdrr.io/r/base/load.html) can no longer be used,
  one must instead use
  [`saveRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  &
  [`readRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md).

### New features

- the NEWS (contained in this file) are now stored in `NEWS.md` rather
  than in `inst/NEWS.Rd` and use a markdown syntax.
- it is now possible to have missing values in predictors used to build
  isoscapes and in the isoscapes themselves.

### Major changes

- plotting methods for polygons, lines and points have been removed from
  IsoriX and are now handled by **lattice** and **rasterVis**.

- new S3 and S4 methods
  [`saveRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  for objects of the class `ISOSCAPE`, `CALIBFIT` & `ISOFIND` (see
  [`?serialize`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  for details).

- new S3 and S4 method
  [`readRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  which should be able to read objects created in IsoriX, as well as
  objects created with **terra** and objects created otherwise.

  In case of issues reading RDS files not created by IsoriX, try using
  [`base::readRDS()`](https://rdrr.io/r/base/readRDS.html) with the
  namespace `base::` mentioned explicitly and please let us know of this
  issue.

### Minor changes

- the bookdown is now listed in DESCRIPTION.
- some old URLs have been updated.

### Bug fixes

- the function
  [`isomultiscape()`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
  was still using **raster** instead of **terra**.
- the object `PrecipBrickDE` was still using **raster** instead of
  **terra**.

### Internal (geeky) changes

- new function `.safe_and_quiet_predictions()` which turns wraps around
  [`spaMM::predict.HLfit()`](https://rdrr.io/pkg/spaMM/man/predict.html),
  turns warnings into messages, allows not to display the same messages
  many times, and outputs `NA`s when
  [`spaMM::predict.HLfit()`](https://rdrr.io/pkg/spaMM/man/predict.html)
  fails. For testing, `options_IsoriX(spaMM_debug = TRUE)` may be used
  to restore the original behaviour of
  [`spaMM::predict.HLfit()`](https://rdrr.io/pkg/spaMM/man/predict.html).
- the classes `ISOSCAPE`, `CALIBFIT` & `ISOFIND` are now also defined as
  S4 classes, which was necessary to design methods for
  [`saveRDS()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  which are compatible with **terra**.
- the package now contains a `WORDLIST` file which is used by
  `devtools::spell_check()` (via
  [`spelling::spell_check_package`](https://docs.ropensci.org/spelling/reference/spell_check_package.html))
  to check for typos in the documentation.
- fixed various `|` or `&` which should have always been `||` or `&&`
  (spotted via `lintr::lint_package()`).
- fixed various sequences of the form `1:...` which should have always
  been handled by `seq_along` or `seq_len` to avoid NULL issues (spotted
  via `lintr::lint_package()`).
- package **withr** now suggested; we use it to automatically delete a
  file created during testing (using
  [`withr::defer`](https://withr.r-lib.org/reference/defer.html)).
- instead of one Rproj file used to handle both the package and the
  bookdown development, we now rely on 2 Rproj files, which solves some
  limitations encountered with **usethis**.
