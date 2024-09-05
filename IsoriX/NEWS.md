# IsoriX (development version)

## New features

  * new datasets `CalibDataBatRev`, `CalibDataBat2Rev`, `AssignDataRev` and `AssignData2Rev` which are revised version of CalibDataBat`, `CalibDataBat2`, `AssignData` and `AssignData2` (respectively). The bat fur isotope values were corrected to align with the current delta values for keratin reference materials (Soto et al. 2017, https://doi.org/10.1002/rcm.7893) ensuring comparability between formerly and more recently normalized datasets of delta values for deuterium.

## Breaking changes

  * the dataset `CalibDataBat2` now contains slightly different elevation values (the one reported by field workers as opposed to those extracted from a specific elevation map).
  * the dataset `CalibDataBat` now contains different values for the column "site_ID".

## Major changes

  * the functions `prepsources()`, `calibfit()` and `isofind()` emit warnings if some locations share the same location IDs or if several locations IDs correspond to a unique location. This should help users to spot issues in their data (fixes #133).

## Minor changes

  * the function `terra::extract()` is now re-exported.

## Small fixes
  
  * the URL for WorldClim data was outdated so `getprecip()` was no longer working.
  * some internal code had arguments misspelt. It was working as a result of R's ability to do partial matching, but this was not good practice. It is now fixed (fixes #181).
  * some links in the documentation were incomplete and flagged is some online CRAN checks ("Found the following Rd file(s) with Rd \link{} targets missing package"). This should now be fixed.
  * function names should now be consistently displayed as computer code in the documentation.

## Internal (geeky) changes

  * the project is now being setup so that the development using VScode is possible.
  * the format of the NEWS file has been changed to allow for using `usethis::use_github_release()` to create new releases (fixes #177).
  * the description of datasets now relies on the markdown syntax.


# IsoriX 0.9.2

## Main release goal

  * This release restores some key features that had been lost after dropping the direct dependencies **raster** and **sp** in v0.9.1.:
     * the possibility to save and reload objects created by IsoriX (now via `saveRDS()` and `readRDS()`). (#172)
     * the possibility to plot oceans and other masks containing "holes" (thanks to changes in **lattice** and **rasterVis**). (#169, #170)

## Breaking changes
  
  As compared to IsoriX versions < 0.9.1, the following changes may break existing code:
  
  * code for plots:
    
    ```r
    layer(sp.polygons(CountryBorders, col = "white")) +
    layer(sp.polygons(OceanMask, col = "white", fill = "lightgreen"))
    ```
    
    now needs to be replaced by:
    
    ```r
    layer(lpolygon(CountryBorders, border = "white")) +
    layer(lpolygon(OceanMask, border = "white", col = "lightgrey"))
    ```
    
    Notice both the change in the function used to plot polygons and the change in the arguments used to control the colour of the borders and the colour of the fill.
    Similarly, the function `sp.points()` should be replaced by `lpoints()` and so on.
    
  * saving and reloading objects `save()` & `load()` can no longer be used, one must instead use `saveRDS()` & `readRDS()`.

## New features

  * the NEWS (contained in this file) are now stored in `NEWS.md` rather than in `inst/NEWS.Rd` and use a markdown syntax.
  * it is now possible to have missing values in predictors used to build isoscapes and in the isoscapes themselves.

## Major changes

  * plotting methods for polygons, lines and points have been removed from IsoriX and are now handled by **lattice** and **rasterVis**.
  * new S3 and S4 methods `saveRDS()` for objects of the class `ISOSCAPE`, `CALIBFIT` & `ISOFIND` (see `?serialize` for details).
  * new S3 and S4 method `readRDS()` which should be able to read objects created in IsoriX, as well as objects created with **terra** and objects created otherwise.
  
    In case of issues reading RDS files not created by IsoriX, try using `base::readRDS()` with the namespace `base::` mentioned explicitly and please let us know of this issue.

## Minor changes

  * the bookdown is now listed in DESCRIPTION.
  * some old URLs have been updated.

## Bug fixes
  
  * the function `isomultiscape()` was still using **raster** instead of **terra**.
  * the object `PrecipBrickDE` was still using **raster** instead of **terra**.

## Internal (geeky) changes

  * new function `.safe_and_quiet_predictions()` which turns wraps around `spaMM::predict.HLfit()`, turns warnings into messages, allows not to display the same messages many times, and outputs `NA`s when `spaMM::predict.HLfit()` fails. For testing, `options_IsoriX(spaMM_debug = TRUE)` may be used to restore the original behaviour of `spaMM::predict.HLfit()`.
  * the classes `ISOSCAPE`, `CALIBFIT` & `ISOFIND` are now also defined as S4 classes, which was necessary to design methods for `saveRDS()` which are compatible with **terra**.
  * the package now contains a `WORDLIST` file which is used by `devtools::spell_check()` (via `spelling::spell_check_package`) to check for typos in the documentation.
  * fixed various `|` or `&` which should have always been `||` or `&&` (spotted via `lintr::lint_package()`).
  * fixed various sequences of the form `1:...` which should have always been handled by `seq_along` or `seq_len` to avoid NULL issues (spotted via `lintr::lint_package()`).
  * package **withr** now suggested; we use it to automatically delete a file created during testing (using `withr::defer`).
  * instead of one Rproj file used to handle both the package and the bookdown development, we now rely on 2 Rproj files, which solves some limitations encountered with **usethis**.


# v0.9.1

## Main release goal

  * Several spatial packages previously used by IsoriX are likely to retire sometimes in October 2023.
  The maintainers of those packages have recommended developers to instead rely on alternative packages which have been recently developed and which superseed the old packages. As a consequence, we had to recode a lot of IsoriX for it to continue to work. For the most part, these changes are internal and should not impact much users, but it is possible that old workflows used to create plots will have to be adapted for the code to keep working. Moreover, IsoriX is not the only package that had to be overhauled, other packages used by IsoriX are also being adapted, which means that the programming landscape is dynamic and bugs caused by incompatibility between packages are likely to surface. We will do our best to react quickly, but please let us know as soon as something goes wrong by dropping issues on the GitHub repository for IsoriX (https://github.com/courtiol/IsoriX/issues). All this change can be perceived as annoying, but it is also for the best: it will allow us to add new features more easily in IsoriX in the future and it also makes it easier for users to convert IsoriX outputs so as to manipulate them using packages such as **sf** and **ggplot2**.

## Major changes
    
  * IsoriX no longer relies on the package **raster**. It instead now uses **terra** for handling rasters. (#90 #161)
  * IsoriX no longer relies on the package **sp**. Plotting functionalities from sp have now been replaced by direct calls to **lattice**. For now, we had to implement methods and generics calling **lattice** in IsoriX, but those should ultimately be handled within **rasterVis** and **lattice**.

## Minor changes
    
  * `getprecip()` now normalizes the input file and returns the path where the precipitation rasters are stored.
  * `prepcipitate()` can now handle as input for `path =` either the full path to the files returned by `getprecip()` -- which contains the folder provided to `path` when calling `getprecip()` in addition to `"/wc2.1_30s_prec"` -- or the reduced path which only contains the folder provided to `path` when calling `getprecip()`.
  * `getprecip()` now changes the timeout R options temporarily so as to avoid the download to fail because the default timeout setting is too short. (#148)
  * the documentation for the datasets `GNIPDataALLagg` and `GNIPDataEUagg` was incorrect. (#158)
  * one message about possible extrapolation during calibration was erroneous and is now removed. (#159)

## Internal (geeky) changes
    
  * `OceanMask` and `CountryBorders` are no longer stored as RDA files in `/data`, but as RDS files in `/extata` since objects created with **terra** cannot be saved as RDA files. These files are automatically loaded when the package is attached.
  * **elevatr** moved from Imports to Suggests. (#157)


# v0.9.0

## Bug fixes
    
  * the previous released introduced an error in how the variance of the assignment test is computed in the absence of calibration (with important consequence in terms of assignments). This is now fixed. (#151)

## Minor changes

  * the base package **colourspace** is now suggested to avoid a note in R CMD check.
    

# v0.8.3

## New features
    
  * the function `calibfit()` gains an argument method that allows for selecting one of four calibration methods ("wild", "lab", "desk", "desk_inverse"). This allows for users to use:
    1) calibration samples associated with unknown environmental isotopic values,
    2) calibration samples associated with known environmental isotopic values, or
    3) & 4) the intercept and slope of a calibration relationship computed by others (e.g. values found in a paper).

    Note: the "desk" methods allow for the consideration of a fractionation factor too (i.e. slope = 0). See `?calibfit` for details. (#20 & #142)

  * the function `getelev()` has been completely rewritten so as to rely on the package **elevatr** to download elevation data. You should check `?getelev` for learning how to use the new version of the function, but we retained the core principle of the previous function so that old workflow will only require minor adjustments. The new version still saves a `*.tif` file on the disk, albeit using a different file name to avoid (data) confusion. (#140 & #107)
  * the function `isofind()` gains an argument `neglect_covPredCalib` that allows for the computation of a covariance term that was so far neglected in IsoriX. See `?isofind` for details. (#143)
  * the function `prepraster()` gains an argument `values_to_zero` to turn a range of elevation values to zeros (nullify negative elevation values by default). This is particular useful because the new version of `get_elev()` download an elevation raster that includes bathymetry.
  * new internal function `.invert_reg()` to invert regression (used for method "desk_inverse" in `calibfit()`.
  
## Minor changes
    
  * when calling `plot()` on an object created with `calibfit()`, the plotting function now returns the fitted values and CI for users to be able to make alternative plots. (#44)
  * new argument `xlim` for the plotting function for calibration fits.
  * new argument `line` for customizing how to plot the regression line in calibration fits.
  * the summary method for calibration fits now displays the residual variance.
  * `calibfit()` performs more check on extrapolation. (#119)
  * when using `plot()` on an object of class ISOFIT, the x-axis for the plot showing the Matérn correlation should have a range more adequate irrespective when autocorrelation is strong over short distances. (#134)
  * documentation for `?plot()` now contains a description of what symbols mean in plots. (#138)
  * when calling `plot()` on an object created with `isofind()`, the plotting function now detects sample of size 1 and no longer displays "Group" in the title of the assignment plot even if `who` = "group". (#120)
  * all functions accepting a `data.frame` as input should also now be compatible when provided with a `tibble`. (#118)
  * typos have been corrected. (#130)
  * default y-axis title changed to "Isotopic value in the environment" when plotting calibration fits to be flexible enough irrespective of the methods used in `calibfit()`
  
## Internal (geeky) changes
    
  * the argument `long_min`, `long_max`, `lat_min` & `lat_max` function `prepsources()` now have explicit default values and should no longer be missing.
  * the version of **spaMM** required by IsoriX has changed to 3.13 so as to benefit from a new extractor we rely on for the computation of the 4th variance term during assignment. (#143)
  * the function depending on the package **RandomFields** are no longer available since that package has been (for now) retired by CRAN :-(
  * IsoriX should now work with tibbles as inputs. (#118)
    
## Bug fixes

  * the printing method for the object of class ISOSCAPE was somehow not exported and thus not used (unreported issue).
  * plotting on a sphere ISOFIND objects did not work in some cases. (#126)


# v0.8.2

## New features
    
  * new argument `ylim` for the plotting function for calibration fits.
  * it is now possible to calibrate data containing missing isotopic values.
  * it is now possible to assign data containing missing isotopic values.
      
## Internal (geeky) changes
    
  * the SpatialPolygons `CountryBorders` and `OceanMask` have been rebuilt for possibly improving the compatibility with new **sp** & **rgdal**.
  * the website for `WorlClim` has now changed address, so links have been updated.
  * **rgdal** is now listed as a suggested package.
      
## Minor changes
    
  * several URL had changed and have been updated.
  * all old defunct functions have been removed from the package.


# v0.8.1

## Bug fixes
    
  * the plotting function was not working for isoscapes not stored in memory due to a wrong use of the quantile function. Many thanks to Dr. Gary Roemer and Amy Withers for reporting it! (#113)

## New features
    
  * the datasets used in Courtiol et al. 2019 are now provided.
  * many useful functions from **raster**, **rasterVis**, **lattice**... are now re-exported so they can be used without attaching those packages.
  * new option in plots that allows to map the isoscape onto a sphere.
  * a new dataset `PrecipBrickDE` containing monthly precipitation amounts for Germany.
  * an argument `y_title` for the plotting function for isoscapes to allow one to simply change the title.
  * arguments `xlab` and `ylab` for the plotting function for calibration fits.
  * new method points for plotting more than one calibration fit.
  * the plotting function for assignments can now show the location of the assignment samples.

## Major changes
    
  * the citations for the package have been updated!
  * many objects have been renamed to prepare the release of the version 1.0.
  * the vignettes have now been moved to a bookdown. To access the documentation you should now visit: https://bookdown.org/content/782/

## Minor changes
    
  * all arguments with the structure `bla.bla` have been renamed so as to match the structure `bla_bla`.
  * the plotting function for `calibfit()` gains an argument `...` for more control.
  * a plotting method for `rasterLayer` has been included for convenience.
  * the function `relevate()` is now called `prepraster()`.
  * the function `prepdata()` is now called `prepsources()`.
  * in several functions the argument `elevation.raster` has been renamed as `raster`.
  * in several functions the argument `xxx.data` has been renamed as `data`.
    
## Internal (geeky) changes
    
  * the file storing the internal functions is now called `zzz.R`.
  * the `dontrun` and `donttest` calls have been replaced by comments due to new R CMD check flags.
  * the function `downloadfile()` is now exported.
  * large temporary objects are now deleted within isofind to limit memory usage.
  * the package is now being tested using **testthat**, but tests will be implemented in the future.
  * a lot of the internal code as been rewritten to comply more closely to the IsoriX coding style.
  * the list of suggested packages has been revised and **rgdal** removed as it caused (again) problems with Travis CI.
  * following a change in **spaMM** `predict.HLfit()`, the prediction are now being made by chunk of 1000 points instead of 150. This should lead to a tiny gain in performance.
  * the function `isoscape()` was performing predictions twice every 150 (or now 1000) locations, this was not influencing the isoscapes produced, but this has now been corrected.
  * the function `prepraster()` now produces an raster stored in memory if it is possible. This should prevent bugs that appears when using loaded rasters that were previously saved (the temporary link to the hard drive location is no longer correct in this case).
  * the function `.objective_fn_calib()` has been moved within the function `calibfit()` as it is not used elsewhere.
  * the function `calibfit()` as been prepared for a possible activation of a random effect for species ID in the future. But whether it would make sense or not remains to be determined.
  * the function `.Fisher_method()` now directly computes the exponential of the log pv if only one value is provided. This leads to much faster assignment in the case of a single observation.
  
## Bug fixes
    
  * the plotting function for calibration fit was displaying CI based on variance instead of SD.
  * the function `getprecip()` and `prepcipitate()` were not handling paths manually defined properly.
  * the plotting functions were crashing in case of no variation in the landscape.
  * the plotting functions were crashing when called on multiple-raster objects not stored 'inMemory'.
  * the plotting function for fitted model was not displaying one plot in RStudio when called on objects of class `MULTIISOFIT`.


# v0.7.1

## New features
    
  * this is a minor update necessary to maintain compatibility with **spaMM** 2.4.
    
  
## Internal (geeky) changes
    
  * the syntax for the extraction of correlation terms of **spaMM** objects has changed.


# v0.7

## New features
    
  * the calibration step is now optional, allowing for users to use an isoscape directly fitted on tissues instead of precipitation water.
  * the function `queryGNIP()` has been renamed and is now called `prepdata()`, this function can also handle other datasets than GNIP.
  * the function `relevate()` has been modified to make crop possible around the pacific meridian -180/180 (but several issues remain to handle extra plot layers automatically).
    
  
## Internal (geeky) changes
    
  * an additional options as been added to prevent prompting during examples.
  * new internal function `.converts_months_to_numbers()`.


# v0.6

## New features
      
  * the maximum duration of running time for examples can now be controlled using `IsoriX.options(example_maxtime = XX)`.
  * due to new GNIP policies, we no longer provide the GNIP dataset for the entire World, but only a subset containing data for Germany (users should thus compile their precipitation data themselves from the 'wiser' platform provided by GNIP; see vignette Workflow).
  * it is now possible to control the colours and labels for the levels of isotopes or p-values in plots.
  * for plotting, it is no longer needed to load the ocean mask and country borders (it now happens automatically).
  * the function `relevate()` now allows for a cropping larger than the extent of the weather stations by means of the argument `margin_pct`.
  * it is now possible to create the so-called annual averaged precipitation isoscapes!
  * queryGNIP can now split the dataset per month or year at each location during the aggregation.
  * new function `prepcipitate()` to prepare the precipitation brick.
  * new function `getprecip()` to download monthly precipitation rasters from WorldClim.
  * new function `isomultifit()` fitting isoscapes per strata (month, year, or any "split").
  * new function `isomultiscape()` building isoscapes averaged across strata.
  * new function `create_aliens()` simulating of organism data.
    
## Minor changes
      
  * the inputs for filtering data by month or year using `queryGNIP()` have changed.
  * the default fixed effect structure for the mean model is `isofit()` has changed.
    
## Internal (geeky) changes
      
  * the namespace is now generated with Roxygen2.
  * the datasets are now 'lazy-loaded'.
  * new vignette for coding conventions.
  * changed some object names following our coding convention (more to come).


# v0.5

## Bug fixes
    
  * the package could not be detached and reloaded.
  * the citation was not correct.
  * the path in `getelev()` was breaking in some cases.
  * the title of the assignment plot was missing when a single individual was plotted.

## New features
      
  * new vignette explaining how to export spatial objects to GIS.
  * the file `GNIPdata` has been updated and now contains data for 2014.
  * names of all functions and objects have been refactored to remove upper cases.
  * links to our GitHub directory have been added.
  * new function `downloadfile()` to download non standard elevation raster or any other file.
  * function `getelev()` can perform MD5 sum checks if the package **tools** is installed.
  * function `getelev()` can display additional information during download if `verbose` > 1.
  * the column `animalID` in the assignment dataset can now handle names with spaces.
  * added **Codecov** to track test coverage for the package.
      
## Minor changes
      
  * the modification of the option `set_ll_warn` from the **sp** package has been moved to `.onLoad()` (instead of `.onAttach()`) and the original state is now restored while unloading IsoriX.
  * the Earth distance method has been moved to the package **spaMM**.
  * function `getelev()` lost its `address` argument as `downloadfile()` should now be used to download non-standard elevation rasters.
  * some typo fixed in documentation files.
  * **RandomFields** moved to suggest.
  * `*.Rd` files for documentation are now generated with Roxygen2.
  * `queryGNIP()` is now provided with a single month argument specifying the months to select.


# v0.4-1

* this was the first version of IsoriX submitted to CRAN.
      
