# Package index

## Package overview

- [`IsoriX-package`](https://courtiol.github.io/IsoriX/reference/IsoriX-package.md)
  [`IsoriX`](https://courtiol.github.io/IsoriX/reference/IsoriX-package.md)
  : Isoscape Computation and Inference of Spatial Origins using Mixed
  Models

## Preparing the data

- [`prepsources()`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
  : Filter and aggregate the raw source dataset
- [`prepraster()`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
  : Prepare the structural raster
- [`prepcipitate()`](https://courtiol.github.io/IsoriX/reference/prepcipitate.md)
  : Prepare the raster brick containing the precipitation data
- [`getelev()`](https://courtiol.github.io/IsoriX/reference/getelev.md)
  : Download an elevation raster from internet
- [`getprecip()`](https://courtiol.github.io/IsoriX/reference/getprecip.md)
  : Download rasters of monthly precipitation from internet
- [`downloadfile()`](https://courtiol.github.io/IsoriX/reference/downloadfile.md)
  : Download files and check their binary integrity
- [`create_aliens()`](https://courtiol.github.io/IsoriX/reference/create_aliens.md)
  : Simulate datasets for calibrations or assignments

## Building isoscapes

- [`isofit()`](https://courtiol.github.io/IsoriX/reference/isofit.md) :
  Fit the isoscape models
- [`isomultifit()`](https://courtiol.github.io/IsoriX/reference/isomultifit.md)
  : Fit isoscape models per strata (typically time interval such as
  months)
- [`isoscape()`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
  : Predicts the spatial distribution of source isotopic values
- [`isomultiscape()`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
  : Predicts the average spatial distribution of isotopic values over
  months, years...
- [`AIC(`*`<ISOFIT>`*`)`](https://courtiol.github.io/IsoriX/reference/AIC.ISOFIT.md)
  : Compute the conditional AIC of fitted isoscapes

## Calibration

- [`calibfit()`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
  : Fit the calibration model (or load parameters from calibration done
  outside IsoriX)
- [`calibscape()`](https://courtiol.github.io/IsoriX/reference/calibscape.md)
  : Builds calibrated isoscapes, i.e. isoscapes of samples (e.g.
  tissues)

## Assignment

- [`isofind()`](https://courtiol.github.io/IsoriX/reference/isofind.md)
  : Infer spatial origins

## Plotting

- [`points(`*`<CALIBFIT>`*`)`](https://courtiol.github.io/IsoriX/reference/plots.md)
  [`plot()`](https://courtiol.github.io/IsoriX/reference/plots.md) :
  Plotting functions for IsoriX
- [`plot_sources()`](https://courtiol.github.io/IsoriX/reference/plot_sources.md)
  : Map the aggregated source data
- [`plot_dependencies()`](https://courtiol.github.io/IsoriX/reference/plot_dependencies.md)
  : Plot the dependencies of IsoriX
- [`isopalette2`](https://courtiol.github.io/IsoriX/reference/isopalette2.md)
  [`isopalette1`](https://courtiol.github.io/IsoriX/reference/isopalette2.md)
  : Plotting: colour palettes for isoscapes and assignments

## Classes

- [`CALIBFIT-class`](https://courtiol.github.io/IsoriX/reference/CALIBFIT-class.md)
  : Class CALIBFIT
- [`ISOFIND-class`](https://courtiol.github.io/IsoriX/reference/ISOFIND-class.md)
  : Class ISOFIND
- [`ISOSCAPE-class`](https://courtiol.github.io/IsoriX/reference/ISOSCAPE-class.md)
  : Class ISOSCAPE

## Options and saving

- [`options_IsoriX()`](https://courtiol.github.io/IsoriX/reference/options.md)
  [`getOption_IsoriX()`](https://courtiol.github.io/IsoriX/reference/options.md)
  : Setting and displaying the options of the package
- [`saveRDS_IsoriX()`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  [`saveRDS(`*`<ISOSCAPE>`*`)`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  [`saveRDS(`*`<CALIBFIT>`*`)`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  [`saveRDS(`*`<ISOFIND>`*`)`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  [`readRDS(`*`<character>`*`)`](https://courtiol.github.io/IsoriX/reference/serialize.md)
  : Save and read objects produced by IsoriX using RDS files

## Datasets

- [`AssignDataAlien`](https://courtiol.github.io/IsoriX/reference/AssignDataAlien.md)
  : Assignment data: simulated hydrogen delta values
- [`AssignDataBat`](https://courtiol.github.io/IsoriX/reference/AssignDataBat.md)
  [`AssignDataBatRev`](https://courtiol.github.io/IsoriX/reference/AssignDataBat.md)
  : Assignment data: hydrogen delta values of common noctule bats (Voigt
  & Lehnert 2019)
- [`AssignDataBat2`](https://courtiol.github.io/IsoriX/reference/AssignDataBat2.md)
  [`AssignDataBat2Rev`](https://courtiol.github.io/IsoriX/reference/AssignDataBat2.md)
  : Assignment data: hydrogen delta values of Myotis bats (Voigt,
  Lehmann & Greif 2015)
- [`CalibDataAlien`](https://courtiol.github.io/IsoriX/reference/CalibDataAlien.md)
  : Calibration data: simulated hydrogen delta values
- [`CalibDataBat`](https://courtiol.github.io/IsoriX/reference/CalibDataBat.md)
  [`CalibDataBatRev`](https://courtiol.github.io/IsoriX/reference/CalibDataBat.md)
  : Calibration data: hydrogen delta values of sedentary bats (Voigt &
  Lehnert 2019)
- [`CalibDataBat2`](https://courtiol.github.io/IsoriX/reference/CalibDataBat2.md)
  [`CalibDataBat2Rev`](https://courtiol.github.io/IsoriX/reference/CalibDataBat2.md)
  : Calibration data: hydrogen delta values of sedentary bats
  (Popa-Lisseanu et al. 2012)
- [`GNIPDataDE`](https://courtiol.github.io/IsoriX/reference/GNIPDataDE.md)
  : Source data: hydrogen delta values in precipitation water in Germany
- [`GNIPDataEUagg`](https://courtiol.github.io/IsoriX/reference/GNIPDataEUagg.md)
  [`GNIPDataALLagg`](https://courtiol.github.io/IsoriX/reference/GNIPDataEUagg.md)
  : Source data: hydrogen delta values in precipitation water in Europe
  and in the world (aggregated per location)
- [`CountryBorders`](https://courtiol.github.io/IsoriX/reference/CountryBorders.md)
  : Polygon data: borders of world countries
- [`OceanMask`](https://courtiol.github.io/IsoriX/reference/OceanMask.md)
  : Polygon data: mask of world oceans
- [`ElevRasterDE`](https://courtiol.github.io/IsoriX/reference/ElevRasterDE.md)
  : Raster data: elevation in Germany
- [`PrecipBrickDE`](https://courtiol.github.io/IsoriX/reference/PrecipBrickDE.md)
  : Raster data: monthly precipitation amounts in Germany

## Defunct and deprecated

- [`IsoriX-defunct`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`Calibfit`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`GetElev`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`Isofit`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`Isorix`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`Isoscape`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`Isosim`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`QueryGNIP`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`queryGNIP`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`prepdata`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`prepelev`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`prepiso`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`RElevate`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  [`relevate`](https://courtiol.github.io/IsoriX/reference/IsoriX-defunct.md)
  : Defunct and deprecated functions
