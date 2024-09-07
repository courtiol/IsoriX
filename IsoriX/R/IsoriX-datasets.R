#' Assignment datasets for bat species
#'
#' These datasets contain data from Voigt & Lenhert (2019). They contain hydrogen
#' delta values of fur keratin from common noctule bats (\emph{Nyctalus noctula})
#' killed at wind turbines in northern Germany. These data can be used as an
#' example to perform assignments using the function [`isofind`]. The difference
#' between `AssignDataBat` and `AssignDataBatRev` is that in the latter the bat
#' fur isotope values were corrected to align with the current delta values for deuterium
#' for keratin reference materials (Soto et al. 2017, https://doi.org/10.1002/rcm.7893)
#' ensuring comparability between formerly and more recently normalized datasets of delta values for deuterium.
#'
#' @name AssignDataBat
#' @aliases AssignDataBat AssignDataBatRev
#' @docType data
#' @format Two *dataframes* with 14 observations and 4 variables:
#' | | | |
#' |---|---|---|
#' |sample_ID|(*factor*)|Identification of the animal|
#' |lat|(*numeric*)|Latitude coordinate (decimal degrees)|
#' |long|(*numeric*)|Longitude coordinate (decimal degrees)|
#' |sample_value|(*numeric*)|Hydrogen delta value of the tissue|
#'
#' @seealso [`isofind`] to perform assignments
#' @references Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial
#' mammals using stable isotopes. In Hobson KA & Wassenaar LI (eds.), Tracking Animal
#' Migration with Stable Isotopes, second edition. Academic Press, London.
#'
#' Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of the hydrogen stable
#' isotopic composition of keratin calibration standards for wildlife and forensic
#' science applications. Rapid Commun Mass Spectrom. 31(14):1193-1203.
#' doi: 10.1002/rcm.7893. PMID: 28475227.
#'
#' @source data provided by  Voigt CC & Lehnert L.
#' @keywords datasets
#' @examples
#'
#' head(AssignDataBat)
#' str(AssignDataBat)
#'
NULL



#' Assignment datasets for bat species
#'
#' These datasets contain data from Voigt, Lehmann & Greif (2015). It contains
#' hydrogen delta values of fur keratin from bats captured in 2008, 2009 and
#' 2013 from their roosting sites in Bulgaria. We only retained the bats of the
#' genus Myotis from the original study. These data can be used as an example to
#' perform assignments using the function [`isofind`].  The difference
#' between `AssignDataBat2` and `AssignDataBat2Rev` is that in the latter the bat
#' fur isotope values were corrected to align with the current delta values for deuterium
#' for keratin reference materials (Soto et al. 2017, https://doi.org/10.1002/rcm.7893)
#' ensuring comparability between formerly and more recently normalized datasets of delta values for deuterium.
#'
#' @name AssignDataBat2
#' @aliases AssignDataBat2 AssignDataBat2Rev
#' @docType data
#' @format Two *dataframes* with 244 observations and 3 variables:
#' | | | |
#' |---|---|---|
#' | sample_ID | (*factor*) | Identification of the animal|
#' | species | (*factor*) | Animal species name|
#' | sample_value | (*numeric*) | Hydrogen delta value of the tissue|
#' @seealso [`isofind`] to perform assignments
#' @references Voigt CC, Lehmann D & Greif S (2015). Stable isotope
#' ratios of hydrogen separate mammals of aquatic and terrestrial food webs.
#' Methods in Ecology and Evolution 6(11).
#'
#' Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of the hydrogen stable
#' isotopic composition of keratin calibration standards for wildlife and forensic
#' science applications. Rapid Commun Mass Spectrom. 31(14):1193-1203.
#' doi: 10.1002/rcm.7893. PMID: 28475227.
#'
#' @source data provided by Voigt CC, Lehmann D & Greif S.
#' @keywords datasets
#' @examples
#'
#' head(AssignDataBat2)
#' str(AssignDataBat2)
#'
NULL



#' Calibration datasets for bat species
#'
#' These datasets contain hydrogen delta values of fur keratin from 6 sedentary
#' bat species. They correspond to the combination of several studies as detailed
#' in Voigt & Lenhert 2019. `CalibDataBat` is the dataset used in Courtiol et al. 2019.
#' The data can be used as an example to fit a calibration model using the
#' function [`calibfit`]. `CalibDataBatRev` is the same data but the bat
#' fur isotope values were corrected to align with the current delta values for deuterium
#' for keratin reference materials (Soto et al. 2017, https://doi.org/10.1002/rcm.7893)
#' ensuring comparability between formerly and more recently normalized datasets of delta values for deuterium.
#'
#' Users who wish to use their own dataset for calibration should create a
#' *dataframe* of similar structure than these ones (only the column 'species'
#' can be dropped). The columns should possess the same names as the ones
#' described above. If the elevation is unknown at the sampling sites, elevation
#' information can be extracted from a high resolution elevation raster using
#' the function [`terra::extract`] (see **Examples** in
#' [`CalibDataBat2`]).
#'
#' @name CalibDataBat
#' @aliases CalibDataBat CalibDataBatRev
#' @docType data
#' @format Two *dataframes* with 335 observations and 7 variables:
#' |   |   |   |
#' |---|---|---|
#' | site_ID | (*factor*) | Identification of the sampling site|
#' | long | (*numeric*) | Longitude coordinate (decimal degrees)|
#' | lat | (*numeric*) | Latitude coordinate (decimal degrees)|
#' | elev | (*numeric*) | Elevation asl (m)|
#' | sample_ID | (*factor*) | Identification of the sampled animal|
#' | species | (*factor*) | A code for the species|
#' | sample_value | (*numeric*) | Hydrogen delta value of the tissue|
#' @seealso [`CalibDataBat2`] for another (related) calibration dataset
#'
#' [`calibfit`] to fit a calibration model
#' @source data provided by Voigt CC & Lehnert L.

#' @references Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial
#' mammals using stable isotopes. In Hobson KA & Wassenaar LI (eds.), Tracking Animal
#' Migration with Stable Isotopes, second edition. Academic Press, London.
#'
#' Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson KA, Wassenaar LI & Kramer-Schadt S (2019). Isoscape
#' computation and inference of spatial origins with mixed models using the R package IsoriX. In Hobson KA & Wassenaar LI (eds.),
#' Tracking Animal Migration with Stable Isotopes, second edition. Academic Press, London.
#'
#' Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of the hydrogen stable
#' isotopic composition of keratin calibration standards for wildlife and forensic
#' science applications. Rapid Commun Mass Spectrom. 31(14):1193-1203.
#' doi: 10.1002/rcm.7893. PMID: 28475227.
#'
#' @keywords datasets
#' @examples
#'
#' head(CalibDataBat)
#' str(CalibDataBat)
NULL



#' Calibration datasets for bat species
#'
#' These datasets contain hydrogen delta values of fur keratin from sedentary
#' bat species captured between 2005 and 2009 from Popa-Lisseanu et al. (2012).
#' These data can be used as an example to fit a calibration model using the
#' function [`calibfit`]. The difference between `CalibDataBat2` and
#' `CalibDataBat2Rev` is that in the latter the bat
#' fur isotope values were corrected to align with the current delta values for deuterium
#' for keratin reference materials (Soto et al. 2017, https://doi.org/10.1002/rcm.7893)
#' ensuring comparability between formerly and more recently normalized datasets of delta values for deuterium.
#'
#' Users who wish to use their own dataset for calibration should create a
#' *dataframe* of similar structure than these ones (only the column
#' 'species' can be dropped). The columns should possess the same names as the
#' ones described above. If the elevation is unknown at the sampling sites,
#' elevation information can be extracted from a high resolution elevation
#' raster using the function [`terra::extract`] (see **Examples**).
#' Note that the original study used a different source of elevation data.
#'
#' @name CalibDataBat2
#' @aliases CalibDataBat2 CalibDataBat2Rev
#' @docType data
#' @format Two *dataframes* with 178 observations and 6 variables:
#' |   |   |   |
#' |---|---|---|
#' | site_ID | (*factor*) | Identification of the sampling site|
#' | long | (*numeric*) | Longitude coordinate (decimal degrees)|
#' | lat | (*numeric*) | Latitude coordinate (decimal degrees)|
#' | elev | (*numeric*) | Elevation asl (m)|
#' | sample_ID | (*factor*) | Identification of the sampled animal|
#' | sample_value | (*numeric*) | Hydrogen delta value of the tissue|
#' @seealso [`CalibDataBat`] for another (related) calibration dataset
#'
#' [`calibfit`] to fit a calibration model
#' @references Popa-Lisseanu AG, Soergel K, Luckner A, Wassenaar LI, Ibanez C,
#' Kramer-Schadt S, Ciechanowski M, Goerfoel T, Niermann I, Beuneux G,
#' Myslajek RW, Juste J, Fonderflick J, Kelm D & Voigt CC (2012).
#' A triple isotope approach to predict the breeding origins of European bats.
#' PLoS ONE 7(1):e30388.
#'
#' Soto DX, Koehler G, Wassenaar LI & Hobson KA (2017). Re-evaluation of the hydrogen stable
#' isotopic composition of keratin calibration standards for wildlife and forensic
#' science applications. Rapid Commun Mass Spectrom. 31(14):1193-1203.
#' doi: 10.1002/rcm.7893. PMID: 28475227.

#' @source data provided by Popa-Lisseanu AG et al.
#' @keywords datasets
#' @examples
#'
#' head(CalibDataBat2)
#' str(CalibDataBat2)
#'
#' ## The following example require to have downloaded
#' ## an elevation raster with the function getelev()
#' ## and will therefore not run unless you uncomment it
#'
#' # if (require(terra)){
#' #    ## We delete the elevation data
#' #    CalibDataBat2$elev <- NULL
#' #
#' #    ## We reconstruct the elevation data using an elevation raster
#' #    getelev(file = "elevBats.tif", z = 6,
#' #            lat_min = min(CalibDataBat2$lat),
#' #            lat_max = max(CalibDataBat2$lat),
#' #            long_min = min(CalibDataBat2$long),
#' #            long_max = max(CalibDataBat2$long))
#' #    ElevationRasterBig <- rast("elevBats.tif")
#' #    CalibDataBat2$elev <- extract(
#' #        ElevationRasterBig,
#' #        cbind(CalibDataBat2$long, CalibDataBat2$lat))
#' #    head(CalibDataBat2)
#' # }
#'
NULL



#' Simulated assignment dataset
#'
#' This dataset contains simulated hydrogen delta values.
#' The data can be used as an example to perform assignments using the function [`isofind`].
#'
#' @name AssignDataAlien
#' @docType data
#' @format A *dataframe* with 10 observations and 2 variables:
#' |   |   |   |
#' |---|---|---|
#' | sample_ID | (*factor*) | Identification of the sample|
#' | sample_value | (*numeric*) | Hydrogen delta value of the tissue|
#' @seealso [`isofind`] to perform assignments
#' @keywords datasets
#' @examples
#'
#' head(AssignDataAlien)
#' str(AssignDataAlien)
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## The following describes how we created such dataset
#'
#'   ### We prepare the precipitation data
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   ### We fit the models for Germany
#'   GermanFit <- isofit(data = GNIPDataDEagg)
#'
#'   ### We build the isoscape
#'   GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#'   ### We create a simulated dataset with 1 site and 10 observations
#'   set.seed(1L)
#'   Aliens <- create_aliens(
#'     calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'     isoscape = GermanScape,
#'     raster = ElevRasterDE,
#'     coordinates = data.frame(
#'       site_ID = "Berlin",
#'       long = 13.52134,
#'       lat = 52.50598
#'     ),
#'     n_sites = 1,
#'     min_n_samples = 10,
#'     max_n_samples = 10
#'   )
#'   AssignDataAlien <- Aliens[, c("sample_ID", "sample_value")]
#'
#'   ### Uncomment the following to store the file as we did
#'   # save(AssignDataAlien, file = "AssignDataAlien.rda", compress = "xz")
#' }
#'
NULL



#' Simulated calibration dataset
#'
#' This dataset contains simulated hydrogen delta values for corresponding locations
#' based on an assumed linear relationship between the animal tissue value and the
#' hydrogen delta values in the environment.
#' The data can be used as an example to fit a calibration model using the
#' function [`calibfit`].
#'
#' Users who wish to use their own dataset for calibration should create a
#' *dataframe* of similar structure than this one. The columns should possess
#' the same names as the ones described above. If the elevation is unknown at the
#' sampling sites, elevation information can be extracted from a high resolution elevation
#' raster using the function [`terra::extract`]. In this dataset, we
#' retrieved elevations from the Global Multi-resolution Terrain Elevation Data
#' 2010.
#'
#' @name CalibDataAlien
#' @docType data
#' @format A *dataframe* with x observations and 6 variables:
#' |   |   |   |
#' |---|---|---|
#' | site_ID | (*factor*) | Identification of the sampling site|
#' | long | (*numeric*) | Longitude coordinate (decimal degrees)|
#' | lat | (*numeric*) | Latitude coordinate (decimal degrees)|
#' | elev | (*numeric*) | Elevation asl (m)|
#' | sample_ID | (*factor*) | Identification of the sampled animal|
#' | tissue.value | (*numeric*) | Hydrogen delta value of the tissue|
#' @seealso [`calibfit`] to fit a calibration model
#' @keywords datasets
#' @examples
#'
#' head(CalibDataAlien)
#' str(CalibDataAlien)
#'
#' ## The examples below will only be run if sufficient time is allowed
#' ## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
#' ## if you want to allow for examples taking up to ca. XX seconds to run
#' ## (so don't write XX but put a number instead!)
#'
#' if (getOption_IsoriX("example_maxtime") > 30) {
#'   ## We prepare the precipitation data
#'   GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#'
#'   ## We fit the models for Germany
#'   GermanFit <- isofit(data = GNIPDataDEagg)
#'
#'   ## We build the isoscape
#'   GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#'   ## We create a simulated dataset with 50 site and 10 observations per site
#'   set.seed(2L)
#'   CalibDataAlien <- create_aliens(
#'     calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'     isoscape = GermanScape,
#'     raster = ElevRasterDE,
#'     n_sites = 50,
#'     min_n_samples = 10,
#'     max_n_samples = 10
#'   )
#'   plot(sample_value ~ source_value, data = CalibDataAlien)
#'   abline(3, 0.5)
#'
#'   CalibDataAlien$source_value <- NULL
#'
#'   ## Uncomment the following to store the file as we did
#'   # save(CalibDataAlien, file = "CalibDataAlien.rda", compress = "xz")
#' }
#'
NULL



#' Borders of world CountryBorders
#'
#' This dataset contains a polygon polygon SpatVector (from \pkg{terra}).
#' It can be used to draw the borders of world countries.
#'
#'
#' @name CountryBorders
#' @docType data
#' @format A *SpatVector* object
#' @seealso
#' - [`OceanMask`] for another polygon used to embellish the plots
#' @source This *SpatVector* is derived from the package
#'   \pkg{rnaturalearth}. Please refer to this other package for description and
#'   sources of this dataset. See example for details on how we created the
#'   dataset.
#' @keywords datasets
#' @examples
#'
#' plot(CountryBorders, border = "red", col = "darkgrey")
#'
#' ## How did we create this file?
#'
#' ## Uncomment the following to create the file as we did
#' # if (require(rnaturalearth) && require(terra)) {
#' #    CountryBorders <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sf')
#' #    CountryBorders <- vect(CountryBorders[, 0])
#' #    #saveRDS(CountryBorders, file = "IsoriX/inst/extdata/CountryBorders.rds", compress = "xz")
#' # }
#'
NULL



#' Mask of world oceans
#'
#' This dataset contains a polygon SpatVector (from \pkg{terra}).
#' It can be used to mask large bodies of water.
#'
#'
#' @name OceanMask
#' @docType data
#' @format A *SpatVector* object
#' @seealso
#' - [`CountryBorders`] for another polygon used to embellish the plots
#' @source See example for details on how we created the dataset.
#' @keywords datasets
#' @examples
#'
#' plot(OceanMask, col = "blue")
#'
#' ## How did we create this file?
#'
#' ## Uncomment the following to create the file as we did
#' # if (require(terra)) {
#' #   worldlimit <- vect(ext(CountryBorders))
#' #   crs(worldlimit) <- crs(CountryBorders)
#' #   OceanMask <- worldlimit - CountryBorders
#' #   #saveRDS(OceanMask, file = "IsoriX/inst/extdata/OceanMask.rds", compress = "xz")
#' # }
#'
NULL



#' The raster of elevation for Germany
#'
#' This raster contains the elevation of the surface of Germany (meters above sea
#' level) with a resolution of approximately 40 square-km.
#'
#' This raster contains elevation data of Germany in a highly aggregated form
#' corresponding to a resolution of approximately one elevation value per 40
#' square-km. This is only for the purpose of having a small and easy-to-handle
#' file to practice, but it should not be used to perform real assignments!
#'
#' @name ElevRasterDE
#' @docType data
#' @format A *SpatRaster* object
#' @seealso [`prepraster`] to crop and/or aggregate this raster
#' @source \url{https://topotools.cr.usgs.gov/gmted_viewer/viewer.htm}
#' @keywords datasets
#' @examples
#'
#' ## Compute crudely the resolution (approximative size of cells in km2)
#' median(values(cellSize(ElevRasterDE, unit = "km")))
#'
#' ## How did we create this file (without IsoriX) ?
#'
#' ## Uncomment the following to create the file as we did
#'
#' # ElevRasterDE <- elevatr::get_elev_raster(locations = data.frame(
#' #                              x = c(5.5, 15.5), y = c(47, 55.5)),
#' #                              prj = "+proj=longlat +datum=WGS84 +no_defs",
#' #                              clip = "bbox", z = 3)
#' #
#' # ElevRasterDE <- terra::rast(ElevRasterDE)
#'
#'
#' ## How to create a similar file with IsoriX ?
#' #
#' # ## Download the tif file (see ?getelev)
#' # getelev(file = "~/ElevRasterDE.tif",
#' #         z = 3,
#' #         long_min = 5.5, long_max = 15.5, lat_min = 47, lat_max = 55.5)
#'
#' # ## Convert the tif into R raster format
#' # ElevRasterDE <- rast('~/ElevRasterDE.tif')
#'
NULL



#' The precipitation monthly amounts for Germany
#'
#' This brick of rasters contains the monthly precipitation amounts (in mm) for
#' Germany with a resolution of approximately 30 square-km.
#'
#' The data are derived from "precipitation (mm) WorldClim Version2" which can
#' be downloaded using the function [`getprecip`].
#'
#' @name PrecipBrickDE
#' @docType data
#' @format A *SpatRaster* with 12 layers
#' @seealso [`prepcipitate`] to prepare this raster
#' @source \url{https://worldclim.org/data/worldclim21.html}
#' @keywords datasets
#' @examples
#'
#' ## The following example requires to download
#' ## a large precipitation rasters with the function getprecip()
#' ## and will therefore not run unless you uncomment it
#'
#' ## How did we create this file?
#'
#' ## Uncomment the following to create the file as we did
#' # getprecip() ## Download the tif files (~ 1 Gb compressed)
#' # PrecipBrickDE <- prepcipitate(raster = ElevRasterDE)
#' # terra::saveRDS(PrecipBrickDE, file = "PrecipBrickDE.rds", compress = "xz")
#'
NULL



#' Hydrogen delta values in precipitation water, Germany
#'
#' This dataset contains the hydrogen delta value from
#' precipitation water sampled at weather stations between 1961 and 2013 in
#' Germany. These data have been kindly provided by Christine Stumpp and
#' processed by the International Atomic Energy Agency IAEA in Vienna (GNIP
#' Project: Global Network of Isotopes in Precipitation). These data are free to
#' reuse provided the relevant citations (see references). These data represent
#' a small sample of the much larger dataset compiled by the GNIP. We no longer
#' provide larger GNIP dataset in the package as those are not free to reuse (but
#' we do provide aggregated versions of it; see [`GNIPDataEUagg`]).
#' You can still download the complete GNIP dataset for free, but you will have
#' to proceed to a registration process with GNIP and use their downloading
#' interface WISER (\url{https://nucleus.iaea.org/wiser/index.aspx}).
#'
#' The dataset contains non-aggregated data for 27 weather stations across Germany.
#'
#' This dataset is the raw data source and should not be directly used for
#' fitting isoscapes.
#'
#' Please use [`prepsources`] to filter the dataset by time and
#' location.
#'
#' If you want to use your own dataset, you must format your data as those
#' produced by the function [`prepsources`].
#'
#' @name GNIPDataDE
#' @docType data
#' @format The *dataframe* includes 8591 observations and the following
#' variables:
#' |   |   |   |
#' |---|---|---|
#' | lat | (*numeric*) | Latitude coordinate (decimal degrees)|
#' | long | (*numeric*) | Longitude coordinate (decimal degrees)|
#' | elev | (*numeric*) | Elevation asl (m)|
#' | source_value | (*numeric*) | hydrogen delta value (per thousand)|
#' | year | (*numeric*) | Year of sampling|
#' | month | (*numeric*) | Month of sampling|
#' | source_ID | (*factor*) | The unique identifier of the weather station|
#' @seealso [`prepsources`] to prepare the dataset for the analyses and
#' to filter by time and location.
#' @references GNIP Project IAEA Global Network of Isotopes in Precipitation: \url{https://www.iaea.org}
#'
#' Stumpp, C., Klaus, J., & Stichler, W. (2014). Analysis of long-term stable isotopic composition in German precipitation. Journal of hydrology, 517, 351-361.
#'
#' Klaus, J., Chun, K. P., & Stumpp, C. (2015). Temporal trends in d18O composition of precipitation in Germany: insights from time series modelling and trend analysis. Hydrological Processes, 29(12), 2668-2680.
#'
#' @source data provided by the IAEA.
#' @keywords datasets
#' @examples
#'
#' head(GNIPDataDE)
#'
NULL



#' Hydrogen delta values in precipitation water (aggregated per location)
#'
#' These datasets contain the mean and variance of hydrogen delta value from
#' precipitation water sampled at weather stations between 1953 and 2015 in
#' Europe (`GNIPDataEUagg`) and in the entire world (`GNIPDataALLagg`). These
#' data have been extracted from the International Atomic Energy Agency IAEA in
#' Vienna (GNIP Project: Global Network of Isotopes in Precipitation) and
#' processed by us using the function [`prepsources`]. The data are aggregated per
#' location (across all month-year combinations). We no longer provide the full
#' non-aggregate GNIP dataset in the package as it is not free to reuse. You can
#' still download the complete GNIP dataset for free, but you will have to
#' proceed to a registration process with GNIP and use their downloading
#' interface WISER
#' (\url{https://nucleus.iaea.org/wiser/index.aspx}).
#'
#' These datasets have been aggregated and can thus be directly used for fitting
#' isoscapes.
#'
#' If you want to use your own dataset, you must format your data as these
#' datasets.
#'
#' @name GNIPDataEUagg
#' @aliases GNIPDataEUagg GNIPDataALLagg
#' @docType data
#' @format The *dataframe*s include many observations and the following
#' variables:
#' |   |   |   |
#' |---|---|---|
#' | source_ID | (*factor*) | The unique identifier of the weather station|
#' | mean_source_value | (*numeric*) | Average of the aggregate of hydrogen delta values (per thousand)|
#' | var_source_value | (*numeric*) | Variance of the aggregate of hydrogen delta values (per thousand^2)|
#' | n_source_value | (*numeric*) | Number of hydrogen delta values aggregated|
#' | lat | (*numeric*) | Latitude coordinate (decimal degrees)|
#' | long | (*numeric*) | Longitude coordinate (decimal degrees)|
#' | elev | (*numeric*) | Elevation asl (m)|
#' @seealso [`GNIPDataDE`] for a non-aggregated dataset.
#' @references GNIP Project IAEA Global Network of Isotopes in Precipitation: \url{https://www.iaea.org}
#' @source data provided by the IAEA and processed by us.
#' @keywords datasets
#' @examples
#'
#' head(GNIPDataALLagg)
#' dim(GNIPDataALLagg)
#' head(GNIPDataEUagg)
#' dim(GNIPDataEUagg)
#'
NULL



#' Colour palettes for plotting
#'
#' These datasets contain colour vectors that can be used for plotting. In our
#' examples, we use the `isopalette1` for plotting the isoscape using
#' [`plot.ISOSCAPE`] and `isopalette2` for plotting the
#' assignment outcome using [`plot.ISOFIND`].
#'
#' Colour palettes can be created by using the function [`colorRamp`]
#' that interpolates colours between a set of given colours. One can also use
#' [`colorRampPalette`] to create functions providing colours. Also
#' interesting, the function [`colorspace::choose_palette`] offers a GUI
#' interface allowing to create and save a palette in a hexadecimal format
#' (which can later on be imported into R). This latter function is however
#' limited to a maximum of 50 colours. You can also use R colour palettes
#' already available such as [`terrain.colors`] or others available
#' (see examples below). Alternatively, you can design your own colour palette
#' by writing standard hexadecimal code of colours into a vector.
#'
#' @name isopalette2
#' @aliases isopalette2 isopalette1
#' @docType data
#' @format A vector of colours
#' @note We use the package \pkg{rasterVis} for plotting. Instead of using
#' colour palettes directly, one can also use any "Theme" designed for the
#' lattice graphic environment (see source for details).
#' @seealso
#' [`grDevices::rainbow`] for information about R colour palettes
#' 
#' [`grDevices::colorRamp`] and [`colorspace::choose_palette`] to create your
#' own palettes
#' @source For information on how to use themes, check:
#'
#' \url{https://oscarperpinan.github.io/rastervis/#themes}
#' @keywords color datasets
#' @examples
#'
#' ## A comparison of some colour palette
#'
#' par(mfrow = c(2, 3))
#' pie(rep(1, length(isopalette1)),
#'   col = isopalette1,
#'   border = NA, labels = NA, clockwise = TRUE, main = "isopalette1"
#' )
#' pie(rep(1, length(isopalette2)),
#'   col = isopalette2,
#'   border = NA, labels = NA, clockwise = TRUE, main = "isopalette2"
#' )
#' pie(rep(1, 100),
#'   col = terrain.colors(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "terrain.colors"
#' )
#' pie(rep(1, 100),
#'   col = rainbow(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "rainbow"
#' )
#' pie(rep(1, 100),
#'   col = topo.colors(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "topo.colors"
#' )
#' pie(rep(1, 100),
#'   col = heat.colors(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "heat.colors"
#' )
#'
#' ## Creating your own colour palette
#' MyPalette <- colorRampPalette(c("blue", "green", "red"), bias = 0.7)
#' par(mfrow = c(1, 1))
#' pie(1:100,
#'   col = MyPalette(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "a home-made palette"
#' )
#'
#' ## Turing palettes into functions for use in IsoriX
#' Isopalette1Fn <- colorRampPalette(isopalette1, bias = 0.5)
#' Isopalette2Fn <- colorRampPalette(isopalette2, bias = 0.5)
#' par(mfrow = c(1, 2))
#' pie(1:100,
#'   col = Isopalette1Fn(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "isopalette1"
#' )
#' pie(1:100,
#'   col = Isopalette2Fn(100), border = NA, labels = NA,
#'   clockwise = TRUE, main = "isopalette2"
#' )
#'
NULL
