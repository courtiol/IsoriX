#' Assignment dataset for bat species
#' 
#' This dataset contains data from Voigt & Lenhert (2019). It contains hydrogen
#' delta values of fur keratin from common noctule bats (\emph{Nyctalus noctula})
#' killed at wind turbines in northern Germany. The data can be used as an
#' example to perform assignments using the function \code{\link{isofind}}.
#' 
#' @name AssignDataBat
#' @docType data
#' @noMd
#' @format A \var{dataframe} with 14 observations on 4 variables:
#' \tabular{rlll}{
#' [, 1] \tab sample_ID \tab (\var{Factor}) \tab Identification of the animal\cr
#' [, 2] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 2] \tab lomg \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 4] \tab sample_value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr }
#' @seealso \code{\link{isofind}} to perform assignments
#' @references Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial 
#' mammals using stable isotopes. In Hobson KA, Wassenaar LI (eds.), Tracking Animal
#' Migration with Stable Isotopes, second edition. Academic Press, London.
#' 
#' @source data directly provided by the authors of the following publication
#' @keywords datasets
#' @examples
#' 
#' head(AssignDataBat)
#' str(AssignDataBat)
#' 
NULL



#' Assignment dataset for bat species
#' 
#' This dataset contains data from Voigt, Lehmann and Greif (2015). It contains
#' hydrogen delta values of fur keratin from bats captured in 2008, 2009 and
#' 2013 from their roosting sites in Bulgaria. We only retained the bats of the
#' genus Myotis from the original study. The data can be used as an example to
#' perform assignments using the function \code{\link{isofind}}.
#' 
#' @name AssignDataBat2
#' @docType data
#' @noMd
#' @format A \var{dataframe} with 244 observations on 3 variables:
#' \tabular{rlll}{
#' [, 1] \tab sample_ID \tab (\var{Factor}) \tab Identification of the animal\cr
#' [, 2] \tab species \tab (\var{Factor}) \tab Animal species name\cr
#' [, 3] \tab sample_value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr }
#' @seealso \code{\link{isofind}} to perform assignments
#' @references Voigt, C.C., Lehmann, D., Greif, S. (2015). Stable isotope
#' ratios of hydrogen separate mammals of aquatic and terrestrial food webs.
#' Methods in Ecology and Evolution 6(11).
#' @source data directly provided by the authors of the following publication
#' @keywords datasets
#' @examples
#' 
#' head(AssignDataBat2)
#' str(AssignDataBat2)
#' 
NULL



#' Calibration dataset for bat species
#' 
#' This dataset contains hydrogen delta values of fur keratin from 6 sedentary
#' bat species. It corresponds to the combination of several studies as detailed
#' in Voigt & Lenhert 2019. This is the dataset used in Courtiol et al. 2019.
#' The data can be used as an example to fit a calibration model using the
#' function \code{\link{calibfit}}.
#' 
#' Users who wish to use their own dataset for calibration should create a
#' \var{dataframe} of similar structure than this one (only the column
#' 'species' can be dropped). The columns should possess the same names as the
#' ones described above. If the elevation is unknown at the sampling sites,
#' elevation information can be extracted from a high resolution elevation
#' raster using the function \code{\link[raster]{extract}} as shown in the 
#' example of \code{\link{CalibDataBat2}}.
#' 
#' @name CalibDataBat
#' @docType data
#' @noMd
#' @format A \var{dataframe} with 335 observations on 7 variables:
#' \tabular{rlll}{
#' [, 1] \tab site_ID \tab (\var{Factor}) \tab Identification of the sampling site\cr
#' [, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 3] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 4] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
#' [, 5] \tab sample_ID \tab (\var{Factor}) \tab Identification of the sampled animal\cr
#' [, 6] \tab species \tab (\var{Factor}) \tab A code for the species\cr
#' [, 7] \tab sample_value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr }
#' @seealso \code{\link{CalibDataBat2}} for another (related) calibration dataset
#' 
#' \code{\link{calibfit}} to fit a calibration model
#' @references Voigt CC & Lehnert L (2019). Tracking of movements of terrestrial 
#' mammals using stable isotopes. In Hobson KA, Wassenaar LI (eds.), Tracking Animal
#' Migration with Stable Isotopes, second edition. Academic Press, London.
#' 
#' Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson KA, Wassenaar LI, Kramer-Schadt S (2019). Isoscape
#' computation and inference of spatial origins with mixed models using the R package IsoriX. In Hobson KA, Wassenaar LI (eds.),
#' Tracking Animal Migration with Stable Isotopes, second edition. Academic Press, London.
#' 
#' @keywords datasets
#' @examples
#' 
#' head(CalibDataBat)
#' str(CalibDataBat)
NULL



#' Calibration dataset for bat species
#' 
#' This dataset contains hydrogen delta values of fur keratin from sedentary
#' bat species captured between 2005 and 2009 from Popa-Lisseanu et al. (2012).
#' The data can be used as an example to fit a calibration model using the
#' function \code{\link{calibfit}}.
#' 
#' Users who wish to use their own dataset for calibration should create a
#' \var{dataframe} of similar structure than this one (only the column
#' 'species' can be dropped). The columns should possess the same names as the
#' ones described above. If the elevation is unknown at the sampling sites,
#' elevation information can be extracted from a high resolution elevation
#' raster using the function \code{\link[raster]{extract}}. In this dataset, we
#' retrieved elevations from the Global Multi-resolution Terrain Elevation Data
#' 2010. Note that the original study used a different source of elevation
#' data.
#' 
#' @name CalibDataBat2
#' @docType data
#' @noMd
#' @format A \var{dataframe} with 178 observations on 6 variables:
#' \tabular{rlll}{
#' [, 1] \tab site_ID \tab (\var{Factor}) \tab Identification of the sampling site\cr
#' [, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 3] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 4] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
#' [, 5] \tab sample_ID \tab (\var{Factor}) \tab Identification of the sampled animal\cr
#' [, 6]  \tab sample_value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr }
#' @seealso \code{\link{CalibDataBat}} for another (related) calibration dataset
#' 
#' \code{\link{calibfit}} to fit a calibration model
#' @references Popa-Lisseanu, A. G., Soergel, K., Luckner, A., Wassenaar, L.
#' I., Ibanez, C., Kramer-Schadt, S., Ciechanowski, M., Goerfoel, T., Niermann,
#' I., Beuneux, G., Myslajek, R. W., Juste, J., Fonderflick, J., Kelm, D.,
#' Voigt, C. C. (2012). A triple isotope approach to predict the breeding
#' origins of European bats. PLoS ONE 7(1):e30388.
#' @source data directly provided by the authors of the following publication
#' @keywords datasets
#' @examples
#' 
#' head(CalibDataBat2)
#' str(CalibDataBat2)
#' 
#' ## The following example require to have downloaded
#' ## a large elevation raster with the function getelev()
#' ## and will therefore not run unless you uncomment it
#' 
#' #if(require(raster)){
#' #    ## We delete the elevation data
#' #    CalibDataBat2$elev <- NULL
#' #
#' #    ## We reconstruct the elevation data using an elevation raster
#' #    ## (see ?getelev for details on how to get the tif file)
#' #    ElevationRasterBig <- raster("gmted2010_30mn.tif")
#' #    CalibDataBat2$elev <- extract(
#' #        ElevationRasterBig,
#' #        cbind(CalibDataBat2$long, CalibDataBat2$lat))
#' #    head(CalibDataBat2)
#' #}
#' 
#' 
NULL



#' Simulated assignment dataset
#' 
#' This dataset contains simulated hydrogen delta values. 
#' The data can be used as an example to perform assignments using the function \code{\link{isofind}}.
#' 
#' @name AssignDataAlien
#' @docType data
#' @noMd
#' @format A \var{dataframe} with 10 observations on 2 variables:
#' \tabular{rlll}{
#' [, 1] \tab sample_ID \tab (\var{Factor}) \tab Identification of the sample\cr
#' [, 2] \tab sample_value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr}
#' @seealso \code{\link{isofind}} to perform assignments
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
#' if(getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## The following describes how we created such dataset
#' 
#' ### We prepare the precipitation data
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ### We fit the models for Germany
#' GermanFit <- isofit(data = GNIPDataDEagg)
#'
#' ### We build the isoscape
#' GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#' ### We create a simulated dataset with 1 site and 10 observations
#' set.seed(1L)
#' Aliens <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'                         isoscape = GermanScape,
#'                         raster = ElevRasterDE,
#'                         coordinates = data.frame(site_ID = "Berlin",
#'                                                  long   = 13.52134,
#'                                                  lat    = 52.50598),
#'                         n_sites = 1,
#'                         min_n_samples = 10,
#'                         max_n_samples = 10)
#' AssignDataAlien <- Aliens[, c("sample_ID", "sample_value")]
#' 
#' ### Uncomment the following to store the file as we did
#' #save(AssignDataAlien, file = "AssignDataAlien.rda", compress = "xz")
#' 
#' }
#' 
NULL



#' Simulated calibration dataset
#' 
#' This dataset contains simulated hydrogen delta values for corresponding locations
#' based on an assumed linear relationship between the animal tissue value and the 
#' hydrogen delta values in the environment.
#' The data can be used as an example to fit a calibration model using the
#' function \code{\link{calibfit}}.
#' 
#' Users who wish to use their own dataset for calibration should create a
#' \var{dataframe} of similar structure than this one. The columns should possess 
#' the same names as the ones described above. If the elevation is unknown at the 
#' sampling sites, elevation information can be extracted from a high resolution elevation
#' raster using the function \code{\link[raster]{extract}}. In this dataset, we
#' retrieved elevations from the Global Multi-resolution Terrain Elevation Data
#' 2010.
#' 
#' @name CalibDataAlien
#' @docType data
#' @noMd
#' @format A \var{dataframe} with x observations on 6 variables:
#' \tabular{rlll}{
#' [, 1] \tab site_ID \tab (\var{Factor}) \tab Identification of the sampling site\cr
#' [, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 3] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 4] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
#' [, 5] \tab sample_ID \tab (\var{Factor}) \tab Identification of the sampled animal\cr
#' [, 6] \tab tissue.value \tab (\var{numeric}) \tab Hydrogen delta value of the tissue\cr }
#' @seealso \code{\link{calibfit}} to fit a calibration model
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
#' if(getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## We prepare the precipitation data
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ## We fit the models for Germany
#' GermanFit <- isofit(data = GNIPDataDEagg)
#'
#' ## We build the isoscape
#' GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#'
#' ## We create a simulated dataset with 50 site and 10 observations per site
#' set.seed(2L)
#' CalibDataAlien <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
#'                                 isoscape = GermanScape,
#'                                 raster = ElevRasterDE,
#'                                 n_sites = 50,
#'                                 min_n_samples = 10,
#'                                 max_n_samples = 10)
#' plot(sample_value ~ source_value, data = CalibDataAlien)
#' abline(3, 0.5)
#' 
#' CalibDataAlien$source_value <- NULL
#' 
#' ## Uncomment the following to store the file as we did
#' #save(CalibDataAlien, file = "CalibDataAlien.rda", compress = "xz")
#' 
#' }
#' 
#' 
NULL



#' Mask of world oceans
#' 
#' This dataset contains a polygon shapefile that can be used to mask large
#' bodies of water.
#' 
#' 
#' @name OceanMask
#' @docType data
#' @format A \var{SpatialPolygons} object
#' @seealso \code{\link{CountryBorders}} for another polygon shapefile used to
#' embellish the plots
#' @source This \var{SpatialPolygons} is derived from the
#' \code{\link{CountryBorders}}. See example for details on how we created the
#' dataset.
#' @keywords datasets
#' @examples
#' 
#' if(require(sp)) {
#'   plot(OceanMask, col='blue')
#' }
#' 
#' ## How did we create this file?
#' 
#' ## Uncomment the following to create the file as we did
#' #if(require(raster) & require(rgeos)){
#' #    worldlimit <- as(extent(CountryBorders), "SpatialPolygons")
#' #    proj4string(worldlimit) <- crs(CountryBorders)
#' #    OceanMask <- gDifference(worldlimit, CountryBorders)  
#' #    OceanMask
#' #    save(OceanMask, file = "OceanMask.rda", compress = "xz")
#' #}
#' 
#' 
NULL



#' Borders of world CountryBorders
#' 
#' This dataset contains a polygon shapefile that can be used to plot the
#' borders of CountryBorders.
#' 
#' 
#' @name CountryBorders
#' @docType data
#' @format A \var{SpatialPolygons}
#' @seealso \code{\link{OceanMask}} for another polygon shapefile used to
#' embellish the plots
#' @source This \var{SpatialPolygons} is derived from the
#' \code{\link[maps]{world}} of the package \pkg{maps}. Please refer to this
#' other package for description and sources of this dataset. See example for
#' details on how we created the dataset.
#' @keywords datasets
#' @examples
#' 
#' if(require(sp))
#'   plot(CountryBorders, border="red", col="darkgrey")
#' 
#' ## How did we create this file?
#' 
#' ## Uncomment the following to create the file as we did
#' #if(require(maps) & require(maptools) & require(rgeos)){
#' #    worldmap <- map("world", fill = TRUE, plot = FALSE)
#' #    CountryBorders <- map2SpatialPolygons(worldmap, IDs = worldmap$names)
#' #    CountryBorders <- gBuffer(CountryBorders, byid = TRUE, width = 0)
#' #    proj4string(CountryBorders) <- CRS(as.character(NA))
#' #    CountryBorders
#' #    save(CountryBorders, file = "CountryBorders.rda", compress = "xz")
#' #}
#' 
NULL



#' The raster of elevation for Germany
#' 
#' This raster contains the elevation of the surface of Germany (meters above sea
#' level) with a resolution of approximately 30 square-km.
#' 
#' This raster contains elevation data of Germany in a highly aggregated form
#' corresponding to a resolution of approximately one elevation value per 50
#' square-km. This is only for the purpose of having a small and easy-to-handle
#' file to practice, but it should not be used to perform real assignments!
#' 
#' In the example below, we show how we generated this small raster from a
#' large original \var{DEM} (digital elevation model) of the entire world. The original raster has
#' a resolution of approximately one elevation value per square-km (cell size
#' of 30 arcseconds, i.e. 0.0083 decimal degrees). Although working on large
#' rasters is technically problematic (memory and CPU greedy), we highly
#' recommend to rely on high-resolution rasters with small to moderate
#' aggregation levels in order to perform reliable assignment analyses. Indeed,
#' large aggregation of raster cells can bias assignments due to the
#' transformation of all elevations into a single value per aggregated raster
#' cell.
#' 
#' We downloaded "Global Multi-resolution Terrain Elevation Data 2010" from:
#' \cr \url{https://topotools.cr.usgs.gov/gmted_viewer/viewer.htm} \cr and converted it
#' into a \var{tif} file. Because the original file is very large, we directly
#' provide the url link of the \var{tif} file in the example below.
#' 
#' @name ElevRasterDE
#' @docType data
#' @format A \var{RasterLayer}
#' @seealso \code{\link{prepraster}} to crop and/or aggregate this raster
#' @source \url{https://topotools.cr.usgs.gov/gmted_viewer/viewer.htm}
#' @keywords datasets
#' @examples
#' 
#' ## The following example require to download
#' ## a large elevation raster with the function getelev()
#' ## and will therefore not run unless you uncomment it
#' 
#' #### Creating the object ElevRasterDE
#' #
#' ### Download the tif file (ca. 700 Mb)
#' ### (see ?getelev for details on how to get the tif file)
#' #getelev()
#' #
#' ### Convert the tif into R raster format
#' #  ElevationRasterBig <- raster("gmted2010_30mn.tif")
#' #  
#' #  ## Create the highly agregated elevation raster
#' #  ElevRasterDE <- prepraster(ElevationRasterBig,
#' #                             aggregation_factor = 10,
#' #                             manual_crop = c(5.5, 15.5, 47, 55.5))
#' #                           
#' #  ## Plot the elevation
#' #  levelplot(ElevRasterDE, margin = FALSE, par.settings=RdBuTheme()) +
#' #    layer(sp.polygons(CountryBorders, col = "white"))
#' #  
#' #  ## Compute crudely the resolution:
#' #  median(values(area(ElevRasterDE)))  ## approximative size of cells in km2
#' 
NULL



#' The precipitation monthly amounts for Germany
#'
#' This brick of rasters contains the monthly precipitation amounts (in mm) for
#' Germany with a resolution of approximately 30 square-km.
#'
#' The data are derived from "precipitation (mm) WorldClim Version2" which can
#' be downloaded using the function \code{\link{getprecip}}.
#' 
#' @name PrecipBrickDE
#' @docType data
#' @format A \var{RasterBrick}
#' @seealso \code{\link{prepcipitate}} to prepare this raster
#' @source \url{https://www.worldclim.org/data/worldclim21.html}
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
#' #getprecip() ## Download the tif files (~ 1 Gb compressed)
#' #PrecipBrickDE <- prepcipitate(raster = ElevRasterDE)
#' #save(PrecipBrickDE, file = "PrecipBrickDE", compress = "xz")
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
#' we do provide aggregated verisons of it; see \code{\link{GNIPDataEUagg}}).
#' You can still download the complete GNIP dataset for free, but you will have
#' to proceed to a registration process with GNIP and use their downloading
#' interface WISER (\url{http://www-naweb.iaea.org/napc/ih/IHS_resources_isohis.html}).
#' 
#' The dataset contains non-aggregated data for 27 weather stations across Germany.
#' 
#' This dataset is the raw data source and should not be directly used for
#' fitting isoscapes.
#' 
#' Please use \code{\link{prepsources}} to filter the dataset by time and
#' location.
#' 
#' If you want to use your own dataset, you must format your data as those
#' produced by the function \code{\link{prepsources}}.
#' 
#' @name GNIPDataDE
#' @docType data
#' @noMd
#' @format The \var{dataframe} includes 8591 observations on the following
#' variables: \tabular{rlll}{
#' [, 1] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 3] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
#' [, 4] \tab source_value \tab (\var{numeric}) \tab hydrogen delta value [per thousand]\cr
#' [, 5] \tab year \tab (\var{numeric}) \tab Year of sampling\cr
#' [, 6] \tab month \tab (\var{numeric}) \tab Month of sampling\cr
#' [, 7] \tab source_ID \tab (\var{Factor}) \tab The unique identifier of the weather station\cr }
#' @seealso \code{\link{prepsources}} to prepare the dataset for the analyses and
#' to filter by time and location.
#' @references GNIP Project IAEA Global Network of Isotopes in Precipitation: \url{https://www.iaea.org}
#' 
#' Stumpp, C., Klaus, J., & Stichler, W. (2014). Analysis of long-term stable isotopic composition in German precipitation. Journal of hydrology, 517, 351-361.
#' 
#' Klaus, J., Chun, K. P., & Stumpp, C. (2015). Temporal trends in d18O composition of precipitation in Germany: insights from time series modelling and trend analysis. Hydrological Processes, 29(12), 2668-2680.
#' 
#' @source Data provided by the IAEA.
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
#' Europe (\code{GNIPDataEUagg}) and in the entire world
#' (\code{GNIPDataALLagg}). These data have been extracted from the
#' International Atomic Energy Agency IAEA in Vienna (GNIP Project: Global
#' Network of Isotopes in Precipitation) and processed by us using the function
#' \code{\link{prepsources}}. The data are aggregated per location. We no longer
#' provide the full non-aggregate GNIP dataset in the package as it is not free
#' to reuse. You can still download the complete GNIP dataset for free, but you
#' will have to proceed to a registration process with GNIP and use their
#' downloading interface WISER
#' (\url{http://www-naweb.iaea.org/napc/ih/IHS_resources_isohis.html}).
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
#' @noMd
#' @format The \var{dataframe}s include many observations on the following
#' variables: \tabular{rlll}{
#' [, 1] \tab lat \tab (\var{numeric}) \tab Latitude coordinate [decimal degrees]\cr
#' [, 2] \tab long \tab (\var{numeric}) \tab Longitude coordinate [decimal degrees]\cr
#' [, 3] \tab elev \tab (\var{numeric}) \tab Elevation asl [m]\cr
#' [, 4] \tab source_value \tab (\var{numeric}) \tab hydrogen delta value [per thousand]\cr
#' [, 5] \tab year \tab (\var{numeric}) \tab Year of sampling\cr
#' [, 6] \tab month \tab (\var{numeric}) \tab Month of sampling\cr
#' [, 7] \tab source_ID \tab (\var{Factor}) \tab The unique identifier of the weather station\cr }
#' @seealso \code{\link{GNIPDataDE}} for a non-aggregated dataset.
#' @references GNIP Project IAEA Global Network of Isotopes in Precipitation: \url{https://www.iaea.org}
#' @source Data provided by the IAEA and processed by us.
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
#' examples, we use the \code{isopalette1} for plotting the isoscape using
#' \code{\link{plot.ISOSCAPE}} and \code{isopalette2} for plotting the
#' assignment outcome using \code{\link{plot.ISOFIND}}.
#' 
#' Colour palettes can be created by using the function \code{\link{colorRamp}}
#' that interpolates colours between a set of given colours. One can also use
#' \code{\link{colorRampPalette}} to create functions providing colours. Also
#' interesting, the function \code{colorspace::choose_palette} offers a GUI
#' interface allowing to create and save a palette in a hexadecimal format
#' (which can later on be imported into R). This latter function is however
#' limited to a maximum of 50 colours. You can also use R colour palettes
#' already available such as \code{\link{terrain.colors}} or others available
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
#' @seealso \code{\link{rainbow}} for information about R colour palettes
#' 
#' \code{\link{colorRamp}} and \code{colorspace::choose_palette} to create your
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
#' pie(rep(1, length(isopalette1)), col = isopalette1,
#'     border = NA, labels = NA, clockwise = TRUE, main = "isopalette1")
#' pie(rep(1, length(isopalette2)), col = isopalette2,
#'     border = NA, labels = NA, clockwise = TRUE, main = "isopalette2")
#' pie(rep(1, 100), col = terrain.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "terrain.colors")
#' pie(rep(1, 100), col = rainbow(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "rainbow")
#' pie(rep(1, 100), col = topo.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "topo.colors")
#' pie(rep(1, 100), col = heat.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "heat.colors")
#' 
#' ## Creating your own colour palette
#' MyPalette <- colorRampPalette(c("blue", "green", "red"), bias = 0.7)
#' par(mfrow = c(1, 1))
#' pie(1:100, col = MyPalette(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "a home-made palette")
#' 
#' ## Turing palettes into functions for use in IsoriX
#' Isopalette1Fn <- colorRampPalette(isopalette1, bias = 0.5)
#' Isopalette2Fn <- colorRampPalette(isopalette2, bias = 0.5)
#' par(mfrow = c(1, 2))
#' pie(1:100, col = Isopalette1Fn(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "isopalette1")
#' pie(1:100, col = Isopalette2Fn(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "isopalette2")
#'
NULL
