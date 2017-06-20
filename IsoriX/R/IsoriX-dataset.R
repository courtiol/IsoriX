#' Assignment dataset for bat species
#' 
#' This dataset contains data from Voigt, Lehmann and Greif (2015). It contains
#' deuterium delta values of fur keratin from bats captured in 2008, 2009 and
#' 2013 from their roosting sites in Bulgaria. We only retained the bats of the
#' genus Myotis from the original study. The data can be used as an example to
#' perform assignments using the function \code{\link{isofind}}.
#' 
#' 
#' @name AssignDataBat
#' @docType data
#' @format A \var{dataframe} with 244 observations on 3 variables:
#' \tabular{rlll}{ [, 1] \tab animalID \tab (\var{Factor}) \tab Identification
#' of the animal\cr [, 2] \tab species \tab (\var{Factor}) \tab Animal species
#' name\cr [, 3] \tab tissue.value \tab (\var{numeric}) \tab Deuterium delta
#' value of the tissue\cr }
#' @seealso \code{\link{isofind}} to perform assignments
#' @references Voigt, C.C., Lehmann, D., Greif, S. (2015). Stable isotope
#' ratios of hydrogen separate mammals of aquatic and terrestrial food webs.
#' Methods in Ecology and Evolution 6(11).
#' @source data directly provided by the authors of the following publication
#' @keywords datasets
#' @examples
#' 
#' head(AssignDataBat)
#' str(AssignDataBat)
#' 
NULL





#' Calibration dataset for bat species
#' 
#' This dataset contains deuterium delta values of fur keratin from sedentary
#' bat species captured between 2005 and 2009 from Popa-Lisseanu et al. (2012).
#' The data can be used as an example to fit a calibration model using the
#' function \code{\link{calibfit}}.
#' 
#' Users who wish to use their own dataset for calibration should create a
#' \var{dataframe} of similar structure than this one (only the column
#' 'species' can be dropped). The columns should possess the same names as the
#' ones described above. If the elevation is unknown at the sampling sites,
#' elevation information can be extracted from a high resolution elevation
#' raster using the function \pkg{\link[raster]{extract}}. In this dataset, we
#' retrieved elevations from the Global Multi-resolution Terrain Elevation Data
#' 2010. Note that the original study used a different source of elevation
#' data.
#' 
#' @name CalibDataBat
#' @docType data
#' @format A \var{dataframe} with 178 observations on 7 variables:
#' \tabular{rlll}{ [, 1] \tab siteID \tab (\var{Factor}) \tab Identification of
#' the sampling site\cr [, 2] \tab long \tab (\var{numeric}) \tab Longitude
#' coordinate [decimal degrees]\cr [, 3] \tab lat \tab (\var{numeric}) \tab
#' Latitude coordinate [decimal degrees]\cr [, 4] \tab elev \tab
#' (\var{numeric}) \tab Elevation asl [m]\cr [, 5] \tab animalID \tab
#' (\var{Factor}) \tab Identification of the sampled animal\cr [, 6] \tab
#' species \tab (\var{Factor}) \tab Species name \cr [, 7] \tab tissue.value
#' \tab (\var{numeric}) \tab Deuterium delta value of the tissue\cr }
#' @seealso \code{\link{calibfit}} to fit a calibration model
#' @references Popa-Lisseanu, A. G., Soergel, K., Luckner, A., Wassenaar, L.
#' I., Ibanez, C., Kramer-Schadt, S., Ciechanowski, M., Goerfoel, T., Niermann,
#' I., Beuneux, G., Myslajek, R. W., Juste, J., Fonderflick, J., Kelm, D.,
#' Voigt, C. C. (2012). A triple isotope approach to predict the breeding
#' origins of European bats. PLoS ONE 7(1):e30388.
#' @source data directly provided by the authors of the following publication
#' @keywords datasets
#' @examples
#' 
#' head(CalibDataBat)
#' str(CalibDataBat)
#' 
#' ## The following example require to have downloaded
#' ## a large elevation raster with the function getelev()
#' ## and will therefore not run unless you type:
#' ## example(CalibDataBat, run.dontrun=TRUE)
#' 
#' \dontrun{
#' if(require(raster)){
#'     ### DELETE AND RECREATE ELEVATION DATA
#'     CalibDataBat$elev <- NULL  ## we delete them
#' 
#'     ## we reconstruct the elevation data using an elevation raster:
#'     ## (see ?getelev for details on how to get the tif file)
#'     elevationrasterbig <- raster("gmted2010_30mn.tif")
#'     CalibDataBat$elev <- extract(
#'         elevationrasterbig,
#'         cbind(CalibDataBat$long, CalibDataBat$lat))
#'     head(CalibDataBat)
#' }
#' 
#' }
#' 
#' 
NULL





#' Simulated assignment dataset
#' 
#' This dataset contains simulated deuterium delta values. 
#' The data can be used as an example to perform assignments using the function \code{\link{isofind}}.
#' 
#' @name AssignDataAlien
#' @docType data
#' @format A \var{dataframe} with x observations on 3 variables:
#' \tabular{rlll}{ [, 1] \tab animalID \tab (\var{Factor}) \tab Identification
#' of the animal\cr [, 2] \tab species \tab (\var{Factor}) \tab Animal species
#' name\cr [, 3] \tab tissue.value \tab (\var{numeric}) \tab Deuterium delta
#' value of the tissue\cr }
#' @seealso \code{\link{isofind}} to perform assignments
#' @keywords datasets
#' @examples
#' 
#' head(AssignDataAlien)
#' str(AssignDataAlien)
#' 
NULL





#' Simulated calibration dataset
#' 
#' This dataset contains simulated deuterium delta values for corresponding locations
#' based on an assumed linear relationship between the animal tissue value and the 
#' deuterium delta values in the environment.
#' The data can be used as an example to fit a calibration model using the
#' function \code{\link{calibfit}}.
#' 
#' Users who wish to use their own dataset for calibration should create a
#' \var{dataframe} of similar structure than this one. The columns should possess 
#' the same names as the ones described above. If the elevation is unknown at the 
#' sampling sites, elevation information can be extracted from a high resolution elevation
#' raster using the function \pkg{\link[raster]{extract}}. In this dataset, we
#' retrieved elevations from the Global Multi-resolution Terrain Elevation Data
#' 2010.
#' 
#' @name CalibDataAlien
#' @docType data
#' @format A \var{dataframe} with x observations on 6 variables:
#' \tabular{rlll}{ [, 1] \tab siteID \tab (\var{Factor}) \tab Identification of
#' the sampling site\cr [, 2] \tab long \tab (\var{numeric}) \tab Longitude
#' coordinate [decimal degrees]\cr [, 3] \tab lat \tab (\var{numeric}) \tab
#' Latitude coordinate [decimal degrees]\cr [, 4] \tab elev \tab
#' (\var{numeric}) \tab Elevation asl [m]\cr [, 5] \tab animalID \tab
#' (\var{Factor}) \tab Identification of the sampled animal\cr [, 6] \tab tissue.value
#' \tab (\var{numeric}) \tab Deuterium delta value of the tissue\cr }
#' @seealso \code{\link{calibfit}} to fit a calibration model
#' @keywords datasets
#' @examples
#' 
#' head(CalibDataAlien)
#' str(CalibDataAlien)
#' 
#' ## The following example require to have downloaded
#' ## a large elevation raster with the function getelev()
#' ## and will therefore not run unless you type:
#' ## example(CalibDataAlien, run.dontrun=TRUE)
#' 
#' \dontrun{
#' if(require(raster)){
#'     ### DELETE AND RECREATE ELEVATION DATA
#'     CalibDataAlien$elev <- NULL  ## we delete them
#' 
#'     ## we reconstruct the elevation data using an elevation raster:
#'     ## (see ?getelev for details on how to get the tif file)
#'     elevationrasterbig <- raster("gmted2010_30mn.tif")
#'     CalibDataAlien$elev <- extract(
#'         elevationrasterbig,
#'         cbind(CalibDataAlien$long, CalibDataAlien$lat))
#'     head(CalibDataAlien)
#' }
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
#' if(require(sp))
#'     plot(OceanMask, col='blue')
#' 
#' ## HOW DID WE CREATE THIS FILE?
#' ## (This example takes some time and will therefore not be run
#' ##  unless you type: example(OceanMask, run.dontrun=TRUE) )
#'   
#' \dontrun{
#' if(require(raster) & require(rgeos)){
#'     worldlimit <- as(extent(CountryBorders), "SpatialPolygons")
#'     proj4string(worldlimit) <- crs(CountryBorders)
#'     OceanMask <- gDifference(worldlimit, CountryBorders)  
#'     OceanMask
#'     ## uncomment the following to store the file:
#'     #save(OceanMask, file = "OceanMask.rda", compress = "xz")
#' }
#' }
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
#' 
#' ### HOW DID WE CREATE THIS FILE?
#' ### (This example takes some time and will therefore not be run
#' ###  unless you type: example(CountryBorders, run.dontrun = TRUE) )
#' 
#' \dontrun{
#' if(require(maps) & require(maptools) & require(raster) & require(rgeos)){
#'     worldmap <- map("world", fill = TRUE, plot = FALSE)
#'     CountryBorders <- map2SpatialPolygons(worldmap, IDs = worldmap$names)
#'     CountryBorders <- gBuffer(CountryBorders, byid = TRUE, width = 0)
#'     proj4string(CountryBorders) <- CRS("+ proj = longlat + datum = WGS84")
#'     CountryBorders
#'     ## uncomment the following to store the file:
#'     #save(CountryBorders, file = "CountryBorders.rda", compress = "xz")
#' }
#' }
#' 
#' 
NULL



#' The raster of world elevation
#' 
#' This raster contains the elevation of the world surface [meters above sea
#' level] with a resolution of approximately 100 square-km.
#' 
#' This raster contains elevation data of the world in a highly aggregated form
#' corresponding to a resolution of approximately one elevation value per 100
#' square-km. This is only for the purpose of having a small and easy-to-handle
#' file to practice, but it should not be used to perform real assignments!
#' 
#' In the example below, we show how we generated this small raster from a
#' large original \var{DEM} (digital elevation model). The original raster has
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
#' \cr \url{http://topotools.cr.usgs.gov/gmted_viewer/} \cr and converted it
#' into a \var{tif} file. Because the original file is very large, we directly
#' provide the url link of the \var{tif} file in the example below.
#' 
#' @name ElevRaster
#' @docType data
#' @format A \var{RasterLayer}
#' @seealso \code{\link{relevate}} to crop and/or aggregate the elevation
#' raster
#' @source \url{http://topotools.cr.usgs.gov/gmted_viewer/}
#' @keywords datasets
#' @examples
#' 
#' 
#' ## The following example require to have downloaded
#' ## a large elevation raster with the function getelev()
#' ## and will therefore not run unless you type:
#' ## example(ElevRaster, run.dontrun=TRUE)
#' 
#' \dontrun{
#' ### CREATING THE OBJECT ElevRaster
#' 
#' ## download the tif file (ca. 700 Mb):
#' ## (see ?getelev for details on how to get the tif file)
#' # getelev()
#' 
#' ## convert the tif into R raster format:
#' library(raster)
#' elevationrasterbig <- raster("gmted2010_30mn.tif")
#' 
#' ## create the highly agregated elevation raster
#' ## (90 sec on one of our computers):
#' ElevRaster <- relevate(
#'     elevationrasterbig,
#'     aggregation.factor = 100)
#' }
#' 
#' 
NULL





#' The fitted isoscape model for Germany
#' 
#' This dataset contains an object of class \code{isofit} containing the mean
#' model and residual dispersion model fitted on the data for Germany (see
#' example).
#' 
#' This fitted isoscape model has been obtained by running the function
#' \code{\link{isofit}} on the isotopic source data stored in GNIPDataDE (see
#' example).
#' 
#' @name GermanFit
#' @docType data
#' @format An object of class \code{isofit}
#' @seealso \code{\link{isofit}} for information about how to fit an isoscape
#' model
#' @keywords datasets models
#' @examples
#' 
#' GermanFit
#' plot(GermanFit)
#' 
#' ## The following example takes a lot of time and will therefore not
#' ## be run unless you type: example(GermanFit, run.dontrun=TRUE)
#' 
#' \dontrun{
#' ## We prepare the data using the function \code{\link{queryGNIP}}
#' GNIPDataDEagg <- queryGNIP(data = GNIPDataDE)
#' 
#' ## We fit the isoscape model
#' GermanFit <- isofit(iso.data = GNIPDataDEagg,
#'                     mean.model.fix = list(elev = TRUE, lat.abs = TRUE),
#'                     mean.model.rand = list("uncorr" = TRUE),
#'                     disp.model.rand = list("uncorr" = TRUE)
#'                     )
#' 
#' ## We created the object GermanFit stored in this package using
#' ## save(GermanFit, file = "GermanFit.rda", compress = "xz")
#' 
#' GermanFit
#' plot(GermanFit)
#' }
#' 
#' 
NULL





#' Weather station data for Germany
#' 
#' This dataset contains the mean and variance of Deuterium delta precipitation
#' values of weather stations sampled between 1960 and 2015. Data have been
#' compiled by the International Atomic Energy Agency IAEA in Vienna (GNIP
#' Project: Global Network of Isotopes in Precipitation).
#' 
#' The dataset contains non-aggregated data for 27 weather stations across Germany.
#' 
#' This dataset is the raw data source and should not be directly used for
#' fitting isoscapes.
#' 
#' Please use \code{\link{queryGNIP}} to filter the dataset by time and
#' location.
#' 
#' If you want to use your own dataset, you must format your data as those
#' produced by the function \code{\link{queryGNIP}}.
#' 
#' @name GNIPDataDE
#' @docType data
#' @format The \var{dataframe} includes 8591 observations on the following
#' variables: \tabular{rlll}{ [, 1] \tab lat \tab (\var{numeric}) \tab Latitude
#' coordinate [decimal degrees]\cr [, 2] \tab long \tab (\var{numeric}) \tab
#' Longitude coordinate [decimal degrees]\cr [, 3] \tab elev \tab
#' (\var{numeric}) \tab Elevation asl [m]\cr [, 4] \tab isoscape.value \tab
#' (\var{numeric}) \tab Deuterium stable hydrogen delta value [per thousand]\cr
#' [, 5] \tab year \tab (\var{numeric}) \tab Year of sampling\cr [, 6] \tab
#' month \tab (\var{numeric}) \tab Month of sampling\cr [, 7] \tab stationID
#' \tab (\var{Factor}) \tab The unique identifier of the weather station\cr }
#' @seealso \code{\link{queryGNIP}} to prepare the dataset for the analyses and
#' to filter by time and location.
#' @references GNIP Project IAEA Global Network of Isotopes in Precipitation:
#' 
#' \url{http://www.iaea.org}
#' @source Data extracted from IAEA (see below for more details).
#' @keywords datasets
#' @examples
#' 
#' head(GNIPDataDE)
#' 
NULL





#' Colour palettes for plotting
#' 
#' These datasets contain colour vectors that can be used for plotting. In our
#' examples, we use the \code{isopalette1} for plotting the isoscape using
#' \code{\link{plot.isoscape}} and \code{isopalette2} for plotting the
#' assignment outcoume using \code{\link{plot.isorix}}.
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
#' ### A COMPARISON OF SOME COLOUR PALETTES
#' 
#' par(mfrow = c(2, 3))
#' pie(rep(1, length(isopalette1)), col = isopalette1,
#' 	border = NA, labels = NA, clockwise = TRUE, main = "isopalette1")
#' pie(rep(1, length(isopalette2)), col = isopalette2,
#' 	border = NA, labels = NA, clockwise = TRUE, main = "isopalette2")
#' pie(rep(1, 100), col = terrain.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "terrain.colors")
#' pie(rep(1, 100), col = rainbow(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "rainbow")
#' pie(rep(1, 100), col = topo.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "topo.colors")
#' pie(rep(1, 100), col = heat.colors(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "heat.colors")
#' 
#' ### CREATING YOUR OWN COLOUR PALETTE
#' my.palette  <- colorRampPalette(c("blue", "green", "red"), bias = 0.7)
#' par(mfrow = c(1, 1))
#' pie(1:100, col = my.palette(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "a home-made palette")
#' 
#' ### TURNING PALETTES INTO FUNCTIONS (FOR USE IN ISORIX)
#' Isopalette1Fn <- colorRampPalette(isopalette1, bias = 0.5)
#' Isopalette2Fn <- colorRampPalette(isopalette2, bias = 0.5)
#' par(mfrow = c(1, 2))
#' pie(1:100, col = Isopalette1Fn(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "isopalette1")
#' pie(1:100, col = Isopalette2Fn(100), border = NA, labels = NA,
#'     clockwise = TRUE, main = "isopalette2")
#'
NULL
