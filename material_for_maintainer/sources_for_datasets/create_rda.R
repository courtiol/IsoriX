## This script allows for the creation of all the rda data stored in IsoriX
## I tried to not use IsoriX to create these objects unless that would imply to recode everything
##
## Note: the source files used by this script (e.g. the raw GNIP data, the
## calibration and assignment data) are not stored in the git repository, only
## this script is (the rest of the folder sources_for_datasets is ignored by git).

# rm(list = ls())

################################################################################
## Creating ElevRasterDE
  ElevRasterDE <- elevatr::get_elev_raster(location = data.frame(long = c(5.5, 15.5),
                                           lat = c(47, 55.5)),
                           prj = "+proj=longlat +datum=WGS84 +no_defs",
                           clip = "bbox",
                           z = 3)
  ElevRasterDE <- terra::rast(ElevRasterDE)
  print(ElevRasterDE)
  terra::plot(ElevRasterDE)
  #save(ElevRasterDE, file = "./saved_rda/ElevRasterDE.rda", compress = "xz")
  terra::writeRaster(ElevRasterDE, file = "material_for_maintainer/sources_for_datasets/saved_rda/ElevRasterDE.tif")
################################################################################

################################################################################
## Creating ElevRasterEU
elevationrasterbig <- raster::raster("gmted2010_30mn.tif")
elevationrasterEU <- raster::crop(elevationrasterbig, raster::extent(-30, 60, 30, 70))
ElevRasterEU <- raster::aggregate(elevationrasterEU, fact = 10)
print(ElevRasterEU)
raster::plot(ElevRasterEU)
save(ElevRasterEU, file = "sources_for_datasets/saved_rda/ElevRasterEU.rda", compress = "xz")
rm(list = c("elevationrasterbig", "elevationrasterEU"))
################################################################################
  
  
################################################################################  
## Creating PrecipBrickDE
  dir.create("sources_for_datasets/source_files/wc2.1_30s_prec")
  #download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip",
  #              destfile = "sources_for_datasets/source_files/wc2.1_30s_prec/wc2.1_30s_prec.zip", mode = "wb")
  #utils::unzip("sources_for_datasets/source_files/wc2.1_30s_prec/wc2.1_30s_prec.zip", exdir = "sources_for_datasets/source_files/wc2.1_30s_prec/wc2.1_30s_prec")
  list.tif <- list.files(path = "sources_for_datasets/source_files/wc2.1_30s_prec/wc2.1_30s_prec", pattern = "\\.tif$")
  for (month in 1:12) {
    print(paste0("Processing month ", month, "..."))
    tmp.raster <- raster::raster(paste0("sources_for_datasets/source_files/wc2.1_30s_prec/wc2.1_30s_prec/wc2.1_30s_prec_",
                                        formatC(month, digits = 0, width = 2, format = "f", flag = 0), ".tif"))
    assign(paste0("month_", month), raster::resample(x = tmp.raster, y = ElevRasterDE))
    rm(tmp.raster)
  }
  rm(list = c("month", "list.tif"))
  PrecipBrickDE <- raster::brick(mget(paste0("month_", 1:12)))
  rm(list = paste0("month_", 1:12))
  print(PrecipBrickDE)
  save(PrecipBrickDE, file = "sources_for_datasets/saved_rda/PrecipBrickDE.rda", compress = "xz")
  rm(PrecipBrickDE)
  rm(ElevRasterDE)
################################################################################

################################################################################
## Creating GNIPDataDE
  rawGNIP <- read.csv("./source_files/GNIP/2016-10-31 Extract_ISORIX.csv")
  ## ID free to use German stations:
  free.stations <- c("ARKONA", "ARTERN", "BAD SALZUFLEN", "BERLIN", "BRAUNSCHWEIG", "CUXHAVEN", "DRESDEN", "EMMERICH",
                     "FEHMARN", "GARMISCH-PARTENKIRCHEN", "GOERLITZ", "HOF-SAALE", "HOHENPEISSENBERG", "KAHLER ASTEN",
                     "KARLSRUHE", "KOBLENZ", "KONSTANZ", "NEUBRANDENBURG", "NORDERNEY", "PASSAU-FUERSTENZELL",
                     "REGENSBURG", "SCHLESWIG", "SEEHAUSEN", "STUTTGART", "TRIER", "WASSERKUPPE RHOEN",
                     "WEIL AM RHEIN", "WUERZBURG")
  
  ## We extract time information
  rawGNIP$year.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%Y"))
  rawGNIP$year.end <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%Y"))
  rawGNIP$year.span <- rawGNIP$year.begin - rawGNIP$year.end
  rawGNIP$month.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%m"))
  rawGNIP$month.end <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%m"))
  rawGNIP$day.span <- as.Date(rawGNIP$Begin.Of.Period) - as.Date(rawGNIP$End.of.Period)
  rawGNIP$Year <- as.numeric(format(as.Date(rawGNIP$Date), "%Y"))
  rawGNIP$Month <- as.numeric(format(as.Date(rawGNIP$Date), "%m"))
  
  ## We select only rows with what we need
  ## and for which the span of the water collection was between 25 and 35 days
  out.index <- is.na(rawGNIP$H2) |
    is.na(rawGNIP$Latitude) | is.na(rawGNIP$Longitude) |
    is.na(rawGNIP$day.span) |
    rawGNIP$day.span > -25 | rawGNIP$day.span < -35 |
    !(rawGNIP$Name.of.Site %in% free.stations)
  
  ## We only keep the data we need
  keep <- c("Name.of.Site", "WMO.Code", "Latitude", "Longitude", "Altitude",
            "Year", "Month", "H2")
  smallrawGNIP <- rawGNIP[!out.index, keep]
  #setdiff(free.stations, unique(smallrawGNIP$Name.of.Site)) ## No H2 data in HOF-SAALE!
  
  ## we turn the site into factors
  smallrawGNIP$WMO.Code <- as.factor(smallrawGNIP$WMO.Code)
  smallrawGNIP$Name.of.Site <- as.factor(smallrawGNIP$Name.of.Site)
  
  ## Export for IsoriX
  GNIPDataDE <- smallrawGNIP[, c("Name.of.Site", "Latitude", "Longitude", "Altitude", "Year", "Month", "H2")]
  colnames(GNIPDataDE) <- c("source_ID", "lat", "long", "elev", "year", "month", "source_value")
  rownames(GNIPDataDE) <- NULL
  print(head(GNIPDataDE))
  print(str(GNIPDataDE))
  print(dim(GNIPDataDE))
  save(GNIPDataDE, file = "./saved_rda/GNIPDataDE.rda", compress = "xz")
  rm(list = ls())
################################################################################

  
################################################################################
## Creating GNIPDataEUagg & GNIPDataALLagg
  rawGNIP <- read.csv("./source_files/GNIP/2016-10-31 Extract_ISORIX.csv")
  rawGNIP$year.begin  <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%Y"))
  rawGNIP$year.end    <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%Y"))
  rawGNIP$year.span   <- rawGNIP$year.begin - rawGNIP$year.end
  rawGNIP$month.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%m"))
  rawGNIP$month.end   <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%m"))
  rawGNIP$day.span    <- as.Date(rawGNIP$Begin.Of.Period) - as.Date(rawGNIP$End.of.Period)
  rawGNIP$Year        <- as.numeric(format(as.Date(rawGNIP$Date), "%Y"))
  rawGNIP$Month       <- as.numeric(format(as.Date(rawGNIP$Date), "%m"))
  rows_missing_info <- is.na(rawGNIP$H2) |
    is.na(rawGNIP$day.span) |
    rawGNIP$day.span > -25 |
    rawGNIP$day.span < -35
  columns_to_keep <- c("Name.of.Site", "Latitude", "Longitude", "Altitude",
                       "Year", "Month", "H2")
  GNIPData <- rawGNIP[!rows_missing_info, columns_to_keep]
  GNIPData$Name.of.Site <- as.factor(GNIPData$Name.of.Site)
  colnames(GNIPData) <- c("source_ID", "lat", "long", "elev", "year", "month", "source_value")
  print(str(GNIPData))
  GNIPDataALL <- GNIPData
  save(GNIPDataALL, file = "./saved_rda/GNIPDataALL_DONTSHARE.rda", compress = "xz")
  
  GNIPDataALLagg <- prepsources(data = GNIPData)
  save(GNIPDataALLagg, file = "./saved_rda/GNIPDataALLagg.rda", compress = "xz")
  print(str(GNIPDataALLagg))
  
  GNIPDataEUagg <- prepsources(data = GNIPData, long_min = -30, long_max = 60, lat_min = 30, lat_max = 70)
  save(GNIPDataEUagg, file = "./saved_rda/GNIPDataEUagg.rda", compress = "xz")
  print(str(GNIPDataEUagg))
  
  rm(list = ls())
################################################################################

  

################################################################################
## Creating CalibDataBat
  CalibDataBat <- read.table("./source_files/calibration/calibrationdata.nnoc.myiso.2017_final.txt", header = TRUE)
  colnames(CalibDataBat)[1] <- "site_ID"
  colnames(CalibDataBat)[5] <- "sample_ID"
  colnames(CalibDataBat)[7] <- "sample_value"
  CalibDataBat$species <- as.factor(CalibDataBat$species)
  CalibDataBat$site_ID <- factor(paste0("site_", as.numeric(as.factor(paste(CalibDataBat$long, CalibDataBat$lat, CalibDataBat$elev, sep = "_")))))

  print(head(CalibDataBat))
  print(str(CalibDataBat))
  save(CalibDataBat, file = "./saved_rda/CalibDataBat.rda", compress = "xz")
  CalibDataBatRev <- CalibDataBat
  CalibDataBatRev$sample_value <- 29.222 + 0.8388*CalibDataBat$sample_value
  save(CalibDataBatRev, file = "./saved_rda/CalibDataBatRev.rda", compress = "xz")
  rm(CalibDataBat)
  rm(CalibDataBatRev)
################################################################################
  
  
################################################################################
## Creating CalibDataBat2
  calib.raw <- read.csv2("./source_files/calibration/Voigt2014_BiodivCons_Sedent_APLNNO.csv", sep = "\t")
  calib.raw$long <- as.numeric(as.character(calib.raw$Lon))
  calib.raw$lat <- as.numeric(as.character(calib.raw$Lat))
  calib.raw$elev <- calib.raw$Elev_Feet ## some measure are in meters (for Noctules)
  calib.raw <- calib.raw[calib.raw$ID_Species != "Nno", ]  ## We remove Noctules as they are a bit aquatic which biases isotopes

  # elev_obj <- elevatr::get_elev_point(as.data.frame(cbind(x = calib.raw$long, y = calib.raw$lat)), prj = 4326, src = "aws", z = 10, ncpu = 50)
  # plot(foo, calib.raw$elev)
  # abline(0, 1)
  # abline(0, 1/0.3048) # feet line
  
  calib.raw$sample_ID <- factor(paste(calib.raw$ID_Species, calib.raw$ID_Indiv, sep = "_"))
  calib.raw$sample_value <- as.numeric(as.character(calib.raw$dDH))
  calib.raw$species <- factor(calib.raw$ID_Species)
  levels(calib.raw$species) <- c("Barbastella_barbastellus", "Eptesicus_isabellinus",
                                 "Eptesicus_serotinus", "Plecotus_auritus", "Plecotus_austriacus")
  
  calib.raw$site_ID <- factor(paste(calib.raw$lat, calib.raw$long, sep = "_"))
  levels(calib.raw$site_ID) <- paste("site", 1:length(levels(calib.raw$site_ID)), sep = "_")
  calib <- calib.raw[, colnames(calib.raw) %in% c("site_ID", "lat", "long", "elev", "sample_value", "sample_ID")]
  rm(calib.raw)
  CalibDataBat2 <- droplevels(calib[order(calib$site_ID), ])
  rm(calib)
  CalibDataBat2 <- CalibDataBat2[, c(6, 1, 2, 3, 4, 5)]
  row.names(CalibDataBat2) <- NULL
  colnames(CalibDataBat2)[6] <- c("sample_value")

  print(head(CalibDataBat2))
  print(str(CalibDataBat2))
  print(dim(CalibDataBat2))
  save(CalibDataBat2, file = "./saved_rda/CalibDataBat2.rda", compress = "xz")
  CalibDataBat2Rev <- CalibDataBat2
  CalibDataBat2Rev$sample_value <- 29.222 + 0.8388*CalibDataBat2$sample_value
  save(CalibDataBat2Rev, file = "./saved_rda/CalibDataBat2Rev.rda", compress = "xz")
  rm(CalibDataBat2)
  rm(CalibDataBat2Rev)
################################################################################

################################################################################
## Creating AssignDataBat
  AssignDataBat <- read.csv(file = "./source_files/assignment/bat.fatalities.to.assign.2.csv")
  AssignDataBat <- AssignDataBat[1:14, c(1, 6:7, 11)]
  AssignDataBat[, 1] <- factor(paste0("Nnoc_", AssignDataBat[, 1]))
  colnames(AssignDataBat) <- c("sample_ID", "lat", "long", "sample_value")
  print(AssignDataBat)
  save(AssignDataBat, file = "./saved_rda/AssignDataBat.rda", compress = "xz")
  AssignDataBatRev <- AssignDataBat
  AssignDataBatRev$sample_value <- 29.222 + 0.8388*AssignDataBatRev$sample_value
  save(AssignDataBatRev, file = "./saved_rda/AssignDataBatRev.rda", compress = "xz")

  rm(AssignDataBat)
  rm(AssignDataBatRev)
################################################################################
  
  
################################################################################
## Creating AssignDataBat2
  assign.raw <- read.csv("./source_files/assignment/raw_data_freeranging_bats_simple.csv")
  assign.raw <- assign.raw[assign.raw$group == 1, ]
  assign.raw$sample_value <- assign.raw$DH
  assign.raw$group <- assign.raw$DH <- NULL
  sp <- lapply(strsplit(as.character(assign.raw$species), "_"), function(i) {
    paste(substr(i[1], 1, 1), substr(i[2], 1, 2), sep = "")
  })
  
  assign.raw$sample_ID <- factor(paste(sp, 1:nrow(assign.raw), sep = "_"))
  rm(sp)
  AssignDataBat2 <- droplevels(assign.raw[, c("sample_ID", "species", "sample_value")])
  rm(assign.raw)
  row.names(AssignDataBat2) <- NULL
  colnames(AssignDataBat2)[3] <- "sample_value"
  print(head(AssignDataBat2))
  print(str(AssignDataBat2))
  print(dim(AssignDataBat2))
  save(AssignDataBat2, file = "./saved_rda/AssignDataBat2.rda", compress = "xz")
  AssignDataBat2Rev <- AssignDataBat2
  AssignDataBat2Rev$sample_value <- 29.222 + 0.8388*AssignDataBat2Rev$sample_value
  save(AssignDataBat2Rev, file = "./saved_rda/AssignDataBat2Rev.rda", compress = "xz")
  rm(AssignDataBat2)
  rm(AssignDataBat2Rev)
################################################################################

################################################################################
## Creating CountryBorders
## (from the current Natural Earth data; we used rnaturalearth 1.2.0 and
## rnaturalearthdata 1.0.0; the column "name" allows for selecting countries)
  CountryBorders_terra <- terra::vect(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[, "name"])
  print(CountryBorders_terra)
  terra::plot(CountryBorders_terra)
  terra::saveRDS(CountryBorders_terra, file = "inst/extdata/CountryBorders.rds", compress = "xz")
################################################################################

################################################################################
## Creating OceanMask
## (the extent of CountryBorders minus the countries)
  worldlimit <- terra::as.polygons(terra::ext(CountryBorders_terra))
  terra::crs(worldlimit) <- terra::crs(CountryBorders_terra)
  OceanMask_terra <- worldlimit - CountryBorders_terra
  print(OceanMask_terra)
  terra::plot(OceanMask_terra, col = "blue")
  terra::saveRDS(OceanMask_terra, file = "inst/extdata/OceanMask.rds", compress = "xz")
  rm(list = c("worldlimit", "CountryBorders_terra", "OceanMask_terra"))
################################################################################

################################################################################
## Creating isopalette1
  isopalette1 <- c("#006198", "#006499", "#00679A", "#006A9C", "#006D9E", "#0071A0", "#0075A2", "#0079A5", "#007DA7", "#0081AA", "#0086AD", "#008AB0", "#008FB3", "#0094B7", "#3099BA", "#479FBE", "#59A5C1", "#6AABC5", "#79B1C9", "#88B8CE", "#97BFD2", "#A6C7D7", "#B6CFDC", "#C8D9E2", "#DEE6EA", "#EBE3E0", "#E5D3CC", "#E0C6BC", "#DBBCAE", "#D6B2A1", "#D1A895", "#CDA08A", "#C8977F", "#C49074", "#C0886A", "#BB815F", "#B77A55", "#B3734A", "#AF6D3F", "#AB6633", "#A66025", "#A25A11", "#9E5400", "#9A4E00", "#964900", "#924300", "#8D3D00", "#893800", "#853300", "#812D00")
  pie(1:length(isopalette1), col = isopalette1, border = NA, labels = NA)
  save(isopalette1, file = "material_for_maintainer/sources_for_datasets/saved_rda/isopalette1.rda", compress = "xz")
  rm(isopalette1)
################################################################################
  
################################################################################
## Creating isopalette2
  isopalette2 <- c("#FFFFFF", rev(terrain.colors(1000)))
  pie(1:length(isopalette2), col = isopalette2, border = NA, labels = NA)
  save(isopalette2, file = "material_for_maintainer/sources_for_datasets/saved_rda/isopalette2.rda", compress = "xz")
  rm(isopalette2)
################################################################################

################################################################################
## Creating CalibDataAliens
  # library(IsoriX)
  # GNIPDataDEagg <- prepsources(data = GNIPDataDE)
  # GermanFit <- isofit(iso.data = GNIPDataDEagg)
  # isoscape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
  # ## We create a simulated dataset with 50 site and 10 observations per site
  # set.seed(2L)
  # CalibDataAlien <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
  #                                 isoscape = isoscape,
  #                                 elevation_raster = ElevRasterDE,
  #                                 n_sites = 50,
  #                                 min_n_samples = 10,
  #                                 max_n_samples = 10)
  # CalibDataAlien$env.value <- NULL
  # save(CalibDataAlien, file = "./saved_rda/CalibDataAlien.rda", compress = "xz")
  ##write.csv(IsoriX::CalibDataAlien, file = "./source_files/calibration/CalibDataAliens.csv", row.names = FALSE)
  CalibDataAlien <- read.csv("material_for_maintainer/sources_for_datasets/source_files/calibration/CalibDataAliens.csv")
  colnames(CalibDataAlien)[c(1, 5:6)] <- c("site_ID", "sample_ID", "sample_value")
  print(head(CalibDataAlien))
  print(str(CalibDataAlien))
  print(dim(CalibDataAlien))
  save(CalibDataAlien, file = "material_for_maintainer/sources_for_datasets/saved_rda/CalibDataAlien.rda", compress = "xz")
  rm(CalibDataAlien)
################################################################################
  
################################################################################
## Creating AssignDataAlien
  ##write.csv(IsoriX::AssignDataAlien, file = "./source_files/assignment/AssignDataAlien.csv", row.names = FALSE)
  # AssignDataAlien <- read.csv("./source_files/assignment/AssignDataAlien.csv")
  # colnames(AssignDataAlien) <- c("sample_ID", "sample_value")
  # print(head(AssignDataAlien))
  # print(str(AssignDataAlien))
  # print(dim(AssignDataAlien))

  GNIPDataDEagg <- prepsources(data = GNIPDataDE)
  GermanFit <- isofit(data = GNIPDataDEagg)
  GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
  
  set.seed(1L)
  Aliens2 <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5, resid_var = 5),
                           isoscape = GermanScape,
                           coordinates = data.frame(site_ID = c("Berlin", "Bielefeld"),
                                                    long   = c(13.52134, 8.49914),
                                                    lat    = c(52.50598, 52.03485)),
                           raster = ElevRasterDE,
                           n_sites = 25,
                           min_n_samples = 5,
                           max_n_samples = 5)
  AssignDataAlien <- Aliens2[, c("sample_ID", "long", "lat", "sample_value")]
  save(AssignDataAlien, file = "material_for_maintainer/sources_for_datasets/saved_rda/AssignDataAlien.rda", compress = "xz")
  rm(AssignDataAlien)
################################################################################
