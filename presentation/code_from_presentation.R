########################################################################
#                                                                      #
# This is a dump of all the R code from the presentation about IsoriX  #
#                                                                      #
#        It is not commented as it comes with the slides               #
#                                                                      #
########################################################################


library(IsoriX)  ## to load

files <- dir(paste0(system.file(package = "IsoriX"), "/R/"))
filenames_R <- paste0(system.file(package = "IsoriX"), "/R/", files)
linesofcode <- sum(sapply(filenames_R, function(file) R.utils::countLines(file)))

options(repos = "https://cran.rstudio.com/")

tools::package_dependencies("IsoriX") ## note: list dependencies, not suggested packages

setdiff(ls("package:IsoriX"), data(package = "IsoriX")$results[, "Item"])  ## note: does not show methods and unexported functions

data(package = "IsoriX")$results[, c("Item", "Title")]

## help(package = 'IsoriX')

head(GNIPDataDE)
tail(GNIPDataDE)

## prepsources(
##   data,
##   month = 1:12, year,
##   long_min, long_max, lat_min, lat_max,
##   split_by = NULL,
##   prop_random = 0, random_level = "source",
##   col_source_value = "source_value",
##   col_source_ID = "source_ID", col_lat = "lat", col_long = "long",
##   col_elev = "elev", col_month = "month", col_year = "year"
##   )

GNIPDataDEagg <- prepsources(data = GNIPDataDE)

head(GNIPDataDEagg)

library(sp) ## for plotting polygons

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "Mean")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = round(GNIPDataDEagg$mean_source_value, digits = 1), col = "blue", cex = 1.5, font = 2)

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "SD")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = round(sqrt(GNIPDataDEagg$var_source_value), digits = 1), col = "blue", cex = 1.5, font = 2)

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "N")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = GNIPDataDEagg$n_source_value, col = "blue", cex = 1.5, font = 2)

## isofit(
##   data,
##   mean_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
##   disp_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE, long_2 = FALSE),
##   mean_model_rand = list(uncorr = TRUE, spatial = TRUE),
##   disp_model_rand = list(uncorr = TRUE, spatial = TRUE),
##   uncorr_terms = list(mean_model = "lambda", disp_model = "lambda"),
##   spaMM_method = list(mean_model = "fitme", disp_model = "fitme"),
##   dist_method = "Earth",
##   control_mean = list(),
##   control_disp = list(),
##   verbose = interactive()
##   )

GermanFit <- isofit(data = GNIPDataDEagg,
                    mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

names(GermanFit)

GermanFit$mean_fit

plot(GermanFit)

GermanFit2 <- isofit(data = GNIPDataDEagg,
                     mean_model_fix = list(elev = TRUE, lat_abs = FALSE))

## AIC(GermanFit$mean_fit)

print(AIC(GermanFit$mean_fit))

## AIC(GermanFit2$mean_fit)

print(AIC(GermanFit2$mean_fit))

## getelev(
##   file = "~/elevation_world_z5.tif",
##   z = 5,
##   long_min = -180, long_max = 180, lat_min = -90, lat_max = 90,
##   margin_pct = 5,
##   override_size_check = FALSE,
##   overwrite = FALSE,
##   Ncpu = getOption_IsoriX("Ncpu"),
##   verbose = interactive(),
##   ...
##   )

## getelev(file = "elevation_DE_z5.tif",
##         long_min = 5.86, long_max = 15.02, lat_min = 47.23, lat_max = 54.90)
if (!file.exists("elevation_DE_z5.tif")) {
  getelev(file = "elevation_DE_z5.tif",
          long_min = raster::xmin(CountryBorders['Germany']), long_max = raster::xmax(CountryBorders['Germany']),
          lat_min = raster::ymin(CountryBorders['Germany']), lat_max = raster::ymax(CountryBorders['Germany']))
}

## getelev()

ElevDE <- raster("elevation_DE_z5.tif")  ## turn the tif into raster
ElevDE

## prepraster(
##   raster,
##   isofit = NULL,
##   margin_pct = 5,
##   aggregation_factor = 0L,
##   aggregation_fn = mean,
##   manual_crop = NULL,
##   values_to_zero = c(-Inf, 0),
##   verbose = interactive()
##   )

ElevRasterDE <- prepraster(ElevDE, isofit = GermanFit, aggregation_factor = 2)

plot(ElevRasterDE)

levelplot(ElevRasterDE, margin = FALSE) +
  layer(sp.polygons(CountryBorders, col = "white")) +
  layer(sp.polygons(OceanMask, fill = "cyan")) +
  xyplot(GNIPDataDEagg$lat ~ GNIPDataDEagg$long, col = "pink")

## isoscape(
##   raster,
##   isofit,
##   verbose = interactive()
##   )

GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
names(GermanScape)

GermanScape$isoscapes

## plot.ISOSCAPE(
##   x,
##   which   = "mean",
##   y_title = list(which = TRUE, title = bquote(delta^2 * H)),
##   sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
##   borders = list(borders = NA, lwd = 0.5, col = "black"),
##   mask    = list(mask = NA, lwd = 0, col = "black", fill = "black"),
##   palette = list(step = NA, range = c(NA, NA), n_labels = 11, digits = 2, fn = NA),
##   plot    = TRUE,
##   sphere  = list(build = FALSE, keep_image = FALSE),
##   ...
##   )

plot(GermanScape)

plot(GermanScape,
     which = "mean_predVar")

plot(GermanScape,
     which = "mean_residVar")

plot(GermanScape,
     palette = list(fn = rainbow))

plot(GermanScape,
     palette = list(fn = NULL))

plot(GermanScape,
     palette = list(fn = NULL, step = 1))

plot(GermanScape,
     palette = list(fn = NULL, step = 10, range = c(-110, -40)))

plot(GermanScape,
     palette = list(fn = NULL, step = 10, range = c(-110, -40), n_labels = 3))

plot(GermanScape,
     sources = list(draw = TRUE, cex = 2, pch = 5, lwd = 2, col = "white"))

plot(GermanScape,
     mask = list(fill = "orange"))

plot(GermanScape,
     mask = list(mask = NULL))

plot(GermanScape,
mask = list(mask = rbind(OceanMask, CountryBorders[names(CountryBorders) != "Germany"]), fill = "white"))

library(raster)

writeRaster(GermanScape$isoscapes$mean,
            filename = "GermanScape.asc",
            format = "ascii",
            overwrite = TRUE,
            NAflag = -9999)

writeRaster(GermanScape$isoscapes$mean,
            filename = "GermanScape.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
detach(package:raster)

head(CalibDataBat)

str(CalibDataBat)

plot(CountryBorders, xlim = range(CalibDataBat$long), ylim = range(CalibDataBat$lat))
plot(CountryBorders["Germany"], col = "orange", add = TRUE)
plot(SpatialPoints(coords = CalibDataBat[, c("long", "lat")]), col = "blue", pch = 20, add = TRUE)

IsoPredicted <- values(GermanScape$isoscapes$mean)
IsoAtBats <- extract(GermanScape$isoscapes$mean, SpatialPoints(coords = CalibDataBat[, c("long", "lat")]))
hist(IsoPredicted, nclass = 1000, main = "", xlab = "Deuterium")
rug(IsoAtBats, col = "red", lwd = 2)
legend("topleft", fill = c("red", "black"), legend = c("sampled locations", "predicted locations"))

IsoPredictedPredVar <- values(GermanScape$isoscapes$mean_predVar)
plot(x = IsoPredicted, y = IsoPredictedPredVar, xlab = "Predicted mean", ylab = "PredVar")
rug(IsoAtBats, col = "red", lwd = 2)

plot(GermanScape, plot = FALSE) +
  xyplot(CalibDataBat$lat ~ CalibDataBat$long,
         pch = 13, col = "white", cex = 2, lwd = 2, panel = panel.points)

## calibfit(
##   data,
##   isofit = NULL,
##   method = c("wild", "lab", "desk", "desk_inverse"),
##   verbose = interactive(),
##   control_optim = list()
##   )

CalibBats <- calibfit(data = CalibDataBat, isofit = GermanFit)

CalibBats

plot(CalibBats)

AssignDataBat

## isofind(
##   data,
##   isoscape,
##   calibfit = NULL,
##   mask = NA,
##   neglect_covPredCalib = TRUE,
##   verbose = interactive()
##   )

AssignedBats <- isofind(data = AssignDataBat, isoscape = GermanScape, calibfit = CalibBats)

names(AssignedBats)

AssignedBats$sample

extract(AssignedBats$sample$stat[[1]], cbind(AssignDataBat$long[1], AssignDataBat$lat[1]))
extract(AssignedBats$sample$pv[[1]], cbind(AssignDataBat$long[1], AssignDataBat$lat[1]))

extract(AssignedBats$sample$stat[[13]], cbind(AssignDataBat$long[13], AssignDataBat$lat[13]))
extract(AssignedBats$sample$pv[[13]], cbind(AssignDataBat$long[13], AssignDataBat$lat[13]))

Pvalues <- sapply(1:nrow(AssignDataBat),
                  function(i) extract(AssignedBats$sample$pv[[i]],
                                       cbind(AssignDataBat$long[i],
                                             AssignDataBat$lat[i])))
AssignDataBat$sample_ID[Pvalues <= 0.05]

AssignedBats$group

## plot(
##   x,
##   who     = "group",
##   cutoff  = list(draw = TRUE, level = 0.05, col = "#909090"),
##   sources = list(draw = TRUE, cex = 0.5, pch = 2, lwd = 1, col = "red"),
##   calibs  = list(draw = TRUE, cex = 0.5, pch = 4, lwd = 1, col = "blue"),
##   assigns = list(draw = TRUE, cex = 0.5, pch = 5, lwd = 1, col = "white"),
##   borders = list(borders = NA, lwd = 0.5, col = "black"),
##   mask    = list(mask = NA, lwd = 0, col = "black", fill = "black"),
##   mask2   = list(mask = NA, lwd = 0, col = "purple", fill = "purple"),
##   palette = list(step = NA, range = c(0, 1), n_labels = 11, digits = 2, fn = NA),
##   plot    = TRUE,
##   sphere  = list(build = FALSE, keep_image = FALSE),
##   ...
##   )

plot(AssignedBats, who = 1:14)

plot(AssignedBats, who = "Nnoc_15", plot = FALSE) +
  xyplot(AssignDataBat$lat[AssignDataBat$sample_ID == "Nnoc_15"] ~ AssignDataBat$long[AssignDataBat$sample_ID == "Nnoc_15"],
         pch = 13, col = "white", cex = 2, lwd = 2, panel = panel.points)

plot(AssignedBats) ## plot the group assignment

AssignDataBat2 <- subset(AssignDataBat, sample_ID != "Nnoc_15")
AssignedBats2 <- isofind(data = AssignDataBat2, isoscape = GermanScape, calibfit = CalibBats)
plot(AssignedBats2)

GNIPDataDEagg <- prepsources(data = GNIPDataDE)

GermanFit <- isofit(data = GNIPDataDEagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

getelev()
ElevDE <- raster("~/elevation_world_z5.tif")
ElevRasterDE <- prepraster(ElevDE, isofit = GermanFit, aggregation_factor = 2)

GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

CalibBats <- calibfit(data = CalibDataBat, isofit = GermanFit)

Assigned15 <- isofind(data = subset(AssignDataBat, sample_ID == "Nnoc_15"),
                      isoscape = GermanScape, calibfit = CalibBats)

plot(Assigned15)
