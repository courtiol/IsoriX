########################################################################
#                                                                      #
# This is a dump of all the R code from the presentation about IsoriX  #
#                                                                      #
#        It is not commented as it comes with the slides               #
#                                                                      #
########################################################################

## install.packages("IsoriX")  ## to install

library(IsoriX)  ## to load

tools::package_dependencies("IsoriX") ## note: list dependencies, not suggested packages

setdiff(ls("package:IsoriX"), ## list all exported objects (ie. functions + data, but not methods)
        c("CountryBorders", "OceanMask", "ElevRasterDE", "PrecipBrickDE", ## remove lazy loaded data
          data(package = "IsoriX")$results[, "Item"]))  ## remove non loaded data

data(package = "IsoriX")$results[, c("Item", "Title")]

head(GNIPDataDE)
tail(GNIPDataDE)

GNIPDataDEagg <- prepsources(data = GNIPDataDE)

head(GNIPDataDEagg)

range_expanded <-  function(x, stretch = 0.05) range(x) + diff(range(x))*c(-1, 1)*stretch

with(GNIPDataDEagg, plot(CountryBorders, xlim = range_expanded(long), ylim = range_expanded(lat), col = "lightgrey", main = "Mean"))
text(lat ~ long, data = GNIPDataDEagg, labels = round(mean_source_value, digits = 1), col = "blue", cex = 1.5, font = 2)

with(GNIPDataDEagg, plot(CountryBorders, xlim = range_expanded(long), ylim = range_expanded(lat), col = "lightgrey", main = "SD"))
text(lat ~ long, data = GNIPDataDEagg, labels = round(sqrt(var_source_value), digits = 1), col = "blue", cex = 1.5, font = 2)

with(GNIPDataDEagg, plot(CountryBorders, xlim = range_expanded(long), ylim = range_expanded(lat), col = "lightgrey", main = "N"))
text(lat ~ long, data = GNIPDataDEagg, labels = n_source_value, col = "blue", cex = 1.5, font = 2)

GermanFit <- isofit(data = GNIPDataDEagg,
                    mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

names(GermanFit)

GermanFit$mean_fit

plot(GermanFit)

GermanFit2 <- isofit(data = GNIPDataDEagg,
                     mean_model_fix = list(elev = TRUE, lat_abs = FALSE))

print(AIC(GermanFit$mean_fit))
print(AIC(GermanFit2$mean_fit))

getelev(file = "elevation_DE_z5.tif",
        long_min = 5.86, long_max = 15.02, lat_min = 47.23, lat_max = 54.90)

ElevDE <- rast("elevation_DE_z5.tif")  ## turn the tif into raster (a SpatRaster object)
ElevDE

plot(ElevRasterDE)

levelplot(ElevRasterDE, margin = FALSE) +
  layer(lpolygon(CountryBorders, border = "white")) +
  layer(lpolygon(OceanMask, border = "white", col = "cyan")) +
  xyplot(lat ~ long, data = GNIPDataDEagg, col = "pink")

GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

names(GermanScape)

GermanScape$isoscapes

plot(GermanScape)

plot(GermanScape, which = "mean_predVar")

plot(GermanScape, which = "mean_residVar")

plot(GermanScape, palette = list(fn = rainbow))

plot(GermanScape, palette = list(fn = NULL))

plot(GermanScape, palette = list(fn = NULL, step = 1))

plot(GermanScape, palette = list(fn = NULL, step = 10, range = c(-110, -40)))

plot(GermanScape, palette = list(fn = NULL, step = 10, range = c(-110, -40), n_labels = 3))

plot(GermanScape, sources = list(draw = TRUE, cex = 2, pch = 5, lwd = 2, col = "white"))

plot(GermanScape, mask = list(fill = "orange"))

plot(GermanScape, mask = list(mask = NULL))

Germany <- rnaturalearth::ne_states(country = "Germany", returnclass = "sv")
Berlin <- Germany[Germany$name == "Berlin"]
plot(GermanScape, mask = list(mask = rbind(OceanMask, Berlin), fill = "white"))

terra::writeRaster(GermanScape$isoscapes$mean,
                   filename = "GermanScape.tif", #many formats are possible
                   overwrite = TRUE)

head(CalibDataBatRev)
str(CalibDataBatRev)

Germany <- rnaturalearth::ne_countries(country = "Germany", scale = "large", returnclass = "sv")
with(CalibDataBatRev, plot(CountryBorders, xlim = range_expanded(long), ylim = range_expanded(lat)))
plot(Germany, col = "orange", border = "purple", add = TRUE)
points(jitter(lat, factor = 0.1) ~ jitter(long, factor = 0.1), data = CalibDataBatRev, col = "blue")

IsoPredicted <- values(GermanScape$isoscapes$mean)[, "mean"]
IsoAtBats <- extract(GermanScape$isoscapes$mean, CalibDataBatRev[, c("long", "lat")])[, "mean"]
hist(IsoPredicted, nclass = 1000, main = "", xlab = "Deuterium")
rug(IsoAtBats, col = "red", lwd = 2)
legend("topleft", fill = c("red", "black"), legend = c("sampled locations", "predicted locations"))

IsoPredictedPredVar <- values(GermanScape$isoscapes$mean_predVar)
plot(x = IsoPredicted, y = IsoPredictedPredVar, xlab = "Predicted mean", ylab = "PredVar")
rug(IsoAtBats, col = "red", lwd = 2)

plot(GermanScape, plot = FALSE) +
  xyplot(lat ~ long, data = CalibDataBatRev, pch = 13, col = "white", cex = 2, lwd = 2)

CalibBats <- calibfit(data = CalibDataBatRev, isofit = GermanFit)
CalibBats

plot(CalibBats)

AssignDataBatRev

AssignedBats <- isofind(data = AssignDataBatRev, isoscape = GermanScape, calibfit = CalibBats)
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

plot(AssignedBats, who = 1:14)

plot(AssignedBats, who = "Nnoc_15", plot = FALSE) +
  xyplot(lat ~ long, data = subset(AssignDataBatRev, sample_ID == "Nnoc_15"),
         pch = 13, col = "white", cex = 2, lwd = 2)

plot(AssignedBats) ## plot the group assignment

AssignDataBat2 <- subset(AssignDataBat, sample_ID != "Nnoc_15")
AssignedBats2 <- isofind(data = AssignDataBat2, isoscape = GermanScape, calibfit = CalibBats)
plot(AssignedBats2)

GNIPDataDEagg <- prepsources(data = GNIPDataDE)

GermanFit     <- isofit(data = GNIPDataDEagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

getelev()
ElevDE        <- rast("~/elevation_world_z5.tif")
ElevRasterDE  <- prepraster(ElevDE, isofit = GermanFit, aggregation_factor = 2)

GermanScape   <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

CalibBats     <- calibfit(data = CalibDataBatRev, isofit = GermanFit)

Assigned15    <- isofind(data = subset(AssignDataBatRev, sample_ID == "Nnoc_15"),
                         isoscape = GermanScape, calibfit = CalibBats)

plot(Assigned15)
