########################################################################
#                                                                      #
# This is a dump of all the R code from the presentation about IsoriX  #
#                                                                      #
#        It is not commented as it comes with the slides               #
#                                                                      #
########################################################################


library(IsoriX)  ## to load

head(GNIPDataDE)
tail(GNIPDataDE)

GNIPDataDEagg <- prepsources(data = GNIPDataDE)
head(GNIPDataDEagg)


library(sp) ## for plotting polygons

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "Mean")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = round(GNIPDataDEagg$mean_source_value, digits = 1), col = "blue", cex = 1.5, font = 2)

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "SD")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = round(sqrt(GNIPDataDEagg$var_source_value), digits = 1), col = "blue", cex = 1.5, font = 2)

plot(CountryBorders, xlim = range(GNIPDataDEagg$long), ylim = range(GNIPDataDEagg$lat), col = "lightgrey", main = "N")
text(x = GNIPDataDEagg$long, y = GNIPDataDEagg$lat, labels = GNIPDataDEagg$n_source_value, col = "blue", cex = 1.5, font = 2)

GermanFit <- isofit(data = GNIPDataDEagg,
                    mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

names(GermanFit)

GermanFit$mean_fit
plot(GermanFit)

GermanFit2 <- isofit(data = GNIPDataDEagg,
                     mean_model_fix = list(elev = TRUE, lat_abs = FALSE))

AIC(GermanFit$mean_fit)
AIC(GermanFit2$mean_fit)

getelev(path = "~/Desktop/")
ElevWorld <- raster("~/Desktop/gmted2010_30mn.tif")  ## turn the tif into raster

ElevWorld

ElevRasterDE <- prepraster(ElevWorld, isofit = GermanFit, aggregation_factor = 5)
plot(ElevRasterDE)

levelplot(ElevRasterDE, margin = FALSE) +
  layer(sp.polygons(CountryBorders, col = "white")) +
  layer(sp.polygons(OceanMask, fill = "cyan")) +
  xyplot(GNIPDataDEagg$lat ~ GNIPDataDEagg$long, col = "pink")

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
plot(GermanScape, palette = list(fn = NULL, step = 10, range = c(-110, -40),
                                 n_labels = 3))

plot(GermanScape,
     sources = list(draw = TRUE, cex = 2, pch = 5, lwd = 2, col = "white"))
plot(GermanScape, mask  = list(fill = "orange"))
plot(GermanScape, mask  = list(mask = NULL))
plot(GermanScape, 
     mask  = list(mask = rbind(OceanMask,
                               CountryBorders[names(CountryBorders) != "Germany"]),
                  fill = "orange"))

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

CalibBats <- calibfit(data = CalibDataBat, isofit = GermanFit)

CalibBats
plot(CalibBats)

AssignDataBat
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
plot(AssignedBats, who = 1:14)

plot(AssignedBats, who = "Nnoc_15", plot = FALSE) +
    xyplot(AssignDataBat$lat[AssignDataBat$sample_ID == "Nnoc_15"] ~ AssignDataBat$long[AssignDataBat$sample_ID == "Nnoc_15"],
         pch = 13, col = "white", cex = 2, lwd = 2, panel = panel.points)

plot(AssignedBats) ## plot the group assignment

AssignDataBat2 <- subset(AssignDataBat, sample_ID != "Nnoc_15")
AssignedBats2 <- isofind(data = AssignDataBat2, isoscape = GermanScape, calibfit = CalibBats)
plot(AssignedBats2)

GNIPDataDEagg   <- prepsources(data = GNIPDataDE)
EuropeFit       <- isofit(data = GNIPDataEUagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
ElevEurope      <- prepraster(raster = ElevWorld, isofit = EuropeFit, aggregation_factor = 10)
EuropeIsoscape  <- isoscape(raster = ElevEurope, isofit = EuropeFit)
CalibBatsEU     <- calibfit(data = CalibDataBat, isofit = EuropeFit)
Assigned15      <- isofind(data = subset(AssignDataBat, sample_ID == "Nnoc_15"),
                           isoscape = EuropeIsoscape, calibfit = CalibBats)
plot(Assigned15)

                           
