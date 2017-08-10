library(rasterVis)

elevationraster <- raster("gmted2010_30mn.tif")
elevation.raster <- aggregate(elevationraster, 100)
levelplot(elevation.raster)
proj4string(elevation.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#proj4string(elevation.raster) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

min.long.crop <- 50
max.long.crop <- -50
min.lat.crop <- -90
max.lat.crop <- 90

coord.full <- extent(elevation.raster)
min.long.full <- coord.full@xmin
max.long.full <- coord.full@xmax
min.lat.full <- coord.full@ymin
max.lat.full <- coord.full@ymax

if ((min.long.crop > max.long.crop) && (min.lat.crop < max.lat.crop)) {
  crop1 <- crop(elevation.raster, extent(min.long.full, max.long.crop, min.lat.crop, max.lat.crop))
  crop2 <- crop(elevation.raster, extent(min.long.crop, max.long.full, min.lat.crop, max.lat.crop))
  crop <- shift(merge(crop1, shift(crop2, x = -360)), x = 360)
}

levelplot(crop)
plot(crop)

origin <- data.frame(long = 170, lat = 0)
levelplot(crop)+
  xyplot(origin$lat~origin$long, pch = 13, col = "white", cex = 10, lwd = 2, 
       panel = panel.points) 

plot(crop)


