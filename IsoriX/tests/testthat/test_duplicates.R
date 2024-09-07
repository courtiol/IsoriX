## preparation steps:
GNIPDataDEagg <- prepsources(data = GNIPDataDE)

GermanFit <- isofit(
  data = GNIPDataDEagg,
  mean_model_fix = list(elev = FALSE, lat_abs = FALSE),
  mean_model_rand = list(uncorr = FALSE, spatial = TRUE),
  disp_model_rand = list(uncorr = FALSE, spatial = FALSE)
)

GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

## tests:
test_that("prepsources() flags duplicated locations with different IDs", {
  GNIPDataDE2 <- GNIPDataDE[1:3, ]
  GNIPDataDE2 <- rbind(GNIPDataDE2, GNIPDataDE2)
  GNIPDataDE2$source_ID <- c("a", "a", "a", "b", "b", "b")
  expect_warning(prepsources(GNIPDataDE2))
})

test_that("prepsources() flags IDs actualy corresponding to different locations", {
  GNIPDataDE2 <- GNIPDataDE[1:3, ]
  GNIPDataDE2 <- rbind(GNIPDataDE2, GNIPDataDE2)
  GNIPDataDE2$elev[1:3] <- 10
  expect_warning(prepsources(GNIPDataDE2))
})

test_that("calibfit() flags siteIDs actualy corresponding to different locations", {
  CalibDataAlien2 <- CalibDataAlien[1:20, ]
  CalibDataAlien2 <- rbind(CalibDataAlien2, CalibDataAlien2)
  CalibDataAlien2$elev[1:3] <- 10
  expect_warning(calibfit(data = CalibDataAlien2, isofit = GermanFit))
})

## FIXME test sometimes returns an error and sometimes a warning, not sure why...
# test_that("isofind() flags siteIDs actualy corresponding to different locations", {
#   AssignDataAlien2 <- AssignDataAlien[1:3, ]
#   AssignDataAlien2 <- rbind(AssignDataAlien2, AssignDataAlien2)
#   AssignDataAlien2$lat[1:3] <- 10
#   expect_error(isofind(
#     data = AssignDataAlien2,
#     isoscape = GermanScape,
#     calibfit = NULL
#   ))
# })
