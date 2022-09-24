library(IsoriX)

## Building the precipitation isoscape
GNIPDataDEagg <- prepsources(data = GNIPDataDE)
GermanFit <- isofit(data = GNIPDataDEagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)

## Calibration using lab method (here, on made up data)
set.seed(123)
CalibDataAlien2 <- create_aliens(calib_fn = list(intercept = 3, slope = 0.5,
                                                 resid_var = 5),
                                 isoscape = GermanScape,
                                 raster = ElevRasterDE,
                                 n_sites = 25,
                                 min_n_samples = 5,
                                 max_n_samples = 5)

CalibDataAlien2 <- CalibDataAlien2[, c("site_ID", "sample_ID", "source_value", 
                                       "sample_value")]
CalibAlien2 <- calibfit(data = CalibDataAlien2, method = "lab")

## plotting mean isoscape for precipitation
plot(GermanScape)

## plotting mean isoscape for predicted animal tissue
TissueScape <- CalibAlien2$param["intercept"] + CalibAlien2$param["slope"] * GermanScape$isoscapes$mean
plot(TissueScape)

## hack for plotting using IsoriX
TissueScape2 <- list(isoscapes = list(mean = TissueScape))
class(TissueScape2) <- c("ISOSCAPE", "list")
plot(TissueScape2)

## hack to change the title posthoc
plot_obj <- lattice::trellis.last.object() # recover last plotting object
plot_obj$main <- bquote(.("Animal")~.(bquote(delta**2*H))) # new title, call also be just character string
print(plot_obj) # replot with updated title
