#' @rdname IsoriX-defunct
#' @export
Isosim <- function(...) {
  .Defunct("isosim")
}

#' Simulate isotopic values
#' 
#' This function allows for the simulation of isoscapes. Both partial or
#' complete (i.e. maps) isoscapes can be simulated.
#' 
#' This function takes as inputs the values for all covariates matching a
#' series of locations (which can be provided as an elevation raster or as a
#' \var{data.frame}), as well as the parameters of the isoscape model. The
#' function is not required to fit an isoscape, nor to perform assignments. It
#' is an additional function that can be useful to test the method, and to
#' study the effect of different parameters on isoscapes. We chose default
#' values for parameters inspired to model fits in order to simulate a
#' relatively realistic isoscape. We precised 'relatively' because, in the
#' simulations, the Matern realizations for the mean and the dispersion are
#' drawn independently, which is not the case in nature. Note that extra
#' parameters present in the input lists will not make the function crash but
#' won't be considered during computations either.
#' 
#' @param simu.data A \var{data.frame} containing the covariates needed for the
#' simulation, or alternatively an elevation raster of class \var{RasterLayer}
#' @param mean.model.fix.coef A \var{vector} of coefficients for fixed-effects
#' @param disp.model.fix.coef A \var{vector} of coefficients for fixed-effects
#' @param mean.model.matern.coef A \var{vector} of coefficients for the spatial
#' random effect
#' @param disp.model.matern.coef A \var{vector} of coefficients for the spatial
#' random effect
#' @param mean.model.uncorr.coef A \var{vector} of coefficients for the
#' uncorrelated random effect
#' @param disp.model.uncorr.coef A \var{vector} of coefficients for the
#' uncorrelated random effect
#' @param dist.method A \var{string} indicating the distance method
#' @param seed An \var{integer} used to set the seed of the random generator
#' @param save.dataframe A \var{logical} indicating whether the detailed
#' \var{data.frame} containing the simulated data should be saved
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \var{TRUE} if users use an interactive R
#' session and \var{FALSE} otherwise.
#' @return This function returns a \var{list} of class \var{isoscape}
#' containing a stack of raster and an optional \var{data.frame}. The stack
#' contains the raster \code{mean.raster} storing the mean isotopic value, and
#' the raster \cr \code{disp.raster} storing the residual dispersion variance.
#' The optional \var{data.frame} contains the simulated data and details of the
#' computation in an object called \code{data}.
#' @note The spatial autocorrelation and the Nugget are computed by the
#' functions \code{\link[RandomFields:RMmatern]{RMwhittle}} and
#' \code{\link[RandomFields]{RMnugget}}, respectively. These two functions are
#' part of the powerful package \pkg{\link[RandomFields]{RandomFields}}.
#' @seealso \code{\link{isofit}} for the function fitting the isoscape model
#' \code{\link{IsoriX}} for the complete work-flow
#' @keywords simulate simulation
#' @examples
#' 
#' 
#' ## This example takes couple of minutes and will therefore not be run
#' ## unless you type: example(isosim, run.dontrun = TRUE)
#' 
#' \dontrun{
#' ## We load an elevation raster
#' data(elevraster)
#' data(countries)
#' data(oceanmask)
#' data(isopalette1)
#' 
#' elevationraster <- relevate(elevraster,
#'     manual.crop = c(-30, 60, 30, 70))
#' 
#' ## We simulate data under default settings
#' simu <- isosim(simu.data = elevationraster,
#'     save.dataframe = TRUE, seed = 2)
#' 
#' simu
#' 
#' ## We build the plots of the outcome using IsoriX
#' plot.mean.simu <- plot(
#'     x = simu,
#'     which = "mean",
#'     borders = list(borders = countries),
#'     mask = list(mask = oceanmask),
#'     palette = isopalette1)
#' 
#' plot.disp.simu <- plot(
#'     x = simu,
#'     which = "disp",
#'     borders = list(borders = countries),
#'     mask = list(mask = oceanmask),
#'     palette = isopalette1)
#' 
#' 
#' ## We fit the simulated data by sampling 200 locations
#' 
#' set.seed(123)
#' newdat <- simu$data[sample(1:nrow(simu$data), 200), ]
#' 
#' isoscapemodel <- isofit(iso.data = newdat,
#'     mean.model.fix = list(elev = TRUE, lat.abs = TRUE))
#' 
#' isoscape <- isoscape(elevationraster, isoscapemodel)
#' 
#' plot.mean.fitted <- plot(
#'     x = isoscape,
#'     which = "mean",
#'     sources = list(draw = FALSE),
#'     borders = list(borders = countries),
#'     mask = list(mask = oceanmask),
#'     palette = isopalette1)
#' 
#' plot.disp.fitted <- plot(
#'     x = isoscape,
#'     which = "disp",
#'     sources = list(draw = FALSE),
#'     borders = list(borders = countries),
#'     mask = list(mask = oceanmask),
#'     palette = isopalette1)
#' 
#' ## We compare simulated and fitted data visually
#' if(require(lattice)){
#'     print(plot.mean.simu, split = c(1, 1, 2, 2), more = TRUE)
#'     print(plot.disp.simu, split = c(2, 1, 2, 2), more = TRUE)
#'     print(plot.mean.fitted, split = c(1, 2, 2, 2), more = TRUE)
#'     print(plot.disp.fitted, split = c(2, 2, 2, 2), more = FALSE)
#' }
#' 
#' ## We compare simulated and fitted data numerically
#' ## Note that differences are expected, as the geographic
#' ##   area is small compared to the scale at which
#' ##   spatial auto-correlation occurs
#' round(cbind(
#'     simulated = c(
#'         intercept = 64,
#'         lat.abs = -2.3, 
#'         elev = -0.01,
#'         nu = 0.35,
#'         rho = 5e-5,
#'         rho_div_nu  =  5e-5/0.35,
#'         lambda.matern = 899,
#'         intercept.disp = 5.8,
#'         nu.disp = 3.2e-01,
#'         rho.disp = 1.5e-05,
#'         lambda.matern.disp = 5),
#'     fitted = c(
#'         intercept = isoscapemodel$mean.fit$fixef[1],
#'         lat.abs = isoscapemodel$mean.fit$fixef[2], 
#'         elev = isoscapemodel$mean.fit$fixef[3],
#'         nu = isoscapemodel$mean.fit$ranFix$nu,
#'         rho = isoscapemodel$mean.fit$ranFix$rho,
#'         rho_div_nu = with(isoscapemodel$mean.fit$ranFix,rho/nu),
#'         lambda.matern = isoscapemodel$mean.fit$lambda,
#'         intercept.disp = isoscapemodel$disp.fit$fixef[1],
#'         nu.disp = isoscapemodel$disp.fit$ranFix$nu,
#'         rho.disp = isoscapemodel$disp.fit$ranFix$rho,
#'         lambda.matern.disp = isoscapemodel$disp.fit$lambda)), 4)
#' 
#' }
#' 
#' @export
isosim <- function(simu.data,
                   mean.model.fix.coef = c(intercept = 64, elev = -0.01, lat.abs = -2.3, lat.2 = 0, long = 0, long.2 = 0),
                   disp.model.fix.coef = c(intercept = 5.8, elev = 0, lat.abs = 0, lat.2 = 0, long = 0, long.2 = 0),
                   mean.model.matern.coef = c(nu = 0.35, rho = 5e-5, lambda = 899),
                   disp.model.matern.coef = c(nu = 3.2e-01, rho = 1.5e-05, lambda = 5),
                   mean.model.uncorr.coef = c(nugget = 0, lambda = 0),
                   disp.model.uncorr.coef = c(nugget = 0, lambda = 0),
                   dist.method = "Earth",
                   seed = NULL,
                   save.dataframe = FALSE,
                   verbose = interactive()
                   ) {

  if (!requireNamespace("RandomFields", quietly = TRUE)) {
    stop("the package 'RandomFields' is needed for this function,
    you can install it by typing install.packages('RandomFields')")
  }
  
  ## if simu.data is a raster, we convert it as data.frame
  if (class(simu.data) == "RasterLayer") {
    elevation.raster <- simu.data
    coord <- sp::coordinates(elevation.raster)
    simu.data <- data.frame(long = coord[, 1],
                            long.2 = coord[, 1]^2,
                            lat = coord[, 2],
                            lat.2 = coord[, 2]^2,
                            lat.abs = abs(coord[, 2]),
                            elev = raster::extract(elevation.raster, coord),
                            n.isoscape.value = rep(1e6, nrow(coord)),
                            stationID = as.factor(paste("simu", 1:nrow(coord), sep = "_"))
                            )
    rm(coord); gc() ## remove coord as it can be a large object
  }

  ## test if inputs are correct
  if (!any(dist.method %in% c("Earth", "Euclidean"))) {
    stop("the argument you chose for dist.method is unknown")
  }

  if (sum(mean.model.uncorr.coef > 0) > 1 | sum(disp.model.uncorr.coef > 0) > 1) {
    stop("mean.model.uncorr.coef and disp.model.uncorr.coef must have at least one coeficient equals to zero each (considering two parameterizations of uncorrelated random effects does not make any sense.)")
  }

  if (any(c(mean.model.uncorr.coef, disp.model.uncorr.coef) < 0)) {
    stop("all nugget and lambda coefficients must be null or positive")
  }

  if (any(c(mean.model.matern.coef, disp.model.matern.coef) < 0)) {
    stop("all spatial coefficients must be null or positive")
  }

  data.needed <- names(c(mean.model.fix.coef, disp.model.fix.coef))
  data.needed <- data.needed[data.needed != "intercept"]

  if (!all(data.needed %in% names(simu.data))) {
    stop(paste(c("you need to provide all the following covariates in simu.data (even if the coefficients associated to the missing ones are null):", data.needed), collapse = " "))
  }

  ## define the seeds (one for RandomFields, one for other R functions) and set other options for RandomFields
  set.seed(seed)
  RandomFields::RFoptions(seed = ifelse(is.null(seed), NA, seed),
                          spConform = FALSE,  ##so that RFsimulte returns vector directly
                          cPrintlevel = 1  ##cPrintlevel = 3 for more details
                          )

  if (dist.method == "Earth") {
    RandomFields::RFoptions(new_coord_sys = "earth")
    }
  if (dist.method == "Euclidean") {
    RandomFields::RFoptions(new_coord_sys = "cartesian")
  }
  ## see ?"coordinate system" for info about RF coordinate system


  ### Simulate the dispersion

  if (verbose) {
    print("Simulating the dispersion...")
  }

  ## compute the linear predictor
  linpred.disp <- .LinPred(fix.coef = disp.model.fix.coef,
                           matern.coef = disp.model.matern.coef,
                           uncorr.coef = disp.model.uncorr.coef,
                           data = simu.data
                           )

  simu.data$disp.logvar.fix <- linpred.disp$fix
  simu.data$disp.logvar.matern <- linpred.disp$matern
  simu.data$disp.logvar.uncorr <- linpred.disp$uncorr
  simu.data$disp.mean <- exp(linpred.disp$eta.sum)

  ## add residual variance
  simu.data$var.isoscape.value <- stats::rgamma(nrow(simu.data),
                                                shape = (simu.data$disp.mean^2)/2,
                                                scale = 2/simu.data$disp.mean
                                                )

  ### Simulate the mean
  if (verbose) {
    print("Simulating the mean...")
  }

  ## compute the linear predictor
  linpred.mean <- .LinPred(fix.coef = mean.model.fix.coef,
                           matern.coef = mean.model.matern.coef,
                           uncorr.coef = mean.model.uncorr.coef,
                           data = simu.data
                           )

  simu.data$mean.var.fix <- linpred.mean$fix
  simu.data$mean.var.matern <- linpred.mean$matern
  simu.data$mean.var.uncorr <- linpred.mean$uncorr
  simu.data$isoscape.value <- linpred.mean$eta.sum


  ### Building rasters
  if (verbose) {
    print("Building rasters...")
  }

  SaveRaster <- function(x){
    with(simu.data, {
      .CreateRaster(long = long,
                    lat = lat,
                    values = get(x),
                    proj = "+proj=longlat +datum=WGS84"
                   )
    })
  }

  mean.raster <- SaveRaster("isoscape.value")
  disp.raster <- SaveRaster("var.isoscape.value")

  ### Buidling return object
  out <- list()

  out$isoscape <- raster::stack(list(
    "mean" = mean.raster,
    "disp" = disp.raster
    ))

  if (!save.dataframe & interactive()) {
    message(paste("Note: simulated data not saved as data.frame (save.dataframe is set to FALSE). Saving the simulated data as data.frame would require", format(utils::object.size(simu.data), units = "MB")))
  } else {
    out$data <- simu.data
  }
    

  class(out) <- c("isoscape", "isosim", "list")

  return(out)
}


.LinPred <- function(fix.coef, matern.coef, uncorr.coef, data) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It builds the linear predictor for the simulations
  
  if (!requireNamespace("RandomFields", quietly = TRUE)) {
    stop("the package 'RandomFields' is needed for this function,
    you can install it by typing install.packages('RandomFields')")
  }
  
  ## fixed effects
  fix <- with(as.list(fix.coef), intercept +
                elev*data$elev + lat.abs*data$lat.abs + lat.2*data$lat.2 +
                long*data$long + long.2*data$long.2
              )

  ## spatial random effects
  matern <- 0
  if (matern.coef["lambda"] > 0) {
    model.matern <- with(as.list(matern.coef),
                         RandomFields::RMwhittle(nu = nu, var = lambda, scale = 1/rho)
                         )
    matern <- RandomFields::RFsimulate(model.matern,
                                       x = data$long, y = data$lat
                                       )
  }

  ## uncorr random effects
  uncorr <- stats::rnorm(nrow(data), mean = 0, sd = sqrt(uncorr.coef["lambda"]))
  if (uncorr.coef["nugget"] > 0) {
    uncorr <- uncorr + RandomFields::RFsimulate(RandomFields::RMnugget(var = uncorr.coef["nugget"]),
                                                x = data$long, y = data$lat
                                                )
  }
  eta.sum <- fix + matern + uncorr
  return(list("fix" = fix, "matern" = matern, "uncorr" = uncorr, "eta.sum" = eta.sum))
}
