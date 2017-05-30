#' @rdname plots
#' @method plot isofit
#' @export
plot.isofit <- function(x, cex.scale = 0.2, ...) {

  ## Test if RStudio is in use
  RStudio <- .Platform$GUI == "RStudio"

  if (!any(class(x) %in% "multiisofit")) {
    ## Determine number of plots in panel
    if (RStudio) {
      nplot <- 1
    } else {
      nplot <- 2 + x$info.fit$disp.model.rand$spatial +
        x$info.fit$mean.model.rand$spatial
    }
  
    ## Define mfrow (number of rows and column in panel)
    mfrow <- switch(as.character(nplot),
                    "1" = c(1, 1),
                    "2" = c(1, 2),
                    "3" = c(1, 3),
                    "4" = c(2, 2),
                    stop("nplot value not anticipated")
                    )
  
    ## Setup the graphic device
    graphics::par(mfrow = mfrow)
  
    ## Plots from spaMM
    spaMM::plot.HLfit(x$mean.fit,
                "predict",
                cex = 0.1 + cex.scale*log(x$mean.fit$data$weights.mean),
                las = 1, ...
                )
    graphics::title(main = "Pred vs Obs in mean.fit")
    .HitReturn()
  
    spaMM::plot.HLfit(x$disp.fit,
                      "predict",
                      cex = 0.1 + cex.scale*log(x$disp.fit$data$weights.disp),
                      las = 1, ...
                      )
    graphics::title(main = "Pred vs Obs in disp.fit")
  
    ## Plot Matern autocorrelation
    if (x$info.fit$mean.model.rand$spatial) {
      .HitReturn()
      .PlotMatern(x$mean.fit, ...)
      graphics::title(main = "Autocorrelation in mean.fit")
    }
  
    if (x$info.fit$disp.model.rand$spatial) {
      .HitReturn()
      .PlotMatern(x$disp.fit, ...)
      graphics::title(main = "Autocorrelation in disp.fit")
    }
    
    ## Reset the graphic device
    graphics::par(mfrow = c(1, 1))
  } else {
    for (fit in 1:length(x$multi.fits)) {
      cat("\n")
      cat(paste("##### Plots for pair of models", names(x$multi.fits)[fit]), "#####")
      cat("\n")
      graphics::plot(x$multi.fits[[fit]])
    }
  }
  return(invisible(NULL))
}


.PlotMatern <- function(model, limit = 0.5, ...) {
  ## This function should not be called by the user but is itself called by other functions.
  ## It plots the Matern autocorrelation.
  d.stop <- FALSE
  d <- 0

  while ((d < 50000) & !d.stop) {
    d <- d + 10
    m <- spaMM::MaternCorr(d = d,
                           rho = model$corrPars$rho,
                           nu = model$corrPars$nu
                           )
    if (m < limit) d.stop <- TRUE
  }

  distances <- seq(0, d, 1)

  m <- spaMM::MaternCorr(d = distances,
                         rho = model$corrPars$rho,
                         nu = model$corrPars$nu
                         )

  graphics::plot(m ~ distances,
            type = "l",
            las = 1,
            xlab = "Distances (km)",
            ylab = "Correlation",
            ...
            )
}

