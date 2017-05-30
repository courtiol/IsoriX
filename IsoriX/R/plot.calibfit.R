#' @rdname plots
#' @method plot calibfit
#' @export
plot.calibfit <- function(x, ...) {
  xs <- with(x$calib.data,
             seq(min(mean.iso),
                 max(mean.iso),
                 length = 100
                 )
             )

  X <- cbind(1, xs)
  fitted <- X %*% x$param
  fixedVar <- rowSums(X * (X %*% x$fixefCov)) ## = diag(X %*% x$fixefCov %*% t(X))

  with(x$calib.data,
       graphics::plot(tissue.value ~ mean.iso,
                      xlab = "Isotopic value in the environment",
                      ylab = "Isotopic value in the organisms",
                      las = 1
                      )
       )

  graphics::points(fitted ~ xs, type = "l", col = "blue", lwd = 2)
  graphics::points(fitted + stats::qnorm(0.975)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")
  graphics::points(fitted - stats::qnorm(0.975)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")

  ## tweak to please codetools::checkUsagePackage('IsoriX', skipWith = TRUE)
  rm(fitted, fixedVar)

  return(invisible(NULL))
}

