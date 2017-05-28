#' @rdname plots
#' @method plot calibfit
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
       plot(tissue.value ~ mean.iso,
            xlab = "Isotopic value in the environment",
            ylab = "Isotopic value in the organisms",
            las = 1
            )
       )

  points(fitted ~ xs, type = "l", col = "blue", lwd = 2)
  points(fitted + qnorm(0.975)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")
  points(fitted - qnorm(0.975)*fixedVar ~ xs, col = "blue", lty = 2, type = "l")

  ## tweak to please codetools::checkUsagePackage('IsoriX', skipWith = TRUE)
  rm(fitted, fixedVar)

  return(invisible(NULL))
}

