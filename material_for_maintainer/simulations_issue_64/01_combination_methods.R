## Simulation 1: validity of p-value combination methods for group assignment
## (see https://github.com/courtiol/IsoriX/issues/64)
##
## The generative model mirrors eq. 9.18 of Courtiol et al. 2019 (terms 1-3),
## using variance components estimated on the package's German example
## (GNIPDataDE + CalibDataAlien). A group of n animals truly originates from a
## candidate cell; they share the isoscape prediction error at that cell and the
## error on the calibration coefficients, and optionally an extra shared
## deviation of correlation rho (group migration / common habitat).
## We record how often the true origin is rejected by each group test.
##
## Run from the root of the repository:
##   Rscript material_for_maintainer/simulations_issue_64/01_combination_methods.R
## Runtime: ca. 3 min.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
out_dir <- "material_for_maintainer/simulations_issue_64/results"
dir.create(out_dir, showWarnings = FALSE)

## ---- variance components from the German example ----
GermanFit <- isofit(data = prepsources(data = GNIPDataDE), mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
CalibAlien <- calibfit(data = CalibDataAlien, isofit = GermanFit)
predVar <- terra::values(GermanScape$isoscapes$mean_predVar)[, 1]
mu <- terra::values(GermanScape$isoscapes$mean)[, 1]
comp <- list(predVar = predVar[!is.na(predVar)], mu = mu[!is.na(predVar)])

set.seed(64)
R <- 50000

alpha <- CalibAlien$param[["intercept"]]
beta <- CalibAlien$param[["slope"]]
V <- CalibAlien$fixefCov
phi <- CalibAlien$phi

## ---- combination methods (operate on rows of a p matrix) ----
fisher <- function(p) pchisq(-2 * rowSums(log(p)), df = 2 * ncol(p), lower.tail = FALSE)
cauchy <- function(p) {
  t <- ifelse(p < 1e-15, 1 / (p * pi), tan((0.5 - p) * pi))
  pcauchy(rowMeans(t), lower.tail = FALSE)
}
hmp_raw <- function(p) pmin(1, ncol(p) / rowSums(1 / p))
## exact joint-Gaussian test: s' Sigma^-1 s ~ chi2(n); Sigma built from the
## quantities isofind already has (per replicate, since calib estimates vary)
## NB: X uses the candidate cell's isoscape mean (the H0 origin value), not each
## sample's own mean_origin: a data-dependent Sigma makes the test conservative.
gauss <- function(s, muhat, v1, bhat) {
  n <- ncol(s)
  vapply(seq_len(nrow(s)), function(r) {
    X <- cbind(1, rep(muhat[r], n))
    Sig <- diag(phi / bhat[r]^2, n) + v1 + X %*% V %*% t(X) / bhat[r]^2
    Si_s <- solve(Sig, s[r, ])
    Si_1 <- solve(Sig, rep(1, n))
    q <- drop(crossprod(s[r, ], Si_s))
    z <- sum(Si_s) / sqrt(sum(Si_1)) # GLS estimate of the common shift / its SE
    c(chisq = pchisq(q, df = n, lower.tail = FALSE), glsmean = 2 * pnorm(-abs(z)))
  }, numeric(2))
}

## ---- one scenario ----
## delta: true origin value minus candidate-cell value (0 = candidate is the true origin)
sim_one <- function(n, v1, mu, rho, delta = 0) {
  e_iso <- rnorm(R, 0, sqrt(v1)) # shared isoscape prediction error at the candidate cell
  e_cal <- MASS::mvrnorm(R, c(0, 0), V) # shared calibration coefficient error
  ahat <- alpha + e_cal[, 1]
  bhat <- beta + e_cal[, 2]
  u <- rnorm(R) # shared group deviation (group migration / common micro-habitat)
  eta <- matrix(rnorm(R * n), R, n)
  eps <- sqrt(phi) * (sqrt(rho) * u + sqrt(1 - rho) * eta)
  mu_true <- mu + delta
  y <- alpha + beta * mu_true + eps # sample values
  mo <- (y - ahat) / bhat # predicted value at origin (as in isofind)
  muhat <- mu + e_iso # isoscape prediction at candidate cell
  s <- mo - muhat
  ## per-sample variance exactly as isofind computes it (terms 1-3)
  vs <- v1 + phi / bhat^2 + (V[1, 1] + 2 * V[1, 2] * mo + V[2, 2] * mo^2) / bhat^2
  p <- 2 * pnorm(-abs(s) / sqrt(vs))
  p <- pmax(p, 1e-300)
  g <- if (n > 1) gauss(s, muhat, v1, bhat) else rbind(p[, 1], p[, 1])
  out <- data.frame(
    single = p[, 1],
    Fisher = fisher(p),
    Gaussian = g[1, ],
    GLSmean = g[2, ],
    Cauchy = cauchy(p),
    HMP_raw = hmp_raw(p)
  )
  out
}

q_cells <- c(p10 = 0.10, p50 = 0.50, p90 = 0.90, max = 1)
cells <- lapply(q_cells, function(q) {
  i <- which.min(abs(comp$predVar - quantile(comp$predVar, q)))
  c(v1 = comp$predVar[i], mu = comp$mu[i])
})

## ---- 1. size (false exclusion of the true origin) ----
grid <- expand.grid(n = c(1, 2, 5, 10, 30), cell = names(cells), rho = c(0, 0.25, 0.5),
                    stringsAsFactors = FALSE)
size <- do.call(rbind, lapply(seq_len(nrow(grid)), function(k) {
  g <- grid[k, ]
  ce <- cells[[g$cell]]
  r <- sim_one(g$n, ce[["v1"]], ce[["mu"]], g$rho)
  data.frame(g, row.names = NULL, v1 = ce[["v1"]],
             shared_frac = ce[["v1"]] / (ce[["v1"]] + phi / beta^2),
             method = names(r),
             rej05 = colMeans(r < 0.05), rej01 = colMeans(r < 0.01))
}))
write.csv(size, file.path(out_dir, "01_size.csv"), row.names = FALSE)

## ---- 2. power (candidate cell differs from the true origin by delta) ----
pgrid <- expand.grid(delta = c(0, 2, 4, 6, 8, 10, 14), n = c(5, 10), stringsAsFactors = FALSE)
power <- do.call(rbind, lapply(seq_len(nrow(pgrid)), function(k) {
  g <- pgrid[k, ]
  ce <- cells[["p50"]]
  r <- sim_one(g$n, ce[["v1"]], ce[["mu"]], rho = 0, delta = g$delta)
  data.frame(g, row.names = NULL, method = names(r), rej05 = colMeans(r < 0.05))
}))
write.csv(power, file.path(out_dir, "01_power.csv"), row.names = FALSE)

## ---- summary ----
show <- subset(size, method != "single" & cell %in% c("p50", "max"))
print(xtabs(rej05 ~ method + n + cell + rho, show))
print(reshape(power, idvar = c("delta", "n"), timevar = "method", direction = "wide"))
