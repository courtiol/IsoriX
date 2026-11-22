## Simulation 3: validate the linearised calibration used in sim_term4.R by
## refitting calibfit() on simulated calibration data.
##
## Conditional on the fitted German isoscape, each replicate draws the isoscape
## prediction errors jointly at the calibration sites and candidate cells,
## simulates CalibDataAlien-like data (same design) from the "true" isoscape,
## refits calibfit(method = "wild") and stores the estimates. We then compare
##   - the empirical covariance of (intercept, slope) with fixefCov (Hessian),
##   - the empirical Cov(d_cell, x' e_theta) with the GLS prediction used for term 4,
##   - the estimated site variance with the truth.
##
## Run from the root of the repository (optional arguments: nrep lambda_true ncores):
##   Rscript material_for_maintainer/simulations_issue_64/03_validate_calibfit.R 200 0 4
## Runtime: each calibfit() takes ca. 85 s, so 200 replicates on 4 cores take ca. 60 min.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
args <- as.numeric(commandArgs(TRUE))
nrep <- if (length(args) >= 1) args[1] else 200
lambda_true <- if (length(args) >= 2) args[2] else 0 # site variance used to simulate calibration data
ncores <- if (length(args) >= 3) args[3] else parallel::detectCores()
out_dir <- "material_for_maintainer/simulations_issue_64/results"
dir.create(out_dir, showWarnings = FALSE)
set.seed(2024)

fit <- isofit(data = prepsources(data = GNIPDataDE), mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
scape <- isoscape(raster = ElevRasterDE, isofit = fit)
cf <- calibfit(CalibDataAlien, isofit = fit, verbose = FALSE)
alpha <- cf$param[["intercept"]]
beta <- cf$param[["slope"]]
phi <- cf$phi
cdat <- cf$calib_fit$data
site <- factor(cdat$site_ID)
first <- match(levels(site), site)
S <- nlevels(site)

xs <- attr(scape, "xs")
mu_r <- scape$isoscapes$mean
cell_sites <- terra::cellFromXY(mu_r, as.matrix(cdat[first, c("long", "lat")]))
pv <- terra::values(scape$isoscapes$mean_predVar)[, 1]
q_cells <- sapply(c(0.5, 0.9, 1), function(q) which.min(abs(pv - quantile(pv, q, na.rm = TRUE))))
cells <- c(cell_sites, q_cells)
K <- length(cells)
muhat_c <- terra::values(mu_r)[cells, 1]
cols <- names(xs)
nd <- rbind(cdat[first, cols], xs[cells, cols])
pr <- spaMM::predict.HLfit(fit$mean_fit, newdata = nd, variances = list(predVar = TRUE, cov = TRUE))
C <- attr(pr, "predVar")
muhat_s <- c(pr)[seq_len(S)]
L <- chol(C + diag(1e-9, nrow(C)))

base <- CalibDataAlien
one_rep <- function(r) {
  d <- drop(rnorm(nrow(C)) %*% L)
  mu_true_s <- muhat_s - d[seq_len(S)]
  b <- rnorm(S, 0, sqrt(lambda_true))
  idx <- match(as.character(base$site_ID), levels(site))
  dat <- base
  dat$sample_value <- alpha + beta * mu_true_s[idx] + b[idx] + rnorm(nrow(base), 0, sqrt(phi))
  f <- tryCatch(suppressMessages(calibfit(dat, isofit = fit, verbose = FALSE)), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  c(ahat = f$param[["intercept"]], bhat = f$param[["slope"]],
    V11 = f$fixefCov[1, 1], V12 = f$fixefCov[1, 2], V22 = f$fixefCov[2, 2],
    phi = f$phi, lambda_site = unname(f$calib_fit$lambda[2]),
    setNames(d[S + seq_len(K)], paste0("d_c", seq_len(K))))
}
## the (slow) simulation is skipped if its output already exists
## (note: replicates are not bit-reproducible, as mclapply streams are not seeded with L'Ecuyer)
rds <- file.path(out_dir, sprintf("03_validate_lambda%s.rds", format(lambda_true)))
if (!file.exists(rds)) {
  t0 <- Sys.time()
  res <- parallel::mclapply(seq_len(nrep), one_rep, mc.cores = ncores, mc.set.seed = TRUE)
  res <- do.call(rbind, Filter(Negate(is.null), res))
  saveRDS(list(res = res, C = C, cells = cells, muhat_c = muhat_c, alpha = alpha, beta = beta,
               phi = phi, lambda_true = lambda_true, fixefCov = cf$fixefCov, S = S, K = K), rds)
  cat("replicates:", nrow(res), " time:", format(Sys.time() - t0), "\n")
}
res <- readRDS(rds)$res
nr <- nrow(res)

## ---- predictions of the linearised (GLS) calibration, as in 02_term4_and_site_effect.R
Z <- model.matrix(~ 0 + site)
X.pv <- cbind(1, cdat$mean_source_value)
Sy <- beta^2 * Z %*% C[seq_len(S), seq_len(S)] %*% t(Z) +
  (unname(cf$calib_fit$lambda[2]) + 1e-6) * tcrossprod(Z) + phi * diag(nrow(Z))
Si_X <- solve(Sy, X.pv)
V_gls <- solve(crossprod(X.pv, Si_X))
G <- V_gls %*% t(Si_X)
G_spamm <- spaMM::get_matrix(cf$calib_fit, which = "fixef_left_ginv", X.pv = X.pv)
covmat <- C[S + seq_len(K), seq_len(S)][, as.integer(site)] # Cov(d_cell, d_rows)
x_c <- rbind(1, muhat_c)

## ---- 1. sampling covariance of (intercept, slope)
e_theta <- cbind(res[, "ahat"] - alpha, res[, "bhat"] - beta)
cat("\n### Calibration coefficients over", nr, "refits\n")
cat("bias:", round(colMeans(e_theta), 4), "\n")
cat("empirical covariance:\n"); print(cov(e_theta))
cat("mean fixefCov reported by calibfit (Hessian):\n")
print(matrix(colMeans(res[, c("V11", "V12", "V12", "V22")]), 2))
cat("GLS prediction:\n"); print(V_gls)
cat("site variance: true", lambda_true, " mean estimate", round(mean(res[, "lambda_site"]), 3),
    " | phi: true", round(phi, 3), " mean estimate", round(mean(res[, "phi"]), 3), "\n")

## ---- 2. term 4: Var contribution 2 Cov(-d_c, -(e_alpha + e_beta mu_c) / beta)
d_c <- res[, paste0("d_c", seq_len(K)), drop = FALSE]
lin <- e_theta %*% x_c # R x K: e_alpha + e_beta * muhat_c
t4_emp <- 2 / beta * sapply(seq_len(K), function(k) cov(d_c[, k], lin[, k]))
t4_emp_se <- 2 / beta * sapply(seq_len(K), function(k) sd((d_c[, k] - mean(d_c[, k])) * (lin[, k] - mean(lin[, k]))) / sqrt(nr))
t4_gls <- -2 * colSums(t(covmat %*% t(G)) * x_c)
t4_isofind <- -beta * colSums(t(covmat %*% t(G_spamm)) * x_c)
t4 <- data.frame(cell = c(rep("calibration site", S), "predVar p50", "predVar p90", "predVar max"),
                 empirical = t4_emp, se = t4_emp_se, derived_gls = t4_gls, isofind = t4_isofind)
cat("\n### Term 4 (empirical from refits vs formulas)\n")
print(rbind(aggregate(cbind(empirical, se, derived_gls, isofind) ~ cell, t4, mean)), digits = 3)
cat("regression through the origin across cells, empirical ~ formula:\n")
cat("  derived (GLS):", round(coef(lm(empirical ~ 0 + derived_gls, t4)), 3),
    "  isofind:", round(coef(lm(empirical ~ 0 + isofind, t4)), 3), "\n")
write.csv(t4, file.path(out_dir, "03_term4_validation.csv"), row.names = FALSE)
