## Simulation 2: term 4 (covariance isoscape prediction / calibration) and site effects
##
## Conditional on the fitted German isoscape (GermanFit) and on the design of
## CalibDataAlien, we simulate:
##   - prediction errors d of the isoscape jointly at the 50 calibration sites
##     and at candidate origin cells (from spaMM's joint predVar covariance),
##   - calibration data y = alpha + beta * mu_true(site) + b_site + eps,
##   - the calibration estimates as the GLS fit used by calibfit "wild"
##     (linearised: theta_hat = theta + G (y - X theta), G = fixef_left_ginv),
##   - groups of n animals truly originating from a candidate cell.
## We then compare the empirical variance of the assignment statistic with the
## terms used by isofind(), and the size of group tests at the true origin.
## Scenarios: no site effect (as in CalibDataAlien), and a site random effect
## worth 40% of the animal-level variance (as estimated on CalibDataBat and
## CalibDataBat2), with groups coming either from one site or from many sites.
##
## Caveat: the calibration estimator is linearised (GLS with known variance
## components); 03_validate_calibfit.R checks this against real calibfit() refits.
##
## Run from the root of the repository:
##   Rscript material_for_maintainer/simulations_issue_64/02_term4_and_site_effect.R
## Runtime: ca. 3 min.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
set.seed(64)
out_dir <- "material_for_maintainer/simulations_issue_64/results"
dir.create(out_dir, showWarnings = FALSE)
## (spaMM fits cannot be cached with saveRDS: external pointers are lost)
fit <- isofit(data = prepsources(data = GNIPDataDE), mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
g <- list(fit = fit, scape = isoscape(raster = ElevRasterDE, isofit = fit))
g$cal <- list(Alien = calibfit(CalibDataAlien, isofit = fit))
R <- 20000

mean_fit <- g$fit$mean_fit
cf <- g$cal$Alien
cfit <- cf$calib_fit
alpha <- cf$param[["intercept"]]
beta <- cf$param[["slope"]]
phi <- cf$phi
lambda_site_fit <- unname(cfit$lambda[2])
cdat <- cfit$data
site <- factor(cdat$site_ID)
Z <- model.matrix(~ 0 + site) # rows -> sites
first <- match(levels(site), site)

## ---- candidate cells: cell containing each calibration site + predVar quantile cells
xs <- attr(g$scape, "xs")
mu_r <- g$scape$isoscapes$mean
cell_sites <- terra::cellFromXY(mu_r, as.matrix(cdat[first, c("long", "lat")]))
pv <- terra::values(g$scape$isoscapes$mean_predVar)[, 1]
q_cells <- sapply(c(0.1, 0.5, 0.9, 1), function(q) which.min(abs(pv - quantile(pv, q, na.rm = TRUE))))
cells <- c(cell_sites, q_cells)
cell_type <- c(rep("calibration site", length(cell_sites)), paste0("predVar ", c("p10", "p50", "p90", "max")))
K <- length(cells)
xs_c <- xs[cells, ]
muhat_c <- terra::values(mu_r)[cells, 1]

## ---- joint prediction covariance of isoscape errors at [sites, cells]
cols <- names(xs)
nd <- rbind(cdat[first, cols], xs_c[, cols])
pr <- spaMM::predict.HLfit(mean_fit, newdata = nd, variances = list(predVar = TRUE, cov = TRUE))
C <- attr(pr, "predVar")
S <- nlevels(site)
iS <- seq_len(S)
iC <- S + seq_len(K)

## ---- isofind's own term-4 ingredients (copied from R/isofind.R)
X.pv <- cbind(1, cdat$mean_source_value)
G_spamm <- spaMM::get_matrix(cfit, which = "fixef_left_ginv", X.pv = X.pv)
## isofind computes covmat with spaMM::get_predCov_var_fix(), which currently
## errors ("non-conformable arrays", spaMM 4.7.17) so that
## isofind(neglect_covPredCalib = FALSE) fails. We try a few variants to locate
## the cause, and otherwise use the joint predVar, which is the same quantity.
covmat_C <- C[iC, iS][, as.integer(site)]
try_cov <- function(label, newdata, fixdata) {
  r <- tryCatch({
    fixc <- spaMM::preprocess_fix_corr(mean_fit, fixdata = fixdata)
    spaMM::get_predCov_var_fix(mean_fit, newdata = newdata, fix_X_ZAC.object = fixc)
  }, error = function(e) e)
  cat("get_predCov_var_fix,", label, ":", if (inherits(r, "error")) paste("ERROR", conditionMessage(r)) else "OK", "\n")
  r
}
fd_chr <- cdat; fd_chr$source_ID <- as.character(fd_chr$source_ID)
nd_chr <- xs_c; nd_chr$source_ID <- as.character(nd_chr$source_ID)
fd_new <- cdat; fd_new$source_ID <- factor(paste0("calib_", cdat$site_ID))
variants <- list(
  as_in_isofind = try_cov("as in isofind (full grid)", xs, cdat),
  character_IDs = try_cov("character source_ID", nd_chr, fd_chr),
  distinct_IDs = try_cov("calibration source_ID not shared with grid", xs_c, fd_new)
)
ok <- Filter(function(x) !inherits(x, "error"), variants)
chk_cov <- NA
if (length(ok)) {
  cm <- ok[[1]]
  if (nrow(cm) == nrow(xs)) cm <- cm[cells, ]
  chk_cov <- max(abs(cm - covmat_C)) / max(abs(covmat_C))
}
covmat <- covmat_C
t4_isofind <- drop((-beta * covmat) %*% t(G_spamm) %*% rbind(1, 0)) +
  drop((-beta * covmat) %*% t(G_spamm) %*% rbind(0, 1)) * muhat_c


## ---- GLS for the calibration (variance components fixed)
gls <- function(lambda_site) {
  Sy <- beta^2 * Z %*% C[iS, iS] %*% t(Z) + (lambda_site + 1e-6) * tcrossprod(Z) + phi * diag(nrow(Z))
  Si_X <- solve(Sy, X.pv)
  Vb <- solve(crossprod(X.pv, Si_X))
  list(G = Vb %*% t(Si_X), V = Vb)
}
g_fit <- gls(lambda_site_fit)
chk_G <- max(abs(g_fit$G - G_spamm)) / max(abs(G_spamm))

## term 4 as derived for s = mean_origin - isoscape mean:
##   2 Cov(-d_c, -(e_alpha + e_beta mu_c)/beta) = -2 Cov(d_c, d_rows) G' x
t4_derived <- function(G) drop(-2 * covmat %*% t(G) %*% rbind(1, 0)) +
  drop(-2 * covmat %*% t(G) %*% rbind(0, 1)) * muhat_c

## ---- Monte Carlo
cauchy <- function(p) pcauchy(rowMeans(ifelse(p < 1e-15, 1 / (p * pi), tan((0.5 - p) * pi))), lower.tail = FALSE)
fisher <- function(p) pchisq(-2 * rowSums(log(p)), df = 2 * ncol(p), lower.tail = FALSE)
pfun <- function(s, v) pmax(2 * pnorm(-abs(s) / sqrt(v)), 1e-300)

run <- function(lambda_true, n_set = c(1, 5, 10, 30), same_site = TRUE) {
  gg <- gls(lambda_true)
  G <- gg$G
  V <- gg$V
  t4d <- t4_derived(G)
  ## joint draws of isoscape errors at sites and cells
  L <- chol(C + diag(1e-9, nrow(C)))
  d <- matrix(rnorm(R * nrow(C)), R) %*% L
  d_rows <- d[, iS][, as.integer(site)]
  b_rows <- (matrix(rnorm(R * S, 0, sqrt(lambda_true)), R) )[, as.integer(site)]
  eps <- matrix(rnorm(R * nrow(Z), 0, sqrt(phi)), R)
  e_theta <- (-beta * d_rows + b_rows + eps) %*% t(G) # R x 2
  ahat <- alpha + e_theta[, 1]
  bhat <- beta + e_theta[, 2]
  out <- list()
  for (k in seq_len(K)) {
    mu_true <- muhat_c[k] - d[, iC[k]]
    t1 <- C[iC[k], iC[k]]
    for (n in n_set) {
      b_new <- if (same_site) matrix(rnorm(R, 0, sqrt(lambda_true)), R, n) else matrix(rnorm(R * n, 0, sqrt(lambda_true)), R, n)
      y <- alpha + beta * mu_true + b_new + matrix(rnorm(R * n, 0, sqrt(phi)), R, n)
      mo <- (y - ahat) / bhat
      s <- mo - muhat_c[k]
      t3 <- (V[1, 1] + 2 * V[1, 2] * mo + V[2, 2] * mo^2) / bhat^2
      vA <- t1 + phi / bhat^2 + t3 # isofind default
      vB <- vA + t4_isofind[k] # isofind with neglect_covPredCalib = FALSE
      vC <- t1 + (phi + lambda_true) / bhat^2 + t3 + t4d[k] # corrected
      pA <- pfun(s, vA); pB <- pfun(s, vB); pC <- pfun(s, vC)
      ## mean test with the corrected covariance (shared vs independent parts)
      t3c <- (V[1, 1] + 2 * V[1, 2] * muhat_c[k] + V[2, 2] * muhat_c[k]^2) / bhat^2
      shared <- t1 + t3c + t4d[k] + if (same_site) lambda_true / bhat^2 else 0
      indep <- (phi + if (same_site) 0 else lambda_true) / bhat^2
      z <- rowMeans(s) / sqrt(shared + indep / n)
      rej <- c(
        single_default = mean(pA[, 1] < 0.05),
        single_term4 = mean(pB[, 1] < 0.05),
        single_corrected = mean(pC[, 1] < 0.05),
        Fisher_default = mean(fisher(pA) < 0.05),
        Fisher_corrected = mean(fisher(pC) < 0.05),
        Cauchy_corrected = mean(cauchy(pC) < 0.05),
        mean_corrected = mean(2 * pnorm(-abs(z)) < 0.05)
      )
      out[[length(out) + 1]] <- data.frame(
        lambda_true = lambda_true, same_site = same_site, cell = k, type = cell_type[k], n = n,
        var_emp = var(s[, 1]), t1 = t1, t2 = mean(phi / bhat^2), t3 = mean(t3[, 1]),
        t4_isofind = t4_isofind[k], t4_derived = t4d[k],
        method = names(rej), rej05 = unname(rej), row.names = NULL
      )
    }
  }
  do.call(rbind, out)
}

lambda_bat <- phi * 0.4 / 0.6 # site share of 40%, as in CalibDataBat / CalibDataBat2
res <- rbind(
  run(0),
  run(lambda_bat, same_site = TRUE),
  run(lambda_bat, same_site = FALSE)
)
## ---- summary ----
cat("relative difference between GLS G and spaMM fixef_left_ginv:", signif(chk_G, 3), "\n")
cat("fixefCov from calibfit (Hessian):\n"); print(cf$fixefCov)
cat("fixefCov from GLS with known variance components:\n"); print(g_fit$V)
v <- unique(res[res$lambda_true == 0 & res$n == 1, c("type", "var_emp", "t1", "t2", "t3", "t4_isofind", "t4_derived")])
v$type[v$type == "calibration site"] <- "calibration sites (mean)"
v <- aggregate(. ~ type, v, mean)
v$var_emp_minus_t123 <- v$var_emp - v$t1 - v$t2 - v$t3
print(v, digits = 3)
write.csv(v, file.path(out_dir, "02_variance_terms.csv"), row.names = FALSE)
res$type[res$type == "calibration site"] <- "calibration sites (mean)"
res$scenario <- with(res, ifelse(lambda_true == 0, "no site effect",
  ifelse(same_site, "site effect, group from one site", "site effect, group from many sites")))
agg <- aggregate(rej05 ~ method + n + type + scenario, res, mean)
agg$method <- factor(agg$method, unique(res$method))
write.csv(agg, file.path(out_dir, "02_size.csv"), row.names = FALSE)
for (sc in unique(agg$scenario)) {
  cat("\n###", sc, "- rejection rate of the true origin at alpha = 0.05\n")
  print(round(xtabs(rej05 ~ method + n + type, agg,
                    subset = scenario == sc & type %in% c("calibration sites (mean)", "predVar p50", "predVar max")), 3))
}
