# Simulations for issue #64 (group assignment in `isofind()`)

These scripts study how `isofind()` combines the per-sample p-value maps into a
group assignment, and the variance terms the assignment test relies on.
See https://github.com/courtiol/IsoriX/issues/64.

All scripts are run from the root of the repository and load the package with
`pkgload::load_all(".")`. Results are written to `results/`.

| script | what it does | runtime |
|---|---|---|
| `01_combination_methods.R` | Size and power of group tests (Fisher, joint Gaussian chi-square, GLS mean, Cauchy, raw harmonic mean p) under the variance model of `isofind()` (terms 1-3), with optional extra correlation among animals | ca. 4 min |
| `02_term4_and_site_effect.R` | Mechanistic simulation of the isoscape prediction errors jointly at the calibration sites and candidate cells, of the calibration data, and of the (linearised) calibration fit; checks term 4 and the calibration site random effect | ca. 3 min |
| `03_validate_calibfit.R` | Same as above but refitting `calibfit()` on each simulated calibration dataset, to validate the linearisation used in script 02 | ca. 75 min on 4 cores (skipped if `results/03_validate_lambda0.rds` exists) |

All simulations are conditional on the German example (`GNIPDataDE`,
`ElevRasterDE`, `CalibDataAlien`): the fitted isoscape is treated as the
predictive distribution of the true isoscape. A fully unconditional
simulation (simulate a true isoscape, sample sources, refit everything) would
need a replacement for `isosim()`, whose dependency `RandomFields` is retired.

## Main findings

1. **Fisher's method is anti-conservative for groups** because all animals
   share the isoscape prediction error at the candidate cell (term 1) and the
   error on the calibration coefficients (term 3). At the true origin, the
   rejection rate at alpha = 0.05 reaches ca. 0.10 (median cell) and 0.21 (worst
   predicted cell) for n = 30, before considering any site effect.
2. **A test on the group mean is exact and cheap.** Under H0 all animals share
   the origin, so the covariance of the statistics is compound symmetric and
   the GLS test reduces to
   `z = mean(s_i) / sqrt(shared + independent / n)`,
   with `shared = term1 + term3(isoscape mean) + term4` and
   `independent = phi / slope^2`. It matches Fisher's apparent power at the
   correct size. The Cauchy combination stays within ca. 0.06 whatever the
   dependence, without modelling it.
3. **The calibration site random effect is ignored by `isofind()`.** The
   "wild" calibration fits `(1|site_ID)`, but only `calibfit$phi` enters term 2.
   On the real bat calibrations (`CalibDataBat`, `CalibDataBat2`, fitted on
   `GermanFit`) the site variance is ca. 40% of the animal-level variance
   (lambda = 55.2 vs phi = 84.9; 40.8 vs 59.1). If that is real, single-sample
   tests reject the true origin ca. 10% of the time instead of 5%, and Fisher's
   group test up to ca. 40% for n = 30. The site variance should be added to
   term 2, and treated as shared when the group comes from a single site.
4. **Term 4 cannot currently be used**: `isofind(neglect_covPredCalib = FALSE)`
   fails on the package example with spaMM 4.7.17
   (`spaMM::get_predCov_var_fix()`: "non-conformable arrays").
   Term 4 is negative (the wild calibration absorbs part of the isoscape
   error), so neglecting it is conservative: at the worst predicted cell,
   single-sample tests reject the true origin ca. 2% of the time instead of 5%.
   In the linearised simulation, the empirical variance matches
   `term4 = -2 * Cov(d_cell, d_calib) %*% t(G) %*% c(1, mu_cell)`
   (G = GLS left generalised inverse of the calibration), while the formula in
   `isofind()` gives about half of that. Script 03 checks this with real
   `calibfit()` refits.
