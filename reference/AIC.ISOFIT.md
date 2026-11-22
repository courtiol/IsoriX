# Compute the conditional AIC of fitted isoscapes

This function computes the conditional AIC (cAIC) of the isoscape models
fitted with
[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) or
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
corrected for the number of parameters estimated in the residual
dispersion model. This corrected cAIC measures the ability of the joint
fit of the mean model and of the residual dispersion model to predict
the isotopic values observed at the source locations. It thus allows for
the comparison of isoscape models differing not only in the structure of
their mean model but also in the structure of their residual dispersion
model.

## Usage

``` r
# S3 method for class 'ISOFIT'
AIC(object, ..., k = 2)
```

## Arguments

- object:

  The output of a call to
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) or
  [`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md).

- ...:

  Optionally, more fitted objects created by
  [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) or
  [`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md).

- k:

  The penalty per parameter, which must be equal to 2 (the default)
  since the cAIC computed by spaMM does not allow for other values. This
  argument only exists for consistency with
  [`stats::AIC`](https://rdrr.io/r/stats/AIC.html).

## Value

If a single fitted object is provided, the corrected cAIC (a *numeric*
value). If several fitted objects are provided, a *data.frame* with one
row per object and the following columns: the number of parameters
estimated in the mean model (`K_mean`) and in the residual dispersion
model (`K_disp`), the cAIC of the mean model before correction
(`cAIC_uncorrected`) and the corrected cAIC (`cAIC`).

## Details

For linear mixed models whose residual errors have a variance (or
covariance) depending on \\r\\ unknown parameters, Overholser & Xu
(2014, theorem 1 and equation 6) showed that the plug-in estimate of the
cAIC is \\-2 \log L_c + 2 (\textrm{tr}(H) + r)\\, where \\\log L_c\\ is
the conditional log-likelihood and \\\textrm{tr}(H)\\ is the effective
number of degrees of freedom of the fixed and random effects (the trace
of the "hat" matrix). The cAIC computed by spaMM for the mean model
(`mean_fit`) is \\-2 \log L_c + 2 \textrm{tr}(H)\\: it treats the
residual variances (`pred_disp`) as known, while they have been
predicted from the fit of the residual dispersion model (`disp_fit`).
Following Courtiol & Rousset (2017), we thus add the missing term by
considering the parameters estimated in the residual dispersion model
(`K_disp`) as the parameters of the residual variance of the mean model
(i.e. \\r = K\_{disp}\\):

\$\$cAIC = cAIC\_{mean\\fit} + 2 \times K\_{disp}\$\$

`K_disp` is the number of fixed-effect coefficients, plus the number of
random-effect variances, plus the number of parameters of the Matérn
correlation function (including the nugget, if any), plus the number of
residual dispersion parameters that have been estimated in `disp_fit`
(see [`spaMM::DoF`](https://rdrr.io/pkg/spaMM/man/DoF.html)). Parameters
kept fixed during the fit (e.g. the residual variance of `disp_fit`,
which is fixed to its theoretical expectation) are not counted.

For a fit produced by
[`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
the cAIC is the sum of the corrected cAIC of all the pairs of models
(one per strata), since all pairs are fitted independently on
non-overlapping data. Similarly, `K_mean` and `K_disp` are summed over
all pairs.

As for any information criterion, only models fitted on the same data
can be compared. The lower the cAIC, the better the predictive power of
the fitted isoscape. Since the goal of an isoscape is prediction, there
is no need to favour simpler models beyond what the cAIC already
accounts for. Note, however, that the cAIC only reflects the quality of
the predictions at the source locations, so always check the isoscapes
produced by the selected model (e.g. by plotting the prediction
variance; see
[`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)).

This correction should be seen as an approximation. First, the result of
Overholser & Xu (2014) is asymptotic and the plug-in cAIC can be biased
in small samples (they also discuss alternative corrections for small
samples). Second, their result assumes that the parameters of the
residual variance are estimated jointly with the other parameters of the
model, whereas IsoriX estimates them in a separate fit (`disp_fit`).
Third, the residual variances predicted by `disp_fit` also depend on the
predicted values of its random effects, which are not counted in
`K_disp`. Finally, the cAIC has been shown to tend to favour models with
additional random effects (Greven & Kneib 2010; Overholser & Xu 2014),
so comparisons of models differing in their random-effect structure
should be interpreted with care.

## References

Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
models. <https://www.biorxiv.org/content/10.1101/207662v1>

Greven, S., Kneib, T. (2010). On the behaviour of marginal and
conditional AIC in linear mixed models. Biometrika, 97(4):773-789.
[doi:10.1093/biomet/asq042](https://doi.org/10.1093/biomet/asq042) .

Overholser, R., Xu, R. (2014). Effective degrees of freedom and its
application to conditional AIC for linear mixed-effects models with
correlated error structures. Journal of Multivariate Analysis,
132:160-170.
[doi:10.1016/j.jmva.2014.08.004](https://doi.org/10.1016/j.jmva.2014.08.004)
.

## See also

[`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) for
fitting the isoscape models

[`spaMM::AIC`](https://rdrr.io/pkg/spaMM/man/AIC.html) for the different
information criteria computed by spaMM on each of the two fits

[`spaMM::DoF`](https://rdrr.io/pkg/spaMM/man/DoF.html) for the
extraction of the number of estimated parameters

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 20) {
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  ## Fitting two isoscape models differing in their residual dispersion model
  GermanFit1 <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE)
  )

  GermanFit2 <- isofit(
    data = GNIPDataDEagg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE),
    disp_model_fix = list(elev = TRUE)
  )

  ## Corrected cAIC of a single fit
  AIC(GermanFit1)

  ## Comparison of the two fits
  AIC(GermanFit1, GermanFit2)
}
```
