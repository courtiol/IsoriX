# Fit the isoscape models

This function fits the aggregated source data using mixed models. The
fitting procedures are done by the package
[`spaMM::spaMM`](https://rdrr.io/pkg/spaMM/man/spaMM.html) which we use
to jointly fit the mean isotopic values and their associated residual
dispersion variance in a spatially explicit manner.

## Usage

``` r
isofit(
  data,
  mean_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE,
    long_2 = FALSE),
  disp_model_fix = list(elev = FALSE, lat_abs = FALSE, lat_2 = FALSE, long = FALSE,
    long_2 = FALSE),
  mean_model_rand = list(uncorr = TRUE, spatial = TRUE),
  disp_model_rand = list(uncorr = TRUE, spatial = TRUE),
  uncorr_terms = list(mean_model = "lambda", disp_model = "lambda"),
  spaMM_method = list(mean_model = "fitme", disp_model = "fitme"),
  dist_method = "Earth",
  control_mean = list(),
  control_disp = list(),
  verbose = interactive()
)
```

## Source

<https://gitlab.in2p3.fr/francois.rousset/spamm-ref>

## Arguments

- data:

  The *dataframe* containing the data used for fitting the isoscape
  model

- mean_model_fix:

  A *list* of *logical* indicating which fixed effects to consider in
  mean_fit

- disp_model_fix:

  A *list* of *logical* indicating which fixed effects to consider in
  disp_fit

- mean_model_rand:

  A *list* of *logical* indicating which random effects to consider in
  mean_fit

- disp_model_rand:

  A *list* of *logical* indicating which random effects to consider in
  disp_fit

- uncorr_terms:

  A *list* of two strings defining the parametrization used to model the
  uncorrelated random effects for mean_fit and disp_fit

- spaMM_method:

  A *list* of two strings defining the spaMM functions used for mean_fit
  and disp_fit

- dist_method:

  A *string* indicating the distance method

- control_mean:

  A *list* of additional arguments to be passed to the call of mean_fit

- control_disp:

  A *list* of additional arguments to be passed to the call of disp_fit

- verbose:

  A *logical* indicating whether information about the progress of the
  procedure should be displayed or not while the function is running. By
  default verbose is `TRUE` if users use an interactive R session and
  `FALSE` otherwise.

## Value

This function returns a *list* of class *ISOFIT* containing two
inter-related fits: `mean_fit` and `disp_fit`. The returned *list* also
contains the object `info_fit` that contains all the call arguments.

## Details

The detailed statistical definition of the isoscape model is described
in Courtiol & Rousset 2017 and summarized in Courtiol et al. 2019.

Briefly, the fitting procedure of the isoscape model is divided into two
fits: `mean_fit` and `disp_fit`. `mean_fit` corresponds to the fit of
the "mean model", which we will use to predict the mean isotopic values
at any location in other functions of the package. `disp_fit`
corresponds to the fit of the "residual dispersion model", which we will
use to predict the residual dispersion variance associated to the mean
predictions. `mean_fit` is a linear mixed-effects model (LMM) with fixed
effects, an optional spatial random effect with a Matérn correlation
structure and an optional uncorrelated random effect accounting for
variation between sources unrelated to their location. `disp_fit` is a
Gamma Generalized LMM (Gamma GLMM) that also has fixed effects, an
optional spatial random effect with a Matérn correlation structure and
an optional uncorrelated random effect. For the GLMM the residual
variance is fixed to its theoretical expectation.

The *dataframe* `data` must contain a single row per source location
with the following columns: `mean_source_value` (the mean isotopic
value), `var_source_value` (the unbiased variance estimate of the
isotopic value at the location), `n_source_value` (the number of
measurements performed at the location, could be 1) and `source_ID` (a
factor defining the identity of the sources at a given location).

The arguments `mean_model_fix` and `disp_model_fix` allow the user to
choose among different fixed-effect structures for each model. These
arguments are lists of booleans (`TRUE` or `FALSE`), which define which
of the following fixed effects must be considered: the elevation
(`elev`), the absolute value of the latitude (`lat_abs`), the squared
latitude (`lat_2`), the longitude (`long`) and the squared longitude
(`long_2`). An intercept is always considered in both models. By
default, none of these fixed effects is considered. This default is a
convenient starting point but it is probably not the best model for your
data, so you should compare alternative fixed-effect structures (see the
[online
documentation](https://courtiol-isorix.share.connect.posit.cloud/isoscape.html#modelselection)
for an example). Note that fixed-effect structures should be compared
using the conditional AIC, which measures the predictive power of the
fitted isoscape and which can be computed by calling
[`AIC()`](https://courtiol.github.io/IsoriX/reference/AIC.ISOFIT.md) on
the output of `isofit()`. To remind users of all this, a message is
displayed the first time the default model is fitted during an R
session.

In the models, the mean (for the mean model) or the log residual
variance (for the residual dispersion model) follow a Gaussian
distribution around a constant value. The arguments `mean_model_rand`
and `disp_model_rand` allow to choose among different random effects for
each model influencing the realizations of these Gaussian random
processes. For each model one can choose not to include random effects
or to include an uncorrelated random effect, a spatial random effect, or
both (default). Setting `"uncorr" = TRUE` implies that the realizations
of the random effect differ between sources for reasons that have
nothing to do with the relative geographic location (e.g. some
micro-climate or some measurement errors trigger a shift in all
measurements (mean model) or a shift in the variance between
measurements (residual dispersion model) performed at a given source by
the same amount). Setting `"spatial" = TRUE` (default) implies that the
random realizations of the Gaussian process follow a Matérn correlation
structure. Put simply, this implies that the closer two locations are,
the more similar the means (or the log residual variance) in isotopic
values are (e.g. because they are likely to be traversed by the same air
masses).

The arguments `uncorr_terms` allow the choice between two alternative
parametrizations for the uncorrelated random effect in the fits:
`"lambda"` or `"nugget"` for each model. When using `"lambda"`, the
variance of the uncorrelated random terms is classically modelled by a
variance. When a spatial random effect is considered, one can
alternatively choose `"nugget"`, which modifies the Matérn correlation
value when distance between location tends to zero. If no random effect
is considered, one should stick to the default setting and it will be
ignored by the function. The choice of the parametrization is a matter
of personal preferences and it does not change the underlying models, so
the estimations for all the other parameters of the models should not be
impacted by whether one chooses `"lambda"` or `"nugget"`. However, only
uncertainty in the estimation of `"lambda"` can be accounted for while
computing prediction variances, which is why we chose this alternative
as the default. Depending on the data one parametrization may lead to
faster fit than the other.

The argument `spaMM_method` is also a list of two *strings* where the
first element defines the spaMM functions used for fitting the mean
model and the second element defines the spaMM method used for fitting
the residual dispersion model. The possible options are `"HLfit"`,
`"corrHLfit"` and `"fitme"`. Note that `"HLfit"` shall only be used in
the absence of a Matérn correlation structure and `"corrHLfit"` shall
only be used in the presence of it. In contrast, `"fitme"` should work
in all situations. Which method is best remains to be determined and it
is good practice to try different methods (if applicable) to check for
the robustness of the results. If all is well one should obtain very
similar results with the different methods. If this is not the case,
carefully check the model output to see if one model fit did not get
stuck at a local minimum during optimization (which would translate in a
lower likelihood, or weird isoscapes looking flat with high peaks at
very localised locations).

The argument `dist_method` allows modifying how the distance between
locations is computed to estimate the spatial correlation structure. By
default, we consider the so-called "Earth" distances which are
technically called orthodromic distances. They account for earth
curvature. The alternative "Euclidean" distances do not. For studies
performed on a small geographic scale, both distance methods should lead
to similar results.

If the sources straddle the antimeridian (i.e. the International Date
Line, 180 degrees), e.g. for studies centred on the Pacific, their
longitudes are automatically expressed within 0 and 360 (instead of -180
and 180) so that the area they cover is continuous (a message is then
displayed). This matters for fixed effects involving longitude, for
cropping the structural raster (see
[`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md))
and for plotting. The "Earth" distances are not affected by this change
of representation.

The arguments `control_mean` and `control_dist` are lists that are
transmitted to the
[`spaMM::spaMM`](https://rdrr.io/pkg/spaMM/man/spaMM.html) fitting
functions (defined by `spaMM_method`). These lists can be used to finely
control the fitting procedure, so advanced knowledge of the package
[`spaMM::spaMM`](https://rdrr.io/pkg/spaMM/man/spaMM.html) is required
before messing around with these inputs.

We highly recommend users to examine the output produced by isofit.
Sometimes, poor fit may occur and such models should therefore not be
used for building isoscapes or performing assignments.

## Note

There is no reason to restrict `mean_fit` and `disp_fit` to using the
same parametrization for fixed and random effects.

Never use a mean_fit object to draw predictions without considering a
disp_fit object: mean_fit is not fitted independently from disp_fit.

For all methods, fixed effects are being estimated by Maximum Likelihood
(ML) and dispersion parameters (i.e. random effects and Matérn
correlation parameters) are estimated by Restricted Maximum Likelihood
(REML). Using REML provides more accurate prediction intervals but
impedes the accuracy of Likelihood Ratio Tests (LRT). Our choice for
REML was motivated by the fact that our package is more likely to be
used for drawing inferences than null hypothesis testing. Users
interested in model comparisons may rely on the conditional AIC values
that can be extracted from fitted models using
[`AIC()`](https://courtiol.github.io/IsoriX/reference/AIC.ISOFIT.md)
(e.g. `AIC(fit1, fit2)`). Unlike the conditional AIC computed by
[`spaMM::AIC`](https://rdrr.io/pkg/spaMM/man/AIC.html) on `mean_fit`,
this value is corrected for the number of parameters estimated in the
residual dispersion model, so it can also be used to compare models
differing in their residual dispersion model.

Variable names for `data` must be respected to ensure a correct
utilization of this package. Alteration to the fixed effect structure is
not implemented so far (beyond the different options proposed) to avoid
misuse of the package. Users that would require more flexibility should
consider using spaMM directly (see Courtiol & Rousset 2017) or let us
know which other covariates would be useful to add in IsoriX.

## References

Courtiol, A., Rousset, F. (2017). Modelling isoscapes using mixed
models. <https://www.biorxiv.org/content/10.1101/207662v1>

Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson
KA, Wassenaar LI & Kramer-Schadt S (2019). Isoscape computation and
inference of spatial origins with mixed models using the R package
IsoriX. In Hobson KA & Wassenaar LI (eds.), Tracking Animal Migration
with Stable Isotopes, second edition. Academic Press, London, pp.
207-236.
[doi:10.1016/B978-0-12-814723-8.00009-X](https://doi.org/10.1016/B978-0-12-814723-8.00009-X)
.

Rousset, F., Ferdy, J. B. (2014). Testing environmental and genetic
effects in the presence of spatial autocorrelation. Ecography,
37(8):781-790.

Bowen, G. J., Wassenaar, L. I., Hobson, K. A. (2005). Global application
of stable hydrogen and oxygen isotopes to wildlife forensics. Oecologia,
143(3):337-348.

## See also

[`AIC.ISOFIT`](https://courtiol.github.io/IsoriX/reference/AIC.ISOFIT.md)
for comparing fitted isoscape models

[`spaMM::spaMM`](https://rdrr.io/pkg/spaMM/man/spaMM.html) for an
overview of the spaMM package

[`spaMM::fitme`](https://rdrr.io/pkg/spaMM/man/fitme.html) and
[`spaMM::corrHLfit`](https://rdrr.io/pkg/spaMM/man/corrHLfit.html) for
information about the two possible fitting procedures that can be used
here

[`spaMM::MaternCorr`](https://rdrr.io/pkg/spaMM/man/MaternCorr.html) for
information about the Matérn correlation structure

[`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md)
for the function preparing the data for isofit

## Examples

``` r

## The examples below will only be run if sufficient time is allowed
## You can change that by typing e.g. options_IsoriX(example_maxtime = XX)
## if you want to allow for examples taking up to ca. XX seconds to run
## (so don't write XX but put a number instead!)

if (getOption_IsoriX("example_maxtime") > 10) {
  ## Fitting the models for Germany
  GNIPDataDEagg <- prepsources(data = GNIPDataDE)

  GermanFit <- isofit(data = GNIPDataDEagg, mean_model_fix = list(elev = TRUE, lat_abs = TRUE))

  GermanFit

  ## Diagnostics for the fits
  plot(GermanFit)

  ## Exploration of the fitted models
  GermanFit$mean_fit
  GermanFit$disp_fit

  ## Conditional AIC (corrected for the residual dispersion model)
  AIC(GermanFit)
}
```
