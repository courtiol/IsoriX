# Isoscape Computation and Inference of Spatial Origins using Mixed Models

IsoriX can be used for building isoscapes using mixed models and
inferring the geographic origin of organisms based on their isotopic
signature. This package is essentially a simplified interface combining
several other packages which implements the statistical framework
proposed by Courtiol & Rousset 2017. It uses the package spaMM for
fitting and predicting isoscapes, and for performing the assignment.
IsoriX also heavily relies on the package rasterVis for plotting the
maps produced with the package terra using the powerful package lattice
visualization system.

## Details

Below, we describe briefly the main steps of the workflow that aims at
performing the construction of an isoscape and the assignment of
organisms of unknown geographic origin(s) based on their isotopic
signature. We advise you to also read the detailed book chapter we wrote
(in press), as well as our [online
documentation](https://courtiol-isorix.share.connect.posit.cloud/),
which essentially cover the same material in a more detailed manner. You
should also read the dedicated help pages of the functions you are
using.

The statistical methods will not be detailed in this document but
information on the computation of isoscapes is available in Courtiol &
Rousset 2017, and information on the calibration and assignment in the
appendix of Courtiol et al. 2019.

1.  Fitting the isoscape model with
    [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md):

    The function
    [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)
    fits a geostatistical model, which approximates the relationship
    between the topographic features of a location and its isotopic
    signature (see
    [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md)
    for details). The model fits observations of isotopic delta values
    at several geographic locations (hereafter, called *sources*). One
    common type of sources used in ecology is the delta values for
    hydrogen in precipitation water collected at weather stations, but
    one may also use measurements performed on sedentary organisms. In
    either case, the accuracy of the isoscape (and thereby the accuracy
    of assignments) increases with the number and spatial coverage of
    the sources. The function
    [`isofit`](https://courtiol.github.io/IsoriX/reference/isofit.md) is
    designed to fit the model on data aggregated per location across all
    measurements. If instead you want to fit the model on measurements
    split per time intervals (e.g. per month), within each location, you
    should use the alternative function
    [`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md).
    Either way the data must be prepared using the function
    [`prepsources`](https://courtiol.github.io/IsoriX/reference/prepsources.md).

2.  Preparing the structural raster with
    [`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md):

    Building isoscapes and assigning organisms to their origin requires
    an adequate structural raster, i.e. a matrix representing a spatial
    grid. The function
    [`prepraster`](https://courtiol.github.io/IsoriX/reference/prepraster.md)
    allows restricting the extent of the raster to the area covered by
    isoscape data (in order to avoid extrapolation) and to reduce the
    resolution of the original structural raster (in order to speed up
    computation in all following steps). Note that aggregating the
    raster may lead to different results for the assignment, if the
    structural raster is used to define a covariate. This is because the
    values of raster cells changes depending on the aggregation
    function, which in turn will affect model predictions.

    We provide the function
    [`getelev`](https://courtiol.github.io/IsoriX/reference/getelev.md)
    to download an elevation raster for the entire world at a resolution
    of one altitude per square-km, and other rasters may be used. Such
    an elevation raster can be used as a structural raster. We have also
    stored a low resolution raster for Germany in our package (see
    [`ElevRasterDE`](https://courtiol.github.io/IsoriX/reference/ElevRasterDE.md))
    for users to try things out, but we do not encourage its use for
    real application.

3.  Predicting the isoscape across the area covered by the elevation
    raster with
    [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md):

    The function
    [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
    generates the isoscapes: it uses the fitted geostatistical models to
    predict the isotopic values (and several variances associated to
    those) for each raster cell defined by the structural raster. If the
    model has been fitted with
    [`isomultifit`](https://courtiol.github.io/IsoriX/reference/isomultifit.md),
    you should use the alternative function
    [`isomultiscape`](https://courtiol.github.io/IsoriX/reference/isomultiscape.md)
    to generate the isoscape.

    Our package allows the production of fine-tuned isoscape figures
    (using the function
    [`plot.ISOSCAPE`](https://courtiol.github.io/IsoriX/reference/plots.md)).
    Alternatively, the isoscape rasters can be exported as ascii raster
    and edited in any Geographic Information System (GIS) software (see
    [`isoscape`](https://courtiol.github.io/IsoriX/reference/isoscape.md)
    and the online documentation for details).

4.  Fitting the calibration model with
    [`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md):

    In most cases, organisms are of another kind than the sources used
    to build the isoscape (i.e. the isoscape is built on precipitation
    isotopic values and organisms are not water drops, but e.g. the fur
    of some bats). In such a case, the hydrogen delta values of the
    sampled organisms were modulated by their distinct physiology and do
    not directly correspond to the isotopic signature of the sources. In
    this situation, one must use sedentary organisms to study the
    relationship between the isotopic values in organisms and that of
    their environment. The function
    [`calibfit`](https://courtiol.github.io/IsoriX/reference/calibfit.md)
    fits a statistical model on such a calibration dataset.

    If the isoscape is directly built from isotopic values of organisms,
    there is no need to fit a calibration model.

    The calibration fit can also be used to build a calibrated isoscape,
    i.e. to convert the isoscape of sources into an isoscape of sample
    values (e.g. of tissues), with
    [`calibscape`](https://courtiol.github.io/IsoriX/reference/calibscape.md).
    Such an isoscape can be plotted but cannot be used for assignments.

5.  Inferring spatial origins of samples with
    [`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md):

    The function
    [`isofind`](https://courtiol.github.io/IsoriX/reference/isofind.md)
    tests for each location across the isoscape if it presents a similar
    isotopic signature than the unknown origin of a given individual(s).
    This assignment procedure considered the some (but not all, see
    Courtiol et al. 2019) uncertainty stemming from the model fits
    (geostatistical models and calibration model). The function
    [`plot.ISOFIND`](https://courtiol.github.io/IsoriX/reference/plots.md)
    then draws such assignment by plotting the most likely origin with
    the prediction region around it. When several organisms are being
    assigned, both assignments at the level of each sample and a single
    assignment for the whole group can be performed.

## Note

Please note that the geographic coordinates (latitude, longitude) of any
spatial data (locations, rasters) must be given in decimal degrees
following the WGS84 spheroid standard.

## References

Courtiol A, Rousset F, Rohwäder M, Soto DX, Lehnert L, Voigt CC, Hobson
KA, Wassenaar LI & Kramer-Schadt S (2019). "Isoscape computation and
inference of spatial origins with mixed models using the R package
IsoriX." In Hobson KA, Wassenaar LI (eds.), Tracking Animal Migration
with Stable Isotopes, second edition. Academic Press, London, pp.
207-236.
[doi:10.1016/B978-0-12-814723-8.00009-X](https://doi.org/10.1016/B978-0-12-814723-8.00009-X)
.

Courtiol A, Rousset F (2017). "Modelling isoscapes using mixed models."
bioRxiv. doi: 10.1101/207662,
[link](https://www.biorxiv.org/content/10.1101/207662v1).

## See also

Useful links:

- <https://github.com/courtiol/IsoriX>

- <https://courtiol-isorix.share.connect.posit.cloud/>

- Report bugs at <https://github.com/courtiol/IsoriX/issues>

## Author

Alexandre Courtiol <alexandre.courtiol@gmail.com>,

François Rousset,

Marie-Sophie Rohwaeder,

Stephanie Kramer-Schadt <kramer@izw-berlin.de>
