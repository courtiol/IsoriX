#' isoscape Computation and Inference of Spatial Origins using Mixed Models
#' 
#' \pkg{IsoriX} can be used for building isoscapes using mixed models and
#' inferring the geographic origin of organisms based on their isotopic
#' signature. This package is essentially a simplified interface combining
#' several other packages. It uses the package \pkg{\link[spaMM]{spaMM}} for
#' fitting and predicting isoscapes, and for performing the assignment.
#' \pkg{IsoriX} also heavily relies on the package
#' \pkg{\link[rasterVis:rasterVis-package]{rasterVis}} for plotting the maps
#' using the powerful \pkg{\link[lattice:Lattice]{lattice}} visualization
#' system.
#' 
#' We describe below, step-by-step, the general work-flow that aims at
#' performing the construction of an isoscape and the assignment of organisms
#' of unknown geographic origin(s) based on their isotopic signature. We advise
#' to also read all dedicated help pages of functions mentioned hereafter.
#' 
#' The statistical methods will not be detailed in this document but should
#' soon be available as a vignette, a publication, or both (we are currently
#' working on it).
#' 
#' \enumerate{ \item Fitting the isoscape model with \code{\link{isofit}}:
#' 
#' The function \code{\link{isofit}} fits a geostatistical model, which
#' approximates the relationship between the topographic features of a location
#' and its isotopic signature (see \code{\link{isofit}} for details). The model
#' fits observations of isotopic delta values at several geographic locations
#' (hereafter, called \emph{sources}). One common type of sources used in
#' ecology is the delta values for deuterium in precipitation water collected
#' at weather stations, but one may also use measurements performed on
#' sedentary organisms. In either case, the accuracy of the isoscape (and
#' thereby the accuracy of assignments) increases with the number and spatial
#' coverage of the sources. Because fitting a geostatistical model may take
#' several hours for large datasets, we have stored an already fitted model for
#' users willing to explore our package (see \code{\link{Europefit}}).
#' 
#' \item Preparing the elevation raster with \code{\link{relevate}}:
#' 
#' Building isoscapes and assigning organisms to their origin requires an
#' adequate elevation raster, i.e. a matrix representing altitude data on a
#' spatial grid. The function \code{\link{relevate}} allows restricting the
#' extent of the raster to the area covered by isoscape data (in order to avoid
#' extrapolation) and to reduce the resolution of the original elevation raster
#' (in order to speed up computation in all following steps). Note that
#' aggregating the raster may lead to different results for the assignment,
#' because the elevation of raster cells changes depending on the aggregation
#' function, which in turn will affect model predictions.
#' 
#' We will soon provide a link to download an elevation raster for the entire
#' world at a resolution of one altitude per square-km, and other rasters may
#' be used. We have also stored a low resolution raster in our package (see
#' \code{\link{elevraster}}) for users to try things out, but we do not
#' encourage its use for real application.
#' 
#' \item Predicting the isoscape across the area covered by the elevation
#' raster with \code{\link{isoscape}}:
#' 
#' The function \code{\link{isoscape}} generates the isoscape: it uses the
#' fitted geostatistical model to predict the isotopic values for each raster
#' cell defined by the elevation raster. Our package allows the production of
#' fine-tuned isoscape figures (using the function
#' \code{\link{plot.isoscape}}). Alternatively, the isoscape rasters can be
#' exported as ascii raster and edited in any Geographic Information System
#' (GIS) software (see \code{\link{isoscape}} for details).
#' 
#' \item Fitting the calibration model with \code{\link{calibfit}}:
#' 
#' In most cases, organisms are of another kind than the sources used to build
#' the isoscape (e.g. the isoscape is built on precipitation isotopic values
#' and organisms are not water drops, i.e. the deuterium values of their
#' keratin structures were modulated by their distinct physiology). In this
#' situation, one must use sedentary organisms to study the relationship
#' between the isotopic values in organisms and that of their environment. The
#' function \code{\link{calibfit}} fits a statistical model on such a
#' calibration dataset.
#' 
#' If the isoscape is directly built from isotopic values of organisms, there
#' is no need to fit a calibration model.
#' 
#' \item Inferring spatial origins of organisms with \code{\link{isofind}}:
#' 
#' The function \code{\link{isofind}} tests for each location across the
#' isoscape if it presents a similar isotopic signature than the unknown origin
#' of a given individual(s). This assignment procedure considered the
#' uncertainty stemming from the model fits (geostatistical model and
#' calibration model). The function \code{\link{plot.isorix}} then draw such
#' assignment by plotting the most likely origin with the prediction region
#' around it. When several organisms are being assigned, both individuals
#' assignments and a single assignment for the whole group can be performed. }
#' 
#' @name IsoriX-package
#' @aliases IsoriX-package IsoriX
#' @docType package
#' @note Please note that for now, the geographic coordinates (latitude,
#' longitude) of any spatial data (locations, rasters) must be given in decimal
#' degrees following the WGS84 spheroid standard.
#' @author Alexandre Courtiol \email{alexandre.courtiol@@gmail.com},
#' 
#' Stephanie Kramer-Schadt \email{kramer@@izw-berlin.de}
#' @references Bowen, G. J., Wassenaar, L. I., Hobson, K. A. (2005). Global
#' application of stable hydrogen and oxygen isotopes to wildlife forensics.
#' Oecologia, 143(3): 337-348.
#' @keywords package
#' @examples
#' 
#' ### A simple workflow for IsoriX
#' ### is provided in a vignette:
#' ### vignette("Workflow", "IsoriX")
#' 
NULL

