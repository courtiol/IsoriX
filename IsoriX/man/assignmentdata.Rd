\name{assignmentdata}
\alias{assignmentdata}
\docType{data}

\title{Assignment dataset}

\description{This dataset contains data from Voigt, Lehmann and Greif (2015). It contains deuterium delta values of fur keratin from bats captured in 2008, 2009 and 2013 from their roosting sites in Bulgaria. We only retained the bats of the genus Myotis from the original study. The data can be used as an example to perform assignments using the function \code{\link{Isorix}}.}

\usage{
data(assignmentdata)
}

\format{
A \var{dataframe} with 244 observations on 3 variables:
\tabular{rlll}{
[, 1] \tab animalID \tab (\var{Factor}) \tab Identification of the animal\cr
[, 2] \tab species \tab (\var{Factor}) \tab Animal species name\cr
[, 3] \tab tissue.value \tab (\var{numeric}) \tab  Deuterium delta value of the tissue\cr
}   
}

\source{data directly provided by the authors of the following publication}

\references{
Voigt, C.C., Lehmann, D., Greif, S. (2015). Stable isotope ratios of hydrogen separate mammals of aquatic and terrestrial food webs. Methods in Ecology and Evolution 6(11).
}

\examples{
data(assignmentdata)
head(assignmentdata)
str(assignmentdata)
}

\seealso{
\code{\link{Isorix}} to perform assignments

}

\keyword{datasets}

