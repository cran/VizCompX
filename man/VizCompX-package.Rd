\name{VizCompX-package}
\alias{VizCompX-package}
\alias{VizCompX}
\docType{package}
\title{
Visualisation of Computer Experiments
}
\description{
Generates Latin Hypercube Designs, fits Gaussian models to computer model output,
and gives a Wireframe visualisation of the fitted surface
}
\details{
\tabular{ll}{
Package: \tab VizCompX\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2010-03-02\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Use Latin.Hypercube to generate the design, NimrodOfunction to generate the responses for the Nimrod/O test function,
mlegp to fit the Gaussian model, and wireframe to get a visualisation of the fitted surface.
}
\author{
Neil Diamond

Maintainer: Neil Diamond <neil.diamond@buseco.monash.edu.au>
}
\references{http://messagelab.monash.edu.au/
}


\examples{
design <- LatinHypercube(50,3,maxs=rep(25,3))
response <- NimrodOexample(design)
mlegpfit <- mlegp(design,response)
wireframe(mlegpfit,c(5,5,4),c(1,2)) 
}