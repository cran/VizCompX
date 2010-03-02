\name{mlegp}
\alias{mlegp}

\title{ Fits Gaussian Model to Computer Experiment}
\description{
  A wrapper for the fitting function in the mlegp package
}
\usage{
mlegp(xlist, yval)
}

\arguments{
  \item{xlist}{ A list consisting of the computer experiment design matrix and vectors of minimums and maximums}
  \item{yval}{ The computer experiment response vector }
}

\value{
 
  \item{comp1 }{The output of the mlegp function.}
  \item{comp2 }{A list giving the computer experiment design matrix and vectors of minimums and maximums}
  
}
\references{ http://messagelab.monash.edu.au/ 

Garrett M. Dancik (2009). mlegp: Maximum Likelihood Estimates of
  Gaussian Processes. R package version 3.1.0.
  http://CRAN.R-project.org/package=mlegp

}
\author{ Neil Diamond }

\examples{
design <- LatinHypercube(50,3,maxs=rep(25,3))
response <- NimrodOexample(design)
mlegpfit <- mlegp(design,response)
}
\keyword{ Computer Experiments }