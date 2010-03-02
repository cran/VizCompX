\name{LatinHypercube}
\alias{LatinHypercube}
\title{ Generates a Latin Hypercube Design
 }
\description{
  Generates a Latin Hypercube Design for an arbitrary number of runs
   and arbitrary number of variables 
}
\usage{
LatinHypercube(N, d, mins = rep(0, d), maxs = rep(1, d), 
varnames = paste("x", seq(1, d), sep = ""))
}
\arguments{
  \item{N}{ The number of runs in the computer experiment. }
  \item{d}{ The number of variables in the computer experiment. }
  \item{mins}{ The minimum levels of the variables. By default the minimums are 0. }
  \item{maxs}{ The maximum levels of the variables. By default the maximums are 1. }
  \item{varnames}{ The names of the variables. If not given x1, x2, ..., xd are used. }
}

\value{
  
  \item{comp1 }{The Latin Hypercube Design}
  \item{comp2 }{The vector of minimums}
  \item{comp3 }{The vector of maximums}
 }
\references{ http://messagelab.monash.edu.au/ 

R. K. S. Hankin 2005. Introducing BACCO, an R bundle for Bayesian
  Analysis of Computer Code Output. Journal of Statistical Software,
  14(16)

}
\author{ Neil Diamond }
\note{Just a wrapper for the latin.hypercube function in the emulator package}
\examples{
LatinHypercube(50,3,maxs=rep(25,3))  
}
\keyword{ Computer Experiments }
