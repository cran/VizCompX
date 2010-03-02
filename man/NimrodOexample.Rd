\name{NimrodOexample}
\alias{NimrodOexample}
\title{ Generates Nimrod/O test function }
\description{
  In two dimensions, gives the Nimrod/O test function. In higher dimensions gives a generalisation.
}
\usage{
NimrodOexample(xlist)
}
\arguments{
  \item{xlist}{ A list giving the computer experiment design matrix and vectors of minimums and maximums}
}

\value{
  A vector of response values corresponding to each row of the design matrix
}
\references{ http://messagelab.monash.edu.au/ }
\author{ Neil Diamond }

\note{In two dimensions the function is given by:

          f(x1,x2)=cos(pi*x1/6)-sin(pi*x2/12)+pi*x1/12+pi*x2/24

      In n dimensions the function is given by:
      
          f(x1,x2,...,xn)=cos(pi*X2/(6*(n-1)))-sin(pi*X1/(12*(n-1)))+pi*X2/(12*(n-1))+pi*X1/(24*(n-1))
          
      where X1=(x2+x3+...+xn) and X2=(x1+x3+...+xn) }   


\examples{    
design <- LatinHypercube(50,3,maxs=rep(25,3))
response <- NimrodOexample(design) 
}

\keyword{ Computer Experiments }
