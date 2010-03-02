\name{wireframe}
\alias{wireframe}

\title{ Visualisation of fitted computer model surface }
\description{
  Takes a mlegp fit of the results of a computer model and generates a visualisation of the fitted surface.
  The user can specify how many levels to use for each of the variables and which are the primary variables
  and which are the conditioning variables.
}
\usage{
wireframe(mlegpfit, numpredictvals, xandy)
}
\arguments{
  \item{mlegpfit}{ Fitted mlegp model }
  \item{numpredictvals}{ Grid of values for which predictions are made }
  \item{xandy}{ Primary visualisation variables e.g. c(1,3) for the first and third variables}
}

\value{Produces wireframe of the predicted response versus the two primary variables
}
\references{ http://messagelab.monash.edu.au/ }
\author{ Neil Diamond }

\examples{
design <- LatinHypercube(50,3,maxs=rep(25,3))
response <- NimrodOexample(design)
mlegpfit <- mlegp(design,response)
wireframe(mlegpfit,c(5,5,4),c(1,2))      
}
\keyword{ Computer Experiments }