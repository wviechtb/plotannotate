\name{blank}
\alias{blank}
\title{Create a Blank Plot}
\description{
   Function to create a blank plot.
}
\usage{
blank(annotate=TRUE, bg="white", \dots)
}
\arguments{
   \item{annotate}{a logical to specify whether to start the annotation mode after creating the plot (\code{TRUE} by default).}
   \item{bg}{a string to specify the background color of the device region (\code{"white"} by default).}
   \item{\dots}{arguments that are passed on to \code{\link{annotate}()} when \code{annotate=TRUE}.}
}
\details{
   The function creates a blank plot. This is useful if one simply wants to make some drawing (and not add annotations to an existing plot).
}
\value{
   No return value (called for side effects).
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com}).
}
\examples{
\donttest{
blank()}
}
