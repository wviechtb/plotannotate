\name{plotannotate-package}
\alias{plotannotate-package}
\docType{package}
\title{plotannotate: Annotate Plots}
\description{

   The \pkg{plotannotate} package provides functionality for annotating plots created with \sQuote{base R graphics}. The main function for this purpose is \code{\link{annotate}}. After creating a plot, for example with \code{\link{plot}()}, the command \code{annotate()} enters a free drawing mode where one can draw on the plot. Various symbols (points, lines, arrows, rectangles, circles) and text can also be added. Pressing \verb{q} or the right mouse button terminates the annotation mode. For further details, see the documentation of the \code{\link{annotate}} function.

   Note that the \code{annotate()} function makes extensive use of \code{\link{getGraphicsEvent}} to capture mouse movements and keyboard inputs. Only some graphics devices support this (currently only \code{windows()} and \code{x11()}). If you receive the error message \sQuote{The graphics device does not support event handling}, then the graphics device that was opened does not provide this kind of functionality. For example, this will be the case for the \code{RStudioGD} graphics device that is used by RStudio. You can then try running \code{x11()} before creating the plot.

   Also, \code{annotate()} does not work for plots based on the \pkg{grid} graphics engine (which includes plots created with the \pkg{ggplot2} or \pkg{lattice} packages), since this such plots are not compatible with \code{\link{getGraphicsEvent}}.

}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\keyword{package}
