\name{annotate}
\alias{annotate}
\title{Annotate Plots}
\description{
   Function to add annotations to a plot.
}
\usage{
annotate(col=c("black","red","green","blue"), lwd=c(4,4,30), cex=c(1,1), verbose=TRUE)
}
\arguments{
   \item{col}{a vector of colors (can be up to length 9).}
   \item{lwd}{a numeric vector to specify the line width for drawing, for symbols, and the eraser.}
   \item{cex}{a numeric vector to specify the size of points and text.}
   \item{verbose}{a logical to specify whether visual information about the drawing mode and colors should be shown while annotating a plot.}
}
\details{

   This is the main function of the package. After creating a plot, for example with \code{\link{plot}()}, the command \code{annotate}() enters a free drawing mode where one can draw on the plot (by pressing the left mouse button and dragging the pointer across the plot).

   Various keys can be used to change the drawing mode or to adjust the settings:

   \tabular{lll}{
   \bold{key} \tab \ics \tab \bold{function} \cr
   \verb{q}   \tab \ics \tab Quit the \code{annotate()} function (the right mouse button can also be used for this purpose). \cr
   \verb{1-9} \tab \ics \tab Select a color for drawing (up to the length of the \code{col} argument). \cr
   \verb{d}   \tab \ics \tab Change to free drawing mode (as explained above). \cr
   \verb{p}   \tab \ics \tab Change to point drawing mode (click on a location of the plot to add a point). \cr
   \verb{l}   \tab \ics \tab Change to line drawing mode (click on a location, hold the mouse button, move the pointer to a different location, and release the mouse button to draw a line). \cr
   \verb{a}   \tab \ics \tab Change to arrow drawing mode (as above). \cr
   \verb{r}   \tab \ics \tab Change to rectangle drawing mode (as above). \cr
   \verb{c}   \tab \ics \tab Change to circle drawing mode (as above). \cr
   \verb{t}   \tab \ics \tab Change to text drawing mode (click on a location, type some text, and after hitting \verb{enter} the text is added next to the click location). \cr
   \verb{-+}  \tab \ics \tab Decrease/increase the line width or point/text size (the default sizes are set by the \code{lwd} and \code{cex} arguments). Can also use \verb{=} instead of \verb{+} or \verb{(} and \verb{)} for the same purpose. \cr
   \verb{s}   \tab \ics \tab Toggle snap mode, so that lines/arrows are drawn exactly horizontally/vertically. \cr
   \verb{u}   \tab \ics \tab Undo (i.e., remove) annotations that have been added to the plot. \cr
   \verb{e}   \tab \ics \tab Change to the eraser mode (can be used to draw over the plot with the background color). \cr
   \verb{z}   \tab \ics \tab Redraw the plot without the annotations. \cr
   \verb{x}   \tab \ics \tab Fix the current state of the plot (so \verb{z} redraws the plot in this state). \cr
   \verb{b}   \tab \ics \tab Make the plot completely blank. See also \code{\link{blank}()} for creating a blank plot. \cr
   \verb{v}   \tab \ics \tab Toggle \code{verbose} on/off. \cr
   }

   When \code{verbose=TRUE} (the default), then information about the drawing mode and colors are shown at the top left while annotating a plot. Note that this information may overlap with the plot title or other elements.

   Note that the undo functionality removes annotations by drawing on top of them with the background color of the plot. Therefore, any elements underneath an annotation that is removed in this manner will also be drawn over.

   After annotating a plot, it should be possible to save the plot including the annotations with \code{\link{dev.print}} (e.g., using something like \code{dev.print(device=png, filename="plot.png", width=<width>, height=<height>)}).

}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com}).
}
\examples{
dat <- mtcars
plot(dat$hp, dat$mpg, pch=21, bg="gray", cex=1.5, main="Horsepower versus Mileage")

\dontrun{
annotate()}
}
