\name{annotate}
\alias{annotate}
\title{Annotate Plots}
\description{
   Function to add annotations to a plot.
}
\usage{
annotate(col=c("black","red","green","blue"), lwd=c(4,4,30), cex=c(1,1), info=TRUE)
}
\arguments{
   \item{col}{a vector of colors (can be up to length 9).}
   \item{lwd}{a numeric vector to specify the line width for freehand drawing, for symbols, and the eraser.}
   \item{cex}{a numeric vector to specify the size of points and text.}
   \item{info}{a logical to specify whether visual information about the drawing mode and colors should be shown while annotating a plot.}
   \item{icex}{a numeric value to specify a multiplication factor for the visual information.}
}
\details{

   This is the main function of the package. After creating a plot, for example with \code{\link{plot}()}, the command \code{annotate}() enters a freehand drawing mode where one can draw on the plot (by pressing the left mouse button and dragging the pointer across the plot). Use the \verb{q} key (or the right mouse button) to quit the \code{annotate()} function.

   Various keyboard keys can be used to select colors and to change the drawing mode (when \code{info=TRUE}, one can also click on the boxes at the top to select modes/colors):

   \tabular{lll}{
   \bold{key} \tab \ics \tab \bold{function} \cr
   \verb{1-9} \tab \ics \tab Select one of the colors from the ones specified via the \code{col} argument. \cr
   \verb{d}   \tab \ics \tab Change to freehand drawing mode as explained above. \cr
   \verb{p}   \tab \ics \tab Change to point drawing mode. Click on a location of the plot to add a point. \cr
   \verb{l}   \tab \ics \tab Change to line drawing mode. Click on a location, hold the mouse button, move the pointer to a different location, and release the mouse button to draw a line. \cr
   \verb{a}   \tab \ics \tab Change to arrow drawing mode. Works like the line drawing mode. The arrowhead is shown at the release location. \cr
   \verb{A}   \tab \ics \tab Change to arrow drawing mode with arrowheads at both ends. \cr
   \verb{r}   \tab \ics \tab Change to rectangle drawing mode. Works like the line drawing mode. \cr
   \verb{c}   \tab \ics \tab Change to circle drawing mode. Works like the line drawing mode. The larger of the x- or y-axis distance determines the radius of the circle. \cr
   \verb{C}   \tab \ics \tab Change to circle drawing mode. Works like the freehand drawing mode, except that the nearest circle corresponding to the input coordinates is drawn. Note that the circle is drawn after releasing the mouse button. \cr
   \verb{o}   \tab \ics \tab Change to ellipse drawing mode. Works like the freehand drawing mode, except that the nearest ellipse corresponding to the input coordinates is drawn. Note that the ellipse is drawn after releasing the mouse button. \cr
   \verb{t}   \tab \ics \tab Change to text drawing mode. Click on a location (which enters type mode), type some text, and after hitting \verb{enter} the text is added to the right of the click location. Note that the text typed is not shown until hitting \verb{enter}. The \verb{escape} key cancels the text entry. \cr
   \verb{e}   \tab \ics \tab Change to eraser mode. Like the freehand drawing mode, except that the background color is used for drawing. \cr
   }

   Various keyboard keys can be used to adjust the settings:

   \tabular{lll}{
   \bold{key} \tab \ics \tab \bold{function} \cr
   \verb{-+}  \tab \ics \tab Decrease/increase the line width or point/text size. The default sizes are set by the \code{lwd} and \code{cex} arguments. Can also use \verb{=} instead of \verb{+} or the down and up arrow keys for the same purpose. \cr
   \verb{,}   \tab \ics \tab Toggles through three different line types (solid, dashed, dotted). This affects lines, arrows, rectangles, circles, and ellipses. \cr
   \verb{s}   \tab \ics \tab For freehand drawing, toggle smooth mode, so that the line is drawn after running a smoother over the input coordinates. For lines and arrows, toggle snap mode, so that lines/arrows are drawn exactly horizontally/vertically. Selecting the same mode twice or using the middle mouse button also toggles these modes. \cr
   }

   Finally, some additional keyboard keys have the following functions:

   \tabular{lll}{
   \bold{key} \tab \ics \tab \bold{function} \cr
   \verb{u}   \tab \ics \tab Undo (i.e., remove) annotations that have been added to the plot by drawing on top of them with the background color of the plot. Therefore, any elements underneath an annotation that is removed in this manner will also be drawn over. \cr
   \verb{z}   \tab \ics \tab Redraw the entire plot without the annotations. This relies on \code{\link{recordPlot}} and \code{\link{replayPlot}} working correctly. \cr
   \verb{x}   \tab \ics \tab Fix the current state of the plot (i.e., runs \code{\link{recordPlot}}) so \verb{z} redraws the plot in this state. \cr
   \verb{b}   \tab \ics \tab Make the plot completely blank. See also the \code{\link{blank}} function for creating a blank plot. \cr
   \verb{i}   \tab \ics \tab Toggle \code{info} on/off. \cr
   }

   When \code{info=TRUE} (the default), then visual information about the drawing mode and colors are shown at the top left while annotating a plot. This information may overlap with the plot title or other elements. One can use the \code{icex} argument to rescale the visual information to better match the resolution of the plot. Note that the boxes for the drawing modes and colors are clickable.

   Resizing plots while or after the \code{annotate} function has been run is not recommended (all annotations and visual elements need to be redrawn, which can take some time, especially when a lot of annotations have been added to the plot).

   After annotating a plot, it should be possible to save the plot including the annotations with \code{\link{dev.print}}, for example using something like \code{dev.print(device=png, filename="plot.png", width=<width>, height=<height>)}. However, by default, the plot will have a transparent background color, which is typically not what is desired. To avoid this issue, use \code{par(bg="white")} before creating the plot to set a white background color.

}
\value{
   No return value (called for side effects).
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com}).
}
\examples{
dat <- mtcars
plot(dat$hp, dat$mpg, pch=21, bg="gray", cex=1.5, main="Horsepower versus Mileage")
annotate()
}
