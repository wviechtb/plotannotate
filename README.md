plotannotate: An R Package to Annotate Plots
============================================

## Description

The `plotannotate` package allows annotating plots created with 'base R graphics'. The function for this purpose is called `annotate`. After creating a plot, for example with `plot()`, the command `annotate()` enters a freehand drawing mode where one can draw on the plot with the mouse (or some other input device). Various symbols (points, lines, arrows, rectangles, circles) and text can also be added. Pressing `q` or the right mouse button terminates the annotation mode.

The functionality provided by the package is especially useful when teaching (e.g., to visually explain certain plot elements), but can also be used for creating quick sketches.

Note that the `annotate()` function makes extensive use of `getGraphicsEvent` to capture mouse movements and keyboard inputs. Only some graphics devices support this (currently only `windows` and `x11`). If you receive the error message 'The graphics device does not support event handling', then the graphics device that was opened does not provide this kind of functionality. For example, this will be the case for the `RStudioGD` graphics device that is used by RStudio. You can then try running `x11()` before creating the plot.

Also, `annotate()` does not work for plots based on the `grid` graphics engine (which includes plots created with the `ggplot2` or `lattice` packages), since such plots are not compatible with `getGraphicsEvent`.

## Features

- freehand drawing mode
- add symbols (points, lines, arrows, rectangles, circles) and text to a plot
- specify up to 9 colors to choose from interactively
- adjust line widths and point/text sizes interactively
- undo annotations or use the eraser mode to draw over annotations
- make blank plots for quick sketches

## Meta

The `plotannotate` package is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/lgpl-3.0.txt). To report any issues or bugs, please go [here](https://github.com/wviechtb/plotannotate/issues).
