annotate <- function(col=c("black","red","green","blue"), lwd=c(4,4,30), cex=c(1,1), info=TRUE) {

   # check for interactive mode

   if (!interactive()) {
      message("Function can only be used in interactive mode.")
      return(invisible())
   }

   # if no plotting device is open, exit

   if (dev.cur() == 1L) {
      message("No graphics device is open.")
      return(invisible())
   }

   # check if plot may be based on the grid graphics engine

   if (length(grid::grid.ls(print=FALSE)$name != 0L)) {
      message("The current plot appears to be based on grid graphics (which is not supported).")
      return(invisible())
   }

   # check for getGraphicsEvent() capabilities of the current plotting device

   if (any(!is.element(c("MouseDown", "MouseMove", "MouseUp", "Keybd"), dev.capabilities()$events))) {
      message("The graphics device does not support event handling.")
      return(invisible())
   }

   # defaults for arguments if NULL is specified (for blank() to work correctly)

   if (is.null(col))
      col <- c("black","red","green","blue")
   if (is.null(lwd))
      lwd <- c(4,4,30)
   if (is.null(cex))
      cex <- c(1,1)
   if (is.null(info))
      info <- TRUE

   # checks on col, lwd, and cex arguments

   ncol <- length(col)

   if (ncol > 9L)
      stop("Argument 'col' can be used to specify up to 9 colors, no more.", call.=FALSE)

   if (length(lwd) == 1L)
      lwd <- rep(lwd, 3L)
   if (length(lwd) == 2L)
      lwd <- rep(lwd, lwd, 3L)
   if (length(lwd) != 3L)
      stop("Argument 'lwd' should be of length 1, 2, or 3.", call.=FALSE)

   if (length(cex) == 1L)
      cex <- rep(cex, 2L)

   lwd.draw   <- lwd[1]
   lwd.symb   <- lwd[2]
   lwd.eraser <- lwd[3]
   cex.pt     <- cex[1]
   cex.txt    <- cex[2]

   # start in freehand draw mode

   colnum <- 1
   mode <- "draw"
   snap <- FALSE
   smooth <- FALSE

   oldxpd <- par("xpd")
   on.exit(par(xpd=oldxpd))
   par(xpd=NA)

   #dev.control(displaylist="inhibit")
   sav <- recordPlot()

   # get background color (set to white if it is transparent)

   col.bg <- par("bg")

   if (col.bg == "transparent")
      col.bg <- "white"

   .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)

   fun.mousedown <- function(button,x,y) {
      if (button == 2L) {
         .clear(info, col.bg)
         return(invisible(1))
      }
      pressed <<- TRUE
      x.start.ndc <<- x
      y.start.ndc <<- y
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      if (mode %in% c("draw", "ellipse")) {
         x.coords <<- x
         y.coords <<- y
      }
      #if (mode == "draw")
      #   buffer <<- c(buffer, draw=list(list(x=x, y=y, lwd=lwd.draw)))
      if (mode == "point") {
         points(x, y, pch=19, col=col[colnum], cex=cex.pt)
         buffer <<- c(buffer, point=list(list(x=x, y=y, cex=cex.pt)))
         pressed <<- FALSE
      }
      if (mode == "text")
         txt <<- ""
      x.start <<- x
      y.start <<- y
      x.last <<- x
      y.last <<- y
      return(NULL)
   }

   fun.mousemove <- function(button,x,y) {
      if (pressed) {
         x.last.ndc <<- x
         y.last.ndc <<- y
         x <- grconvertX(x, from="ndc", to="user")
         y <- grconvertY(y, from="ndc", to="user")
         if (mode %in% c("draw", "ellipse")) {
            x.coords <<- c(x.coords, x)
            y.coords <<- c(y.coords, y)
         }
         if (mode == "draw") {
            if (!smooth)
               segments(x.last, y.last, x, y, lwd=lwd.draw, col=col[colnum])
            #blen <- length(buffer)
            #buffer[[blen]]$x <<- c(buffer[[blen]]$x, x)
            #buffer[[blen]]$y <<- c(buffer[[blen]]$y, y)
         }
         if (mode == "eraser")
            segments(x.last, y.last, x, y, lwd=lwd.eraser, col=col.bg)
         x.last <<- x
         y.last <<- y
      }
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      pressed <<- FALSE
      if (mode == "text") {
         mode <<- "type"
         .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         return(NULL)
      }
      sameloc <- x.start == x.last && y.start == y.last
      if (sameloc)
         return(NULL)
      if (mode == "draw") {
         if (smooth) {
            xy <- .smooth(x.coords, y.coords)
            lines(xy$x, xy$y, lwd=lwd.symb, col=col[colnum])
            buffer <<- c(buffer, draw=list(list(x=xy$x, y=xy$y, lwd=lwd.symb)))
         } else {
            buffer <<- c(buffer, draw=list(list(x=x.coords, y=y.coords, lwd=lwd.symb)))
         }
         x.coords <<- NULL
         y.coords <<- NULL
      }
      if (mode == "rect")
         rect(x.start, y.start, x.last, y.last, lwd=lwd.symb, border=col[colnum])
      if (mode == "circle") {
         x.cent <- (x.start + x.last) / 2
         y.cent <- (y.start + y.last) / 2
         x.frac <- abs(x.start.ndc-x.last.ndc)
         y.frac <- abs(y.start.ndc-y.last.ndc)
         if (x.frac > y.frac) {
            radius <- abs(x.start-x.last) / 2
         } else {
            asp <- dev.size()[2] / dev.size()[1]
            radius <- abs(grconvertX(y.start.ndc*asp, from="ndc", to="user") - grconvertX(y.last.ndc*asp, from="ndc", to="user")) / 2
         }
         symbols(x.cent, y.cent, circles=radius, lwd=lwd.symb, fg=col[colnum], inches=FALSE, add=TRUE)
         buffer <<- c(buffer, circle=list(list(x=x.cent, y=y.cent, radius=radius, lwd=lwd.symb)))
      }
      if (mode %in% c("line", "arrow", "arrow2") && snap) {
         slope <- (y.start.ndc - y.last.ndc) / (x.start.ndc - x.last.ndc)
         if (abs(slope) > 0.5) {
            x.last <- x.start
         } else {
            y.last <- y.start
         }
      }
      if (mode == "line")
         segments(x.start, y.start, x.last, y.last, lwd=lwd.symb, col=col[colnum])
      if (mode == "arrow")
         arrows(x.start, y.start, x.last, y.last, lwd=lwd.symb, col=col[colnum])
      if (mode == "arrow2")
         arrows(x.start, y.start, x.last, y.last, lwd=lwd.symb, col=col[colnum], code=3)
      if (mode == "ellipse") {
         xy <- cbind(x.coords, y.coords)
         fit <- try(conicfit::EllipseDirectFit(xy), silent=TRUE)
         if (!inherits(fit, "try-error")) {
            pars <- AtoG(fit)$ParG
            xy <- calculateEllipse(pars[1], pars[2], pars[3], pars[4], 180/pi*pars[5], steps=101)
            lines(xy[,1], xy[,2], lwd=lwd.symb, col=col[colnum])
            buffer <<- c(buffer, ellipse=list(list(x=xy[,1], y=xy[,2], lwd=lwd.symb)))
         }
         x.coords <<- NULL
         y.coords <<- NULL
      }
      if (mode %in% c("rect", "line", "arrow", "arrow2")) {
         buffer <<- c(buffer, list(list(x=c(x.start, x.last), y=c(y.start, y.last), lwd=lwd.symb)))
         if (is.null(names(buffer))) { # if there is nothing in the buffer yet
            names(buffer) <<- mode
         } else {
            names(buffer)[length(names(buffer))] <<- mode
         }
      }
      return(NULL)
   }

   fun.key <- function(key) {

      if (mode == "type") {

         # escape to restart

         if (key == "\033" || key == "ctrl-[") {
            txt <<- ""
            mode <<- "text"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         # backspace to remove the last character from txt

         if (key == "\b" || key == "ctrl-H") {
            if (nchar(txt) > 1L) {
               txt <<- substr(txt, 1L, nchar(txt)-1L)
            } else {
               txt <<- ""
            }
            #print(txt)
            return(NULL)
         }

         # enter to add the text at the chosen location

         if (key == "\r" || key == "ctrl-J") {
            text(x.start, y.start, txt, cex=cex.txt, adj=c(0,0.5))
            txtw <- strwidth(txt, cex=cex.txt)  * 2
            txth <- strheight(txt, cex=cex.txt) * 2
            xleft   <- x.start
            xright  <- x.start + txtw
            ybottom <- y.start - txth / 2
            ytop    <- y.start + txth / 2
            buffer <<- c(buffer, text=list(list(x=c(xleft, xright), y=c(ybottom, ytop), txt=txt)))
            txt <<- ""
            mode <<- "text"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         txt <<- paste0(txt, key, collapse="")
         txt <<- gsub("\\n", "\n", txt, fixed=TRUE)
         #print(txt)

      } else {

         # q to exit

         if (key == "q") {
            .clear(info, col.bg)
            return(invisible(1))
         }

         # number keys to select colors

         if (is.element(key, 1:ncol)) {
            colnum <<- as.numeric(key)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         # b for blank canvas

         if (key == "b") {
            cords1 <- c(grconvertX(0.00, from="ndc", to="user"),
                        grconvertY(0.00, from="ndc", to="user"))
            cords2 <- c(grconvertX(1.00, from="ndc", to="user"),
                        grconvertY(1.00, from="ndc", to="user"))
            rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=NA)
            buffer <<- list()
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         # - / += to decrease/increase the line width / point size

         if (is.element(key, c("-","Down"))) {
            if (mode == "draw")
               lwd.draw <<- max(1, lwd.draw - 1)
            if (mode == "eraser")
               lwd.eraser <<- max(1, lwd.eraser - 5)
               if (mode %in% c("rect", "circle", "ellipse", "line", "arrow", "arrow2"))
               lwd.symb <<- max(1, lwd.symb - 1)
            if (mode == "point")
               cex.pt <<- max(0.5, cex.pt - 0.5)
            if (mode == "text")
               cex.txt <<- max(0.5, cex.txt - 0.5)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         if (is.element(key, c("=","+","Up"))) {
            if (mode == "draw")
               lwd.draw <<- lwd.draw + 1
            if (mode == "eraser")
               lwd.eraser <<- lwd.eraser + 5
            if (mode %in% c("rect", "circle", "ellipse", "line", "arrow", "arrow2"))
               lwd.symb <<- lwd.symb + 1
            if (mode == "point")
               cex.pt <<- cex.pt + 0.5
            if (mode == "text")
               cex.txt <<- cex.txt + 0.5
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
            return(NULL)
         }

         # d/p/e/l/a/A/s/r/c/t to switch into the various drawing modes

         if (key == "d") {
            mode <<- "draw"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "p") {
            mode <<- "point"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "e") {
            mode <<- "eraser"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "l") {
            mode <<- "line"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "a") {
            mode <<- "arrow"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "A") {
            mode <<- "arrow2"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "s") {
            if (mode == "draw")
               smooth <<- !smooth
            if (mode %in% c("line", "arrow", "arrow2"))
               snap <<- !snap
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "r") {
            mode <<- "rect"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "c") {
            mode <<- "circle"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "o") {
            mode <<- "ellipse"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "t") {
            mode <<- "text"
            txt <<- ""
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         # z and x for replaying and recording the plot

         if (key == "z") {
            replayPlot(sav)
            buffer <<- list()
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         if (key == "x") {
            sav <<- recordPlot()
            buffer <<- list()
         }

         # i to toggle info

         if (key == "i") {
            info <<- !info
            if (!info)
               .clear(TRUE, col.bg)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, smooth, info)
         }

         #if (key == "F12")
         #   print(buffer)

         # u to undo

         if (key == "u" && length(buffer) > 0) {
            blen <- length(buffer)
            type <- names(buffer)[blen]
            xb <- buffer[[blen]]$x
            yb <- buffer[[blen]]$y
            lwdb <- buffer[[blen]]$lwd
            cexb <- buffer[[blen]]$cex
            if (type %in% c("draw", "ellipse"))
               lines(xb, yb, lwd=lwdb+4, col=col.bg)
            if (type == "rect")
               rect(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, border=col.bg)
            if (type == "circle")
               symbols(xb[1], yb[1], circles=buffer[[blen]]$radius, lwd=lwdb+4, fg=col.bg, inches=FALSE, add=TRUE)
            if (type == "point")
               points(xb, yb, pch=19, col=col.bg, cex=cexb*1.2)
            if (type == "line")
               segments(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg)
            if (type == "arrow")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg)
            if (type == "arrow2")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg, code=3)
            if (type == "text")
               rect(xb[1], yb[1], xb[2], yb[2], col=col.bg, border=col.bg)
            buffer <<- buffer[-blen]
         }

      }

      return(NULL)

   }

   pressed <- FALSE
   x.last  <- NA_real_
   y.last  <- NA_real_
   x.start <- NA_real_
   y.start <- NA_real_
   x.start.ndc <- NA_real_
   y.start.ndc <- NA_real_
   x.last.ndc  <- NA_real_
   y.last.ndc  <- NA_real_
   txt <- ""
   buffer <- list()
   x.coords <- NULL
   y.coords <- NULL

   getGraphicsEvent(prompt="", onMouseDown=fun.mousedown, onMouseMove=fun.mousemove, onMouseUp=fun.mouseup, onKeybd=fun.key)

}
