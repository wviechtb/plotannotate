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

   if (is.null(col))
      col <- c("black","red","green","blue")
   if (is.null(lwd))
      lwd <- c(4,4,30)
   if (is.null(cex))
      cex <- c(1,1)
   if (is.null(info))
      info <- TRUE

   # checks on col argument
   ncol <- length(col)

   if (ncol > 9L)
      stop("Argument 'col' can be used to specify up to 9 colors, no more.", call.=FALSE)

   # checks on lwd and cex arguments
   if (length(lwd) == 1L)
      lwd <- rep(lwd, 3)
   if (length(lwd) == 2L)
      lwd <- rep(lwd, lwd, 3)
   if (length(lwd) != 3L)
      stop("Argument 'lwd' should be of length 1, 2, or 3.", call.=FALSE)
   if (length(cex) == 1L)
      cex <- rep(cex, 2)

   lwd.draw   <- lwd[1]
   lwd.symb   <- lwd[2]
   lwd.eraser <- lwd[3]
   cex.pt     <- cex[1]
   cex.txt    <- cex[2]

   # start in draw mode with first color selected and snap=FALSE
   colnum <- 1
   mode <- "draw"
   snap <- FALSE

   #dev.control(displaylist="inhibit")
   sav <- recordPlot()

   # get background color
   col.bg <- par("bg")

   if (col.bg == "transparent")
      col.bg <- "white"

   .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)

   fun.mousedown <- function(button,x,y) {
      if (button == 2L) {
         .clear(info)
         return(invisible(1))
      }
      pressed <<- TRUE
      x.start.ndc <<- x
      y.start.ndc <<- y
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      if (mode == "draw")
         buffer <<- c(buffer, draw=list(list(x=x, y=y)))
      if (mode == "point") {
         points(x, y, pch=19, col=col[colnum], cex=cex.pt)
         buffer <<- c(buffer, point=list(list(x=x, y=y)))
         pressed <<- FALSE
      }
      if (mode == "text") {
         txt <<- ""
      }
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
         if (mode == "draw") {
            segments(x.last, y.last, x, y, lwd=lwd.draw, col=col[colnum])
            blen <- length(buffer)
            buffer[[blen]]$x <<- c(buffer[[blen]]$x, x)
            buffer[[blen]]$y <<- c(buffer[[blen]]$y, y)
         }
         if (mode == "eraser")
            segments(x.last, y.last, x, y, lwd=lwd.eraser, col=col.bg)
         x.last <<- x
         y.last <<- y
      }
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      if (mode == "text") # so that 'pressed' stays TRUE and keystrokes are for adding text
         return(NULL)
      pressed <<- FALSE
      xyok <- x.start != x.last && y.start != y.last
      if (!xyok)
         return(NULL)
      if (mode == "rect")
         rect(x.start, y.start, x.last, y.last, lwd=lwd.symb, border=col[colnum])
      if (mode == "circle") {
         x.cent <- (x.start + x.last) / 2
         y.cent <- (y.start + y.last) / 2
         symbols(x.cent, y.cent, circles=max(abs(x.start-x.last), abs(y.start-y.last))/2, lwd=lwd.symb, fg=col[colnum], inches=FALSE, add=TRUE)
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
      if (mode %in% c("rect", "circle", "line", "arrow", "arrow2")) {
         buffer <<- c(buffer, list(list(x=c(x.start, x.last), y=c(y.start, y.last))))
         names(buffer)[length(names(buffer))] <<- mode
      }
      return(NULL)
   }

   fun.key <- function(key) {

      if (mode == "text" && pressed) {

         # escape to restart
         if (key == "\033" || key == "ctrl-[") {
            txt <- ""
            pressed <<- FALSE
            return(NULL)
         }

         # backspace to remove the last character from txt
         if (identical(key, "\b") || identical(key, "ctrl-H")) {
            if (nchar(txt) > 1) {
               txt <<- substr(txt, 1, nchar(txt)-1)
            } else {
               txt <<- ""
            }
            #print(txt)
            return(NULL)
         }

         # enter to add the text at the chosen location
         if (identical(key, "\r") || identical(key, "ctrl-J")) {
            text(x.start, y.start, txt, cex=cex.txt, adj=c(0,0.5))
            txtw <- strwidth(txt, cex=cex.txt)  * 2
            txth <- strheight(txt, cex=cex.txt) * 2
            xleft   <- x.start
            xright  <- x.start + txtw
            ybottom <- y.start - txth / 2
            ytop    <- y.start + txth / 2
            buffer <<- c(buffer, text=list(list(x=c(xleft, xright), y=c(ybottom, ytop), txt=txt)))
            txt <- ""
            pressed <<- FALSE
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
            return(NULL)
         }

         txt <<- paste0(txt, key, collapse="")
         txt <<- gsub("\\n", "\n", txt, fixed=TRUE)
         #print(txt)

      } else {

         # q to exit

         if (key == "q") {
            .clear(info)
            return(invisible(1))
         }

         # number keys to select colors

         if (is.element(key, 1:ncol)) {
            colnum <<- as.numeric(key)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
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
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
            return(NULL)
         }

         # - / += to decrease/increase the line width / point size

         if (is.element(key, c("-","("))) {
            if (mode == "draw")
               lwd.draw <<- max(1, lwd.draw - 1)
            if (mode == "eraser")
               lwd.eraser <<- max(1, lwd.eraser - 5)
               if (mode %in% c("rect", "circle", "line", "arrow", "arrow2"))
               lwd.symb <<- max(1, lwd.symb - 1)
            if (mode == "point")
               cex.pt <<- max(0.5, cex.pt - 0.5)
            if (mode == "text")
               cex.txt <<- max(0.5, cex.txt - 0.5)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
            return(NULL)
         }

         if (is.element(key, c("=","+",")"))) {
            if (mode == "draw")
               lwd.draw <<- lwd.draw + 1
            if (mode == "eraser")
               lwd.eraser <<- lwd.eraser + 5
            if (mode %in% c("rect", "circle", "line", "arrow", "arrow2"))
               lwd.symb <<- lwd.symb + 1
            if (mode == "point")
               cex.pt <<- cex.pt + 0.5
            if (mode == "text")
               cex.txt <<- cex.txt + 0.5
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
            return(NULL)
         }

         # d/p/e/r/c to switch into the various drawing modes

         if (key == "d") {
            mode <<- "draw"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "p") {
            mode <<- "point"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "e") {
            mode <<- "eraser"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "l") {
            mode <<- "line"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "a") {
            mode <<- "arrow"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "A") {
            mode <<- "arrow2"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "s") {
            snap <<- !snap
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "r") {
            mode <<- "rect"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "c") {
            mode <<- "circle"
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "t") {
            mode <<- "text"
            txt <<- ""
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "z") {
            replayPlot(sav)
            buffer <<- list()
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "x") {
            sav <<- recordPlot()
            buffer <<- list()
         }

         if (key == "v") {
            info <<- !info
            if (!info)
               .clear(TRUE)
            .info(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info)
         }

         if (key == "F12")
            print(buffer)

         if (key == "u" && length(buffer) > 0) {
            blen <- length(buffer)
            type <- names(buffer)[blen]
            xb <- buffer[[blen]]$x
            yb <- buffer[[blen]]$y
            if (type == "draw") {
               for (i in 1:(length(xb)-1)) {
                  segments(xb[i], yb[i], xb[i+1], yb[i+1], lwd=lwd.draw+4, col=col.bg)
               }
            }
            if (type == "rect")
               rect(xb[1], yb[1], xb[2], yb[2], lwd=lwd.symb+4, border=col.bg)
            if (type == "circle") {
               x.cent <- (xb[1] + xb[2]) / 2
               y.cent <- (yb[1] + yb[2]) / 2
               symbols(x.cent, y.cent, circles=max(abs(xb[1]-xb[2]), abs(yb[1]-yb[2]))/2, lwd=lwd.symb+4, fg=col.bg, inches=FALSE, add=TRUE)
            }
            if (type == "point")
               points(xb, yb, pch=19, col=col.bg, cex=1.2*cex.pt)
            if (type == "line")
               segments(xb[1], yb[1], xb[2], yb[2], lwd=lwd.symb+4, col=col.bg)
            if (type == "arrow")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwd.symb+4, col=col.bg)
            if (type == "arrow2")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwd.symb+4, col=col.bg, code=3)
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

   getGraphicsEvent(prompt="", onMouseDown=fun.mousedown, onMouseMove=fun.mousemove,
                    onMouseUp=fun.mouseup, onKeybd=fun.key)

}
