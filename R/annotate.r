annotate <- function(col=c("black","red","green","blue"), lwd=c(4,4,30), cex=c(1,1), info=TRUE, icex=1) {

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
      lwd <- c(lwd, lwd, 30)
   if (length(lwd) == 2L)
      lwd <- c(lwd[1], lwd[1], lwd[2])
   if (length(lwd) != 3L)
      stop("Argument 'lwd' should be of length 1, 2, or 3.", call.=FALSE)

   if (length(cex) == 1L)
      cex <- c(cex, cex)

   # get some current par() values and make sure that changed values are reset upon exit

   oldusr <- par("usr")
   oldxpd <- par("xpd")
   on.exit(par(usr=oldusr, xpd=oldxpd))
   par(usr=c(0,1,0,1))
   par(xpd=NA)
   par.cex <- par("cex")

   # get background color (set to white if it is transparent)

   col.bg <- par("bg")

   if (col.bg == "transparent")
      col.bg <- "white"

   # set defaults when starting

   mode <- "draw"
   colnum <- 1
   snap <- FALSE
   smooth <- FALSE
   lty <- 1
   pch <- 19

   # list to store old settings in

   old <- list(mode=mode, colnum=colnum, lwd=lwd, cex=cex, snap=snap, smooth=smooth, lty=lty, pch=pch)

   # set icex

   dims <- dev.size(units="px")
   #asp <- dims[1] / dims[2]
   icex <- icex * min(dims) / 1800

   # draw initial info

   boxpos <- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex, drawall=TRUE)

   # record plot in its current state

   sav <- recordPlot()

   # create the mouse and keyboard event handlers

   fun.mousedown <- function(button,x,y) {
      if (button == 2L) {
         .clear(info, col.bg)
         return(invisible(1))
      }
      if (button == 1L) {
         if (mode == "draw") {
            smooth <<- !smooth
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["smooth"]] <<- smooth
         }
         if (mode %in% c("line", "arrow", "arrow2")) {
            snap <<- !snap
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["snap"]] <<- snap
         }
         pressed <<- FALSE
         return(NULL)
      }
      pressed <<- TRUE
      x.start.ndc <<- x
      y.start.ndc <<- y
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      if (info && y.start.ndc >= 0.975) {
         modesel <- apply(boxpos[[1]], 1, function(pos) x > pos[1] && x < pos[3] && y > pos[2] && y < pos[4])
         if (any(modesel)) {
            modenum <- which(modesel)
            if (modenum == 1) {
               if (mode == "draw") {
                  smooth <<- !smooth
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["smooth"]] <<- smooth
               } else {
                  mode <<- "draw"
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["mode"]] <<- mode
               }
            }
            if (modenum == 2) {
               if (mode == "point") {
                  pchs <- c(19, 15, 18, 21)
                  pchnum <- which(pch == pchs)
                  pchnum <- pchnum + 1
                  if (pchnum > 4)
                     pchnum <- 1
                  pch <<- pchs[pchnum]
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["pch"]] <<- pch
               } else {
                  mode <<- "point"
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["mode"]] <<- mode
               }
            }
            if (modenum == 3) {
               if (mode == "line") {
                  snap <<- !snap
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["snap"]] <<- snap
               } else {
                  mode <<- "line"
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["mode"]] <<- mode
               }
            }
            if (modenum == 4) {
               if (mode == "arrow") {
                  snap <<- !snap
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["snap"]] <<- snap
               } else {
                  mode <<- "arrow"
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["mode"]] <<- mode
               }
            }
            if (modenum == 5) {
               if (mode == "arrow2") {
                  snap <<- !snap
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["snap"]] <<- snap
               } else {
                  mode <<- "arrow2"
                  boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
                  old[["mode"]] <<- mode
               }
            }
            if (modenum == 6) {
               mode <<- "rect"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
            if (modenum == 7) {
               if (mode == "circle") {
                  mode <<- "circle2"
               } else if (mode == "circle2") {
                  mode <<- "circle"
               } else {
                  mode <<- "circle"
               }
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
            if (modenum == 8) {
               mode <<- "ellipse"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
            if (modenum == 9) {
               mode <<- "text"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
            if (modenum == 10) {
               mode <<- "eraser"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
            pressed <<- FALSE
            return(NULL)
         }
         colsel <- apply(boxpos[[2]], 1, function(pos) x > pos[1] && x < pos[3] && y > pos[2] && y < pos[4])
         if (any(colsel)) {
            colnum <<- which(colsel)
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["colnum"]] <<- colnum
            pressed <<- FALSE
            return(NULL)
         }
      }
      if (mode %in% c("draw", "ellipse", "circle2")) {
         x.coords <<- x
         y.coords <<- y
      }
      if (mode == "point") {
         points(x, y, pch=pch, col=col[colnum], cex=cex[1]/par.cex)
         buffer <<- c(buffer, point=list(list(x=x, y=y, cex=cex[1]/par.cex, pch=ifelse(pch==21,19,pch))))
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
      if (!pressed)
         return(NULL)
      x.last.ndc <<- x
      y.last.ndc <<- y
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      if (mode %in% c("draw", "ellipse", "circle2")) {
         x.coords <<- c(x.coords, x)
         y.coords <<- c(y.coords, y)
      }
      if (mode == "draw") {
         if (!smooth)
            segments(x.last, y.last, x, y, lwd=lwd[1], col=col[colnum])
      }
      if (mode == "eraser")
         segments(x.last, y.last, x, y, lwd=lwd[3], col=col.bg)
      x.last <<- x
      y.last <<- y
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      if (!pressed)
         return(NULL)
      pressed <<- FALSE
      if (mode == "text") {
         mode <<- "type"
         boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
         old[["mode"]] <<- mode
         return(NULL)
      }
      sameloc <- x.start == x.last && y.start == y.last
      if (sameloc)
         return(NULL)
      if (mode == "draw") {
         if (smooth) {
            xy <- .smooth(x.coords, y.coords)
            lines(xy$x, xy$y, lwd=lwd[1], col=col[colnum])
            buffer <<- c(buffer, draw=list(list(x=xy$x, y=xy$y, lwd=lwd[1])))
         } else {
            buffer <<- c(buffer, draw=list(list(x=x.coords, y=y.coords, lwd=lwd[1])))
         }
         x.coords <<- NULL
         y.coords <<- NULL
      }
      if (mode == "rect")
         rect(x.start, y.start, x.last, y.last, lwd=lwd[2], border=col[colnum], lty=lty)
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
         symbols(x.cent, y.cent, circles=radius, lwd=lwd[2], fg=col[colnum], inches=FALSE, add=TRUE, lty=lty)
         buffer <<- c(buffer, circle=list(list(x=x.cent, y=y.cent, radius=radius, lwd=lwd[2])))
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
         segments(x.start, y.start, x.last, y.last, lwd=lwd[2], col=col[colnum], lty=lty)
      if (mode == "arrow")
         .arrows(x.start, y.start, x.last, y.last, lwd=lwd[2], col=col[colnum], lty=lty)
      if (mode == "arrow2")
         .arrows(x.start, y.start, x.last, y.last, lwd=lwd[2], col=col[colnum], lty=lty, code=3)
      if (mode == "ellipse") {
         xy <- cbind(x.coords, y.coords)
         fit <- try(conicfit::EllipseDirectFit(xy), silent=TRUE)
         if (!inherits(fit, "try-error")) {
            pars <- conicfit::AtoG(fit)$ParG
            xy <- conicfit::calculateEllipse(pars[1], pars[2], pars[3], pars[4], 180/pi*pars[5], steps=100)
            polygon(c(xy[,1],rev(xy[,1])), c(xy[,2], rev(xy[,2])), lwd=lwd[2], border=col[colnum], lty=lty)
            buffer <<- c(buffer, ellipse=list(list(x=xy[,1], y=xy[,2], lwd=lwd[2])))
         }
         x.coords <<- NULL
         y.coords <<- NULL
      }
      if (mode == "circle2") {
         xy <- cbind(x.coords, y.coords)
         fit <- try(conicfit::CircleFitByPratt(xy), silent=TRUE)
         if (!inherits(fit, "try-error")) {
            symbols(fit[1], fit[2], circles=fit[3], lwd=lwd[2], fg=col[colnum], inches=FALSE, add=TRUE, lty=lty)
            buffer <<- c(buffer, circle=list(list(x=fit[1], y=fit[2], radius=fit[3], lwd=lwd[2])))
         }
         x.coords <<- NULL
         y.coords <<- NULL
      }
      if (mode %in% c("line", "arrow", "arrow2", "rect")) {
         buffer <<- c(buffer, list(list(x=c(x.start, x.last), y=c(y.start, y.last), lwd=lwd[2])))
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
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
            return(NULL)
         }

         # backspace to remove the last character from txt

         if (key == "\b" || key == "ctrl-H") {
            if (nchar(txt) > 1L) {
               txt <<- substr(txt, 1L, nchar(txt)-1L)
            } else {
               txt <<- ""
            }
            return(NULL)
         }

         # enter to add the text at the chosen location

         if (key == "\r" || key == "ctrl-J") {
            text(x.start, y.start, txt, cex=cex[2]/par.cex, adj=c(0,0.5), col=col[colnum])
            txtw <- strwidth(txt, cex=cex[2]/par.cex)  * 2
            txth <- strheight(txt, cex=cex[2]/par.cex) * 2
            xleft   <- x.start
            xright  <- x.start + txtw
            ybottom <- y.start - txth / 2
            ytop    <- y.start + txth / 2
            buffer <<- c(buffer, text=list(list(x=c(xleft, xright), y=c(ybottom, ytop), txt=txt)))
            txt <<- ""
            mode <<- "text"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
            return(NULL)
         }

         txt <<- paste0(txt, key, collapse="")
         txt <<- gsub("\\n", "\n", txt, fixed=TRUE)
         #print(txt)

      } else {

         # q to quit/exit

         if (key == "q") {
            .clear(info, col.bg)
            return(invisible(1))
            #return(invisible(buffer))
         }

         # number keys to select colors

         if (is.element(key, 1:ncol)) {
            colnum <<- as.numeric(key)
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["colnum"]] <<- colnum
            return(NULL)
         }

         # keys to select tools

         if (key == "d") {
            if (mode == "draw") {
               smooth <<- !smooth
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["smooth"]] <<- smooth
            } else {
               mode <<- "draw"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
         }

         if (key == "p") {
            mode <<- "point"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "l") {
            if (mode == "line") {
               snap <<- !snap
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["snap"]] <<- snap
            } else {
               mode <<- "line"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
         }

         if (key == "a") {
            if (mode == "arrow") {
               snap <<- !snap
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["snap"]] <<- snap
            } else {
               mode <<- "arrow"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
         }

         if (key == "A") {
            if (mode == "arrow2") {
               snap <<- !snap
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["snap"]] <<- snap
            } else {
               mode <<- "arrow2"
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["mode"]] <<- mode
            }
         }

         if (key == "r") {
            mode <<- "rect"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "c") {
            if (mode == "circle") {
               mode <<- "circle2"
            } else if (mode == "circle2") {
               mode <<- "circle"
            } else {
               mode <<- "circle"
            }
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "C") {
            if (mode == "circle") {
               mode <<- "circle2"
            } else if (mode == "circle2") {
               mode <<- "circle"
            } else {
               mode <<- "circle2"
            }
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "o") {
            mode <<- "ellipse"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "t") {
            mode <<- "text"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key == "e") {
            mode <<- "eraser"
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["mode"]] <<- mode
         }

         if (key %in% c("d","p","l","a","A","r","c","C","o","t","e"))
            return(NULL)

         # down/up (or -+=) to decrease/increase the line width / point size

         if (is.element(key, c("Down","-"))) {
            if (mode == "draw")
               lwd[1] <<- max(1, lwd[1] - 1)
            if (mode %in% c("line", "arrow", "arrow2", "rect", "circle", "circle2", "ellipse"))
               lwd[2] <<- max(1, lwd[2] - 1)
            if (mode == "eraser")
               lwd[3] <<- max(1, lwd[3] - 5)
            if (mode == "point")
               cex[1] <<- max(0.5, cex[1] - 0.5)
            if (mode == "text")
               cex[2] <<- max(0.5, cex[2] - 0.25)
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["lwd"]] <<- lwd
            old[["cex"]] <<- cex
            return(NULL)
         }

         if (is.element(key, c("Up","=","+"))) {
            if (mode == "draw")
               lwd[1] <<- lwd[1] + 1
            if (mode %in% c("line", "arrow", "arrow2", "rect", "circle", "circle2", "ellipse"))
               lwd[2] <<- lwd[2] + 1
            if (mode == "eraser")
               lwd[3] <<- lwd[3] + 5
            if (mode == "point")
               cex[1] <<- cex[1] + 0.5
            if (mode == "text")
               cex[2] <<- cex[2] + 0.25
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
            old[["lwd"]] <<- lwd
            old[["cex"]] <<- cex
            return(NULL)
         }

         # left/right to change the point symbol / line type

         if (is.element(key, c("Left","Right"))) {
            if (mode == "point") {
               pchs <- c(19, 15, 18, 21)
               pchnum <- which(pch == pchs)
               pchnum <- pchnum + ifelse(key == "Left", -1, +1)
               if (pchnum > 4)
                  pchnum <- 1
               if (pchnum < 1)
                  pchnum <- 4
               pch <<- pchs[pchnum]
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["pch"]] <<- pch
            }
            if (mode %in% c("line", "arrow", "arrow2", "rect", "circle", "circle2", "ellipse")) {
               lty <<- lty + ifelse(key == "Left", -1, +1)
               if (lty > 3)
                  lty <<- 1
               if (lty < 1)
                  lty <<- 3
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["lty"]] <<- lty
            }
            return(NULL)
         }

         # s to toggle smooth / snap

         if (key == "s") {
            if (mode == "draw") {
               smooth <<- !smooth
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["smooth"]] <<- smooth
            }
            if (mode %in% c("line", "arrow", "arrow2")) {
               snap <<- !snap
               boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex)
               old[["snap"]] <<- snap
            }
            return(NULL)
         }

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
               points(xb, yb, pch=buffer[[blen]]$pch, col=col.bg, cex=cexb*1.2)
            if (type == "line")
               segments(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg)
            if (type == "arrow")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg)
            if (type == "arrow2")
               arrows(xb[1], yb[1], xb[2], yb[2], lwd=lwdb+4, col=col.bg, code=3)
            if (type == "text")
               rect(xb[1], yb[1], xb[2], yb[2], col=col.bg, border=col.bg)
            buffer <<- buffer[-blen]
            return(NULL)
         }

         # z and x for replaying and recording the plot

         if (key == "z") {
            replayPlot(sav)
            buffer <<- list()
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex, drawall=TRUE)
            return(NULL)
         }

         if (key == "x") {
            sav <<- recordPlot()
            buffer <<- list()
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
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex, drawall=TRUE)
            return(NULL)
         }

         # i to toggle info

         if (key == "i") {
            info <<- !info
            if (!info)
               .clear(TRUE, col.bg)
            boxpos <<- .info(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex, drawall=TRUE)
            return(NULL)
         }

         # F11 to print 'old'

         #if (key == "F11")
         #   print(old)

         # F12 to print the buffer

         if (key == "F12")
            print(buffer)

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
