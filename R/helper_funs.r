.info <- function(mode, old, col, colnum, lwd, cex, snap, smooth, lty, pch, info, icex, drawall=FALSE) {

   if (!info)
      return(invisible())

   col.bg <- par("bg")

   if (col.bg == "transparent")
      col.bg <- "white"

   if (drawall)
      .clear(info, col.bg)

   # add mode text

   if (drawall || mode != old$mode || (mode %in% c("draw", "line", "arrow", "arrow2") && (snap != old$snap || smooth != old$smooth))) {

      .clear(info, col.bg, x1=0.00, y1=0.985, x2=0.118, y2=1.00)

      cords <- c(grconvertX(0.00, from="ndc", to="user"),
                 grconvertY(0.99, from="ndc", to="user"))

      txt <- paste0("Mode: ", mode)

      if (mode == "draw" && smooth)
         txt <- paste0(txt, " (smooth)")
      if (mode %in% c("line", "arrow", "arrow2") && snap)
         txt <- paste0(txt, " (snap)")

      text(cords[1], cords[2], txt, pos=4, cex=0.5/par("cex"))

   }

   # add lwd / cex text

   if (drawall || mode != old$mode || any(lwd != old$lwd) || any(cex != old$cex)) {

      .clear(info, col.bg, x1=0.00, y1=0.975, x2=0.118, y2=0.985)

      cords <- c(grconvertX(0.00, from="ndc", to="user"),
                 grconvertY(0.98, from="ndc", to="user"))

      if (mode == "draw")
         txt <- paste0("Line width: ", lwd[1])
      if (mode %in% c("line", "arrow", "arrow2", "rect", "circle", "circle2", "ellipse"))
         txt <- paste0("Line width: ", lwd[2])
      if (mode == "eraser")
         txt <- paste0("Eraser width: ", lwd[3])
      if (mode == "point")
         txt <- paste0("Point size: ", cex[1])
      if (mode %in% c("text", "type"))
         txt <- paste0("Text size: ", cex[2])

      text(cords[1], cords[2], txt, pos=4, cex=0.5/par("cex"))

   }

   # add tool boxes

   modes <- c("draw", "point", "line", "arrow", "arrow2", "rect", "circle", "ellipse", "text", "eraser")
   xpos <- 0.12 + 0:9 * 0.024

   modesel <- mode

   if (modesel == "circle2")
      modesel <- "circle"
   if (modesel == "type")
      modesel <- "text"

   modenum <- which(modes == modesel)
   modepos <- matrix(NA_real_, nrow=10, ncol=4)

   if (lty == 2)
      lty <- "32"
   if (lty == 3)
      lty <- "12"

   dx <- c(0.1963, 0.2254, 0.2524, 0.2771, 0.2995, 0.3196, 0.3372, 0.3524, 0.3651, 0.3742, 0.3799, 0.3845, 0.3878, 0.3827, 0.3707, 0.354, 0.3352, 0.3164, 0.3002, 0.2887, 0.2845, 0.2858, 0.2908, 0.2993, 0.311, 0.3256, 0.3428, 0.3643, 0.3921, 0.4242, 0.4582, 0.492, 0.5233, 0.5499, 0.5694, 0.5841, 0.5971, 0.609, 0.62, 0.6305, 0.6401, 0.6486, 0.6559, 0.662, 0.667, 0.6709, 0.6737, 0.6755, 0.6763, 0.676)
   dy <- c(0.7292, 0.7438, 0.7561, 0.7662, 0.7739, 0.7792, 0.7822, 0.7826, 0.7805, 0.7749, 0.7661, 0.756, 0.7444, 0.725, 0.6991, 0.6688, 0.6363, 0.6037, 0.573, 0.5463, 0.5258, 0.5108, 0.4997, 0.4917, 0.4858, 0.4814, 0.4775, 0.4756, 0.4784, 0.4841, 0.4908, 0.4966, 0.4995, 0.4976, 0.4889, 0.4752, 0.4594, 0.4417, 0.4228, 0.4028, 0.3813, 0.3583, 0.3335, 0.3071, 0.2789, 0.2492, 0.218, 0.1852, 0.1508, 0.1148)
   dx <- (dx - min(dx)) / (max(dx) - min(dx))
   dy <- (dy - min(dy)) / (max(dy) - min(dy))
   dx <- dx * (xpos[1] + 0.02 - 0.005 - (xpos[1] + 0.005)) + (xpos[1] + 0.005)
   dy <- dy * (0.990 - 0.980) + 0.980

   dsx <- c(0.0858, 0.0907, 0.0961, 0.102, 0.1084, 0.1153, 0.1226, 0.1305, 0.1388, 0.1476, 0.1569, 0.1667, 0.177, 0.1878, 0.199, 0.2108, 0.223, 0.2357, 0.2489, 0.2626, 0.2769, 0.2915, 0.3067, 0.3225, 0.3388, 0.3556, 0.3731, 0.3912, 0.4098, 0.429, 0.4485, 0.4686, 0.4891, 0.5103, 0.5319, 0.5539, 0.5763, 0.599, 0.622, 0.6455, 0.6695, 0.6938, 0.7186, 0.7437, 0.7693, 0.7952, 0.8216, 0.8483, 0.8755, 0.9031)
   dsy <- c(0.9509, 0.9247, 0.8987, 0.873, 0.8475, 0.8223, 0.7973, 0.7725, 0.748, 0.7238, 0.6998, 0.6761, 0.6527, 0.6295, 0.6065, 0.5836, 0.5611, 0.5388, 0.5169, 0.4954, 0.474, 0.4526, 0.4316, 0.4111, 0.3913, 0.3723, 0.3535, 0.3351, 0.3171, 0.2997, 0.2832, 0.2677, 0.2528, 0.2386, 0.2251, 0.2123, 0.2002, 0.1891, 0.1786, 0.1689, 0.1598, 0.1515, 0.144, 0.1372, 0.1311, 0.1258, 0.1213, 0.1175, 0.1144, 0.112)
   dsx <- (dsx - min(dsx)) / (max(dsx) - min(dsx))
   dsy <- (dsy - min(dsy)) / (max(dsy) - min(dsy))
   dsx <- dsx * (xpos[1] + 0.02 - 0.005 - (xpos[1] + 0.005)) + (xpos[1] + 0.005)
   dsy <- dsy * (0.990 - 0.980) + 0.980

   for (i in 1:10) {

      cords1 <- c(grconvertX(xpos[i], from="ndc", to="user"),
                  grconvertY(0.975, from="ndc", to="user"))
      cords2 <- c(grconvertX(xpos[i] + 0.02, from="ndc", to="user"),
                  grconvertY(0.995, from="ndc", to="user"))

      modepos[i,] <- c(cords1, cords2)

      if (!drawall && colnum != old$colnum)
         next

      if (i == 1 && (drawall || (mode == "draw" || old$mode == "draw"))) { # freehand drawing / with smoothing
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         if (smooth) {
            lines(grconvertX(dsx, from="ndc", to="user"), grconvertY(dsy, from="ndc", to="user"), lwd=lwd[1]*icex)
         } else {
            lines(grconvertX(dx, from="ndc", to="user"), grconvertY(dy, from="ndc", to="user"), lwd=lwd[1]*icex)
         }
      }

      if (i == 2 && (drawall || (mode == "point" || old$mode == "point" || cex[1] != old$cex[1] || pch != old$pch))) { # point
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         points(grconvertX(xpos[i] + 0.01, from="ndc", to="user"),
                grconvertY(0.985, from="ndc", to="user"), pch=pch, cex=min(cex[1],1.7)/par("cex")*sqrt(icex))
      }

      if (i == 3 && (drawall || (mode == "line" || old$mode == "line" || snap != old$snap || lwd[2] != old$lwd[2] || lty != old$lty))) { # line / with snap mode
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         if (snap) {
            segments(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                     grconvertY(0.985, from="ndc", to="user"),
                     grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                     grconvertY(0.985, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, lty=lty)
         } else {
            segments(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                     grconvertY(0.980, from="ndc", to="user"),
                     grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                     grconvertY(0.990, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, lty=lty)
         }
      }

      if (i == 4 && (drawall || (mode == "arrow" || old$mode == "arrow" || snap != old$snap || lwd[2] != old$lwd[2]))) { # arrow / with snap mode
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         if (snap) {
            arrows(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                   grconvertY(0.985, from="ndc", to="user"),
                   grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                   grconvertY(0.985, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, length=0.10)
         } else {
            arrows(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                   grconvertY(0.980, from="ndc", to="user"),
                   grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                   grconvertY(0.990, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, length=0.10)
         }
      }

      if (i == 5 && (drawall || (mode == "arrow2" || old$mode == "arrow2" || snap != old$snap || lwd[2] != old$lwd[2]))) { # double-sided arrow / with snap mode
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         if (snap) {
            arrows(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                   grconvertY(0.985, from="ndc", to="user"),
                   grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                   grconvertY(0.985, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, length=0.10, code=3)
         } else {
            arrows(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
                   grconvertY(0.980, from="ndc", to="user"),
                   grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
                   grconvertY(0.990, from="ndc", to="user"), lwd=min(lwd[2],8)*icex, length=0.10, code=3)
         }
      }

      if (i == 6 && (drawall || (mode == "rect" || old$mode == "rect" || lwd[2] != old$lwd[2]))) { # rectangle
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         rect(grconvertX(xpos[i] + 0.003, from="ndc", to="user"),
              grconvertY(0.980, from="ndc", to="user"),
              grconvertX(xpos[i] + 0.017, from="ndc", to="user"),
              grconvertY(0.990, from="ndc", to="user"), lwd=min(lwd[2],8)*icex)
      }

      if (i == 7 && (drawall || (mode %in% c("circle", "circle2")) || old$mode %in% c("circle", "circle2") || lwd[2] != old$lwd[2])) { # circle / circle2
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         points(grconvertX(xpos[i] + 0.01, from="ndc", to="user"),
                grconvertY(0.985, from="ndc", to="user"), pch=21, cex=1.5/par("cex"), lwd=min(lwd[2],8)*icex)
         if (mode == "circle2") {
            points(grconvertX(xpos[i] + 0.01, from="ndc", to="user"),
                   grconvertY(0.985, from="ndc", to="user"), pch=19, cex=0.4/par("cex"), col="gray60")
         }
      }

      if (i == 8 && (drawall || (mode == "ellipse" || old$mode == "ellipse") || lwd[2] != old$lwd[2])) { # ellipse
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         xy <- conicfit::calculateEllipse(grconvertX(xpos[i] + 0.01, from="ndc", to="user"),
                                          grconvertY(0.985, from="ndc", to="user"), 0.01, 0.005, 180/pi*70, steps=101)
         lines(xy[,1], xy[,2], lwd=min(lwd[2],8)*icex)
      }

      if (i == 9  && (drawall || (mode %in% c("text", "type") || old$mode %in% c("text", "type") || cex[2] != old$cex[2]))) { # text / type
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         text(grconvertX(xpos[i] + 0.01, from="ndc", to="user"),
              grconvertY(0.985, from="ndc", to="user"), "T", font=2, cex=min(1.2, cex[2])/par("cex")*sqrt(icex))
      }

      if (i == 10 && (drawall || (mode == "eraser" || old$mode == "eraser" || lwd[3] != old$lwd[3]))) { # eraser
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=col.bg, lwd=2*icex)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border="gray80", lwd=1*icex)
         xl <- xpos[i] + 0.005
         xr <- xpos[i] + 0.02 - 0.005
         yb <- 0.975 + 0.005
         yt <- 0.995 - 0.005
         lines(grconvertX(c(xl + 0.004, xr, xr - 0.004, xl, xl + 0.004), from="ndc", to="user"),
               grconvertY(c(yb, yt - 0.004, yt, yb + 0.004, yb), from="ndc", to="user"),
               lwd=max(0.5, min(0.1*lwd[3],8))*icex)
      }

      if (i == modenum)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], border="black", lwd=2*icex)

   }

   # add color boxes

   xpos <- xpos[length(xpos)] + 1:9 * 0.024

   colpos <- matrix(NA_real_, nrow=length(col), ncol=4)

   for (i in 1:length(col)) {

      cords1 <- c(grconvertX(xpos[i], from="ndc", to="user"),
                  grconvertY(0.975, from="ndc", to="user"))
      cords2 <- c(grconvertX(xpos[i] + 0.02, from="ndc", to="user"),
                  grconvertY(0.995, from="ndc", to="user"))

      colpos[i,] <- c(cords1, cords2)

      if (!drawall && colnum == old$colnum)
         next

      if (drawall || old$colnum == i)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col[i], border=col.bg, lwd=10*icex)

      if (i == colnum)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], border="black", lwd=2*icex)

   }

   return(list(modepos, colpos))

}

.arrows <- function(x0, y0, x1, y1, col, lwd, lty, code = 1) {

   segments(x0, y0, x1, y1, col=col, lwd=lwd, lty=lty)

   arrow_length <- 0.01
   dx <- x1 - x0
   dy <- y1 - y0
   len <- sqrt(dx^2 + dy^2)

   dx <- dx / len
   dy <- dy / len

   arrows(x1 - dx * arrow_length, y1 - dy * arrow_length, x1, y1, col=col, lwd=lwd)

   if (code == 3)
      arrows(x0 + dx * arrow_length, y0 + dy * arrow_length, x0, y0, col=col, lwd=lwd)

   return(invisible())

}

.clear <- function(info, col.bg, x1=0.00, y1=0.97, x2=0.60, y2=1.00) {

   if (!info)
      return(invisible())

   cords1 <- c(grconvertX(x1, from="ndc", to="user"),
               grconvertY(y1, from="ndc", to="user"))
   cords2 <- c(grconvertX(x2, from="ndc", to="user"),
               grconvertY(y2, from="ndc", to="user"))

   rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=NA)

   return(invisible())

}

.smooth <- function(x, y, n=1000) {
   if (length(x) >= 10L) { # only run smoother for 10 or more points
      dx <- diff(x)
      dy <- diff(y)
      dist <- c(0, cumsum(sqrt(dx^2 + dy^2)))
      newdat <- seq(min(dist), max(dist), length.out=n)
      x.smooth <- predict(loess(x ~ dist), newdata=newdat)
      y.smooth <- predict(loess(y ~ dist), newdata=newdat)
   } else {
      return(list(x=x, y=y))
   }
   return(list(x=x.smooth, y=y.smooth))
}
