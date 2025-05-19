.info <- function(mode, col, colnum, lwd.draw, lwd.eraser, lwd.symb, cex.pt, cex.txt, snap, info) {

   if (!info)
      return(invisible())

   col.bg <- par("bg")

   if (col.bg == "transparent")
      col.bg <- "white"

   .clear(info, col.bg)

   # add mode text

   cords <- c(grconvertX(0.00, from="ndc", to="user"),
              grconvertY(0.99, from="ndc", to="user"))

   txt <- paste0("Mode: ", mode)
   text(cords[1], cords[2], txt, pos=4, cex=0.5/par("cex"))

   # add lwd / cex text

   cords <- c(grconvertX(0.00, from="ndc", to="user"),
              grconvertY(0.98, from="ndc", to="user"))

   if (mode == "draw")
      lwd <- lwd.draw
   if (mode == "eraser")
      lwd <- lwd.eraser
   if (mode %in% c("rect", "circle", "line", "arrow", "arrow2"))
      lwd <- lwd.symb
   if (mode == "point")
      cex.pt <- cex.pt
   if (mode %in% c("text", "type"))
      cex.txt <- cex.txt

   if (mode %in% c("draw", "eraser", "rect", "circle", "line", "arrow", "arrow2"))
      txt <- paste0("Line width: ", lwd)
   if (mode %in% c("line", "arrow", "arrow2") && snap)
      txt <- paste0(txt, " (snap)")
   if (mode == "point")
      txt <- paste0("Point size: ", cex.pt)
   if (mode %in% c("text", "type"))
      txt <- paste0("Text size: ", cex.txt)

   text(cords[1], cords[2], txt, pos=4, cex=0.5/par("cex"))

   # add color boxes

   xpos <- seq(0.10, 0.10 + 9 * 0.024, length.out=9)

   for (i in 1:length(col)) {

      cords1 <- c(grconvertX(xpos[i], from="ndc", to="user"),
                  grconvertY(0.975, from="ndc", to="user"))
      cords2 <- c(grconvertX(xpos[i] + 0.02, from="ndc", to="user"),
                  grconvertY(0.995, from="ndc", to="user"))

      rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col[i], border=col.bg, lwd=6)

      if (i == colnum)
         rect(cords1[1], cords1[2], cords2[1], cords2[2], border="black", lwd=2)

   }

   return(invisible())

}

.clear <- function(info, col.bg) {

   if (!info)
      return(invisible())

   cords1 <- c(grconvertX(0.00, from="ndc", to="user"),
               grconvertY(0.96, from="ndc", to="user"))
   cords2 <- c(grconvertX(0.34, from="ndc", to="user"),
               grconvertY(1.00, from="ndc", to="user"))

   rect(cords1[1], cords1[2], cords2[1], cords2[2], col=col.bg, border=NA)

   return(invisible())

}
