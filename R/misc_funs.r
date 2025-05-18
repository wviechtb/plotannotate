blank <- function(annotate=TRUE, bg="white", ...) {
   par(bg=bg)
   plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
   dots <- list(...)
   if (annotate)
      annotate(col=dots$col, lwd=dots$lwd, cex=dots$cex, info=dots$info)
   return(invisible())
}
