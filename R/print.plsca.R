#' @method print plsca
#' @export
print.plsca <-
function(x,...)
{       
  cat("\nPLS Canonical Analysis\n")
  cat(rep("-",51), sep="")
  cat("\n$x.scores   ", "X-scores (T-components)")
  cat("\n$x.wgs      ", "X-weights")
  cat("\n$x.loads    ", "X-loadings")
  cat("\n$y.scores   ", "Y-scores (U-components)")
  cat("\n$y.wgs      ", "Y-weights")
  cat("\n$y.loads    ", "Y-loadings")
  cat("\n$cor.xt     ", "X,T correlations")
  cat("\n$cor.yu     ", "Y,U correlations")
  cat("\n$cor.tu     ", "T,U correlations")
  cat("\n$cor.xu     ", "X,U correlations")
  cat("\n$cor.yt     ", "Y,T correlations")
  cat("\n$R2X        ", "explained variance of X by T")
  cat("\n$R2Y        ", "explained variance of Y by U")
  cat("\n$com.xu     ", "communality of X with U")
  cat("\n$com.yt     ", "communality of Y with T\n")
  cat(rep("-",51), sep="")
  cat("\n\n")
  invisible(x)
}
