#' @method print plsreg1
#' @export
print.plsreg1 <-
  function(x, ...)
  {
    cat("\nPLS Regression 1\n")
    cat(rep("-",44), sep="")
    cat("\n$x.scores    ", "X-scores (T-components)")
    cat("\n$x.loads     ", "X-loadings")
    cat("\n$y.scores    ", "Y-scores (U-components)")
    cat("\n$y.loads     ", "Y-loadings")
    cat("\n$cor.xyt     ", "score correlations")
    cat("\n$raw.wgs     ", "raw weights")
    cat("\n$mod.wgs     ", "modified weights")
    cat("\n$std.coefs   ", "standard coefficients")
    cat("\n$reg.coefs   ", "regular coefficients")
    cat("\n$R2          ", "R-squared")
    cat("\n$R2Xy        ", "explained variance of X-y by T")
    cat("\n$y.pred      ", "y-predicted")
    cat("\n$resid       ", "residuals")
    cat("\n$T2          ", "T2 hotelling")
    cat("\n$Q2          ", "Q2 cross validation\n")
    cat(rep("-",44), sep="")
    cat("\n\n")
    invisible(x)
  }
