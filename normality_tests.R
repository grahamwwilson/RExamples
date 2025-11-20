normality_tests <- function(x) {
  results <- list()

  ## Base R
  results$shapiro <- tryCatch(shapiro.test(x), error = function(e) NULL)

  # KS with true parameters (valid when parameters are correct)
  results$ks <- ks.test(x, "pnorm", 0.0, 1.0)

  ## nortest package
  if (requireNamespace("nortest", quietly = TRUE)) {
    library(nortest)
    results$ad <- ad.test(x)
    results$cvm <- cvm.test(x)
    results$lillie <- lillie.test(x)
    results$pearson <- pearson.test(x)
    results$sf <- sf.test(x)
  }

  ## tseries package
  if (requireNamespace("tseries", quietly = TRUE)) {
    library(tseries)
    results$jb <- jarque.bera.test(x)
  }

  ## moments package
  if (requireNamespace("moments", quietly = TRUE)) {
    library(moments)
    results$agostino <- agostino.test(x)
    results$anscombe <- anscombe.test(x)
  }

  ## fBasics package
  if (requireNamespace("fBasics", quietly = TRUE)) {
    library(fBasics)
    results$dago <- dagoTest(x)
  }

  return(results)

}
