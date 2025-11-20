#!/usr/bin/env Rscript

normality_tests <- function(x) {
  results <- list()

  ## Base R
  results$shapiro <- tryCatch(shapiro.test(x), error = function(e) NULL)

  # KS with estimated parameters (not valid)
  results$ks <- ks.test(x, "pnorm", mean(x), sd(x))

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

normality_report <- function(x, filename="normality_report.pdf") {
  library(grDevices)
  pdf(filename, width = 8, height = 6)

  par(mfrow=c(2,2))

  hist(x, freq=FALSE, col="lightgray",
       main="Histogram + KDE")
  lines(density(x), lwd=2)
  curve(dnorm(x, mean(x), sd(x)),
        add=TRUE, col="red", lwd=2)

  qqnorm(x, main="Q-Q Plot")
  qqline(x, col="red", lwd=2)

  plot(ecdf(x), main="ECDF")
  curve(pnorm(x, mean(x), sd(x)), add=TRUE,
        col="red", lwd=2)

  plot(density(x), main="Density Curve")

  tests <- normality_tests(x)

  cat("\n========== Normality Test Results ==========\n")
  print(tests)
  cat("\n===========================================\n")

  dev.off()
}

x <- rlnorm(100)
normality_report(x)
