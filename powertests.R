#!/usr/bin/env Rscript

power_compare <- function(n = 100, reps = 1000) {

#  tests <- c("shapiro", "ad", "cvm", "lillie", "sf", "jb")

  tests <- c("shapiro", "ad", "cvm", "lillie", "pearson", "sf", "jb", "agostino", "anscombe", "dago") 

  out <- matrix(NA, nrow = length(tests), ncol = 4,
                dimnames=list(tests, c("lognormal","t3","uniform","skew")))

  for (dist in colnames(out)) {
    for (tst in rownames(out)) {
      count <- 0
      for (i in 1:reps) {

        x <- switch(dist,
                    lognormal = rlnorm(n),
                    t3 = rt(n, df=3),
                    uniform = runif(n),
                    skew = if (requireNamespace("sn", quietly = TRUE))
                             sn::rsn(n, xi=0, omega=1, alpha=2.5)
                           else rchisq(n, df=3))

        res <- normality_tests(x)
        pval <- tryCatch(res[[tst]]$p.value,
                         error=function(e) NA)

        if (!is.na(pval) && pval < 0.05)
          count <- count + 1
      }
      out[tst, dist] <- count / reps
    }
  }
  return(out)
}

source("normality_tests.R")

power_table <- power_compare()
power_table

