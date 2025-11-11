#!/usr/bin/env Rscript

blomqvist_beta <- function(x, y) {

  stopifnot(length(x) == length(y))

  mx <- median(x)
  my <- median(y)
  lower_left <- (x <= mx & y <= my)
  4 * mean(lower_left) - 1

}

# --------------------------------------------------------------
# Generate bivariate samples (n = 2000) from N(0,1) with rho=0.5
# Apply Mann–Whitney (Wilcoxon rank-sum) test
# --------------------------------------------------------------

set.seed(125)   # remove or change for different random draws

# Sample sizes
n <- 2000
mu <- c(0,0)
Sigma <- matrix(c(  1, 0.5,
                  0.5,   1 ), nrow = 2)

library(MASS)
xy <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

head(xy)

x <- xy[,1]
y <- xy[,2]

# Perform Mann–Whitney / Wilcoxon rank-sum test
test_result <- wilcox.test(x, y, alternative = "two.sided", exact = FALSE)

# Various dependence measures
tauK <- cor(x, y, method = "kendall")
rhoP <- cor(x, y, method = "pearson")
rhoS <- cor(x, y, method = "spearman")
betaB <- blomqvist_beta(x, y)

cat("\nMann–Whitney (Wilcoxon rank-sum) test result:\n")
print(test_result)

cat("\nDependence measure 1. Pearson's rho:\n")
print(rhoP)
cat("\nDependence measure 2. Spearman's rho:\n")
print(rhoS)
cat("\nDependence measure 3. Kendall's tau:\n")
print(tauK)
cat("\nDependence measure 4. Blomqvist's beta:\n")
print(betaB)

