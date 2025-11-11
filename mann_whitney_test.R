#!/usr/bin/env Rscript

blomqvist_beta <- function(x, y) {

  stopifnot(length(x) == length(y))

  mx <- median(x)
  my <- median(y)
  lower_left <- (x <= mx & y <= my)
  4 * mean(lower_left) - 1

}

# ------------------------------------------------------------
# Generate 2 independent samples (n = 2000) from N(0,1)
# Apply Mann–Whitney (Wilcoxon rank-sum) test
# ------------------------------------------------------------

set.seed(125)   # remove or change for different random draws

# Sample sizes
n <- 2000

# Independent standard normal random samples
x <- rnorm(n, mean = 0, sd = 1)
y <- rnorm(n, mean = 0, sd = 1)

# Perform Mann–Whitney / Wilcoxon rank-sum test
test_result <- wilcox.test(x, y, alternative = "two.sided", exact = FALSE)

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
