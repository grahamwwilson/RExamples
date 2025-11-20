#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Default test type
test_type <- "two.sided"

# Override if user provides an argument
if (length(args) >= 1) {
    test_type <- args[1]
}

if (!(test_type %in% c("two.sided", "greater", "less"))) {
    stop("Usage: ./permtest.R [two.sided|greater|less]")
}

cat(sprintf("Running permutation test (%s)\n", test_type))

set.seed(121)

# Number of permutations (always do 1 less than a round number
B <- 100000 - 1

# Sample data
x <- rnorm(20, mean = 0)   # Normal with mean of 0 and standard deviation of 1
y <- rnorm(30, mean = 1)   # Normal with mean of 1 and standard deviation of 1

# Observed statistic
T_obs <- mean(y) - mean(x)                         # Observed statistic   

# Combine data
z <- c(x, y)
n1 <- length(x)

# Permutation distribution
T_perm <- replicate(B, {                           # do B MC permutations of the statistic
    idx <- sample(z)                               # sample(z) randomly permutes (shuffles) the combined data vector z
    mean(idx[(n1+1):length(z)]) - mean(idx[1:n1])  # computes T_perm = mean(second group) - mean(first group)
})

### --- Compute p-value (with k for error estimate) ---

if (test_type == "two.sided") {
    exceed <- abs(T_perm) >= abs(T_obs)
} else if (test_type == "greater") {
    exceed <- T_perm >= T_obs
} else if (test_type == "less") {
    exceed <- T_perm <= T_obs
}

k <- sum(exceed)

# Use the +1 correction to avoid p=0 and match exact permutation logic
p_value <- (k+1) / (B+1)
p_se <- sqrt(p_value * (1 - p_value) / B)   # binomial SE. Here we still use the adjusted one. 

# Formatted output
cat(sprintf("Observed statistic (T_obs): %.4f\n", T_obs))
cat(sprintf("k (number of exceedances):  %d\n", k))
cat(sprintf("B (number of perms):        %d\n", B))
cat(sprintf("Adjusted permutation p-value (%s): %.5f\n", test_type, p_value))
cat(sprintf("Binomial SE on p-value:     %.6f\n", p_se))

# PDF output
pdf("permtest.pdf", width = 6, height = 5)
hist(T_perm, breaks = 40,
     main = sprintf("Permutation Test (%s)", test_type),
     xlab = "Permuted test statistic")
abline(v = T_obs, col = "blue", lwd = 2)
dev.off()

# Double check the t test performance
t.test(x, y, alternative = "two.sided", var.equal = TRUE)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)   # Try Welch's test too

