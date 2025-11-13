#!/usr/bin/env Rscript

# power_vs_sample_chisq.R
# Plot power vs. sample size for a chi-squared goodness-of-fit test

# Parameters
alpha <- 0.05                     # Significance level
p0 <- c(0.25, 0.25, 0.25, 0.25)   # Expected probabilities for 4 categories under H0
p1 <- c(0.10, 0.20, 0.30, 0.40)   # True probabilities under H1
df <- length(p0) - 1              # Degrees of freedom

df

# Function to compute power analytically
# n=sample size, 
# p0 = expected probabilities per category under H0
# p1=true probabilities under H1 
# alpha = significance level
power_chisq <- function(n, p0, p1, alpha) {
  lambda <- n * sum((p1 - p0)^2 / p0)                # Noncentrality parameter
  crit <- qchisq(1 - alpha, df = length(p0) - 1)     # Critical value of the chi-squared under H0
  power <- 1 - pchisq(crit, df = length(p0) - 1, ncp = lambda) # Computes P (chisq > chisq_crit)
  return(power)
}

# Sample sizes to test
n_values <- seq(10, 150, by = 5)
power_values <- sapply(n_values, power_chisq, p0 = p0, p1 = p1, alpha = alpha)

# PDF output
pdf("PowerPlot.pdf", width = 6, height = 5)

# Plot
plot(n_values, power_values, type = "l", lwd = 2, col = "blue",
     xlab = "Sample size (n)",
     ylab = "Power",
     main = "Power vs. Sample Size for Chi-squared Test")
abline(h = 0.9, lty = 2, col = "red")  # Typical power threshold
abline(h = 0.95, lty = 2, col = "magenta")  # Typical power threshold
text(15, 0.85, "90% power", col = "red", pos = 4)
text(15, 0.975, "95% power", col = "magenta", pos = 4)

grid()

print(n_values)
print(power_values)

# Close display
dev.off()

