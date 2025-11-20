#!/usr/bin/env Rscript

### ---------------------------------------------------------
###  Goodness-of-fit tests with diagnostic plots
### ---------------------------------------------------------

set.seed(126)
pdf("gof_plots.pdf", width = 7, height = 6)

### =========================================================
### 1. CHI-SQUARED GOODNESS-OF-FIT (Discrete)
### =========================================================

observed <- c(25, 18, 22, 35)
names(observed) <- c("A","B","C","D")
expected_probs <- rep(0.25, 4)
chisq <- chisq.test(observed, p = expected_probs)
chisq

# --- Plot: Observed vs Expected ---
bar_centers <- barplot(observed,
                       main = "Chi-Squared GOF: Observed vs Expected",
                       ylab = "Counts",
                       col = "lightgray")

expected_counts <- expected_probs * sum(observed)

points(bar_centers, expected_counts, pch = 19, col = "red")
segments(bar_centers, expected_counts, bar_centers, observed, col="red")
legend("topright", legend=c("Observed","Expected"),
       pch=c(22,19), pt.bg=c("lightgray","red"))

mtext(paste("Test statistic =", round(chisq$statistic, 3),
            "p-value =", signif(chisq$p.value, 3)))

### =========================================================
### 2. KOLMOGOROV–SMIRNOV TEST FOR EXPONENTIAL DISTRIBUTION
### =========================================================

x <- rexp(500, rate = 1)
ks_result <- ks.test(x, "pexp", rate = 1)
ks_result

# --- Histogram + fitted curve ---
hist(x, breaks = 20, freq = FALSE,
     main = "Exponential Fit: Histogram + PDF",
     xlab = "x", col = "lightgray")
curve(dexp(x, rate = 1), add = TRUE, lwd = 2)

# --- CDF comparison ---
plot.ecdf(x, main = "ECDF vs Exponential CDF",
          xlab = "x", ylab = "CDF")
curve(pexp(x, rate = 1), add = TRUE, col = "red", lwd = 2)

legend("bottomright", legend=c("ECDF","Theoretical CDF"),
       col=c("black","red"), lwd=2)
mtext(paste("KS statistic =", round(ks_result$statistic,3),
            "p-value =", signif(ks_result$p.value,3)))

### =========================================================
### 3. SHAPIRO–WILK NORMALITY TEST
### =========================================================

y <- rnorm(4000, mean = 2.0, sd = 1.0)  # non-normal data
shap <- shapiro.test(y)
shap
kstestn <- ks.test(y,"pnorm", mean=mean(y), sd=sd(y))
kstestn
kstestl <- ks.test(y,"plnorm", meanlog = 0, sdlog = 0.5)
kstestl

# --- Histogram + Normal fit ---
hist(y, breaks = 20, freq = FALSE,
     main = "Normality Check: Histogram + Normal PDF",
     xlab = "y", col = "lightgray")
curve(dnorm(x, mean=mean(y), sd=sd(y)), add=TRUE, lwd=2)

# --- Q–Q Plot ---
qqnorm(y, main = "Q-Q plot for normality")
qqline(y, col = "red")

mtext(paste("Shapiro-Wilk W =", round(shap$statistic,3),
            "p-value =", signif(shap$p.value,3)))

### =========================================================

### =========================================================
### 4. SHAPIRO–WILK NORMALITY TEST
### =========================================================

z <- rnorm(200, mean = 0, sd = 1.0)  # Normal data
shap <- shapiro.test(z)
shap
kstestn <- ks.test(z,"pnorm")
kstestn

# --- Histogram + Normal fit ---
hist(z, breaks = 20, freq = FALSE,
     main = "Normality Check: Histogram + Normal PDF",
     xlab = "y", col = "lightgray")
curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, lwd=2)

# --- Q–Q Plot ---
qqnorm(z, main = "Q-Q plot for normality")
qqline(z, col = "red")

mtext(paste("Shapiro-Wilk W =", round(shap$statistic,3),
            "p-value =", signif(shap$p.value,3)))

### =========================================================

dev.off()
cat("Plots saved to gof_plots.pdf\n")

