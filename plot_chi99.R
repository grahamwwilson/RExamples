#!/usr/bin/env Rscript

# plot_chi99.R
# Plot the 99% quantile of the chi-squared distribution for df = 1:100

# Load necessary library (optional, base R is enough here)
# library(ggplot2)  # Uncomment if you want to use ggplot2

# Degrees of freedom
df <- 1:100

# 99% quantile of chi-squared for each df
chi99 <- qchisq(0.99, df)
print(chi99)
print(chi99/df)

# Create plot and save to file
png(filename="chi99_quantile.png", width=800, height=600)
plot(df, chi99, type="b", pch=19, col="blue",
     xlab="Degrees of Freedom",
     ylab="99% Quantile",
     main="99% Quantile of Chi-squared Distribution")
grid()
dev.off()

cat("Plot saved to chi99_quantile.png\n")

