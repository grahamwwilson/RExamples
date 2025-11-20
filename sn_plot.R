#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Plot skew normal density curves for selected degrees of freedom
# and save result to PDF.
# ------------------------------------------------------------

# Degrees of freedom
alphas <- c(2.5, 0.2, 0, -0.1, -0.3, -0.5)
cols <- c("red", "blue", "darkgreen","magenta","green","cyan")

# x-range — choose a range that covers the relevant density
x <- seq(-4.0, 4.0, length.out = 2000)

# PDF output
pdf("sn_densities.pdf", width = 6, height = 5)

# Plot first curve
plot(
  x,
  sn::dsn(x, alpha = alphas[1]),
  type = "l",
  col = cols[1],
  lwd = 2,
  main = expression(paste("Skew normal probability density functions")),
  xlab = expression(x),
  ylab = "Probability density function",
  axes = FALSE
)

# Add remaining curves

for (i in 2:length(alphas)) {
  lines(x, sn::dsn(x, alpha = alphas[i]), col = cols[i], lwd = 2)
}

xticks <- pretty(x, n = 12)
yvals <- sapply(alphas, function(alpha) sn::dsn(x, alpha=5))
yticks <- pretty(range(yvals), n = 8)

# Axes on all 4 sides
axis(1, at=xticks)
axis(2, at=yticks)
axis(3, at=xticks, labels=FALSE)
axis(4, at=yticks, labels=FALSE)

# Add box outline
box()

# Add grid
grid()

# Add legend
legend(
  "topright",
  legend = paste("alpha", alphas),
  col = cols,
  lwd = 2
)

# Close PDF
dev.off()

