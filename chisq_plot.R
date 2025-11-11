#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Plot chi-square density curves for selected degrees of freedom
# and save result to PDF.
# ------------------------------------------------------------

# Degrees of freedom
dofs <- c(5, 10, 20)
cols <- c("red", "blue", "darkgreen")

# x-range — choose a range that covers the relevant density
x <- seq(0, 50, length.out = 2000)

# PDF output
pdf("chisq_densities.pdf", width = 6, height = 5)

# Plot first curve
plot(
  x,
  dchisq(x, df = dofs[1]),
  type = "l",
  col = cols[1],
  lwd = 2,
  main = expression(paste("Chi-squared probability density functions")),
  xlab = expression(chi^2),
  ylab = "Probability density function",
  axes = FALSE
)

# Add remaining curves

for (i in 2:length(dofs)) {
  lines(x, dchisq(x, df = dofs[i]), col = cols[i], lwd = 2)
}

xticks <- pretty(x, n = 12)
yvals <- sapply(dofs, function(df) dchisq(x, df=5))
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
  legend = paste(dofs, "dof"),
  col = cols,
  lwd = 2
)

# Close PDF
dev.off()

