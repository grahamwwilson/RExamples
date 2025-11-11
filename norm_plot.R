#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Plot normal density curves for selected degrees of freedom
# and save result to PDF.
# ------------------------------------------------------------

# Degrees of freedom
means <- c(5, 10, 20)
vars <- c(10, 20, 40)
sds <- sqrt(vars)
cols <- c("red", "blue", "darkgreen")

# x-range — choose a range that covers the relevant density
x <- seq(0, 50, length.out = 2000)

# PDF output
pdf("norm_densities.pdf", width = 6, height = 5)

# Plot first curve
plot(
  x,
  dnorm(x, mean= means[1], sd = sds[1]),
  type = "l",
  col = cols[1],
  lwd = 2,
  main = expression(paste("Gaussian probability density functions")),
  xlab = 'x',
  ylab = "Probability density function",
  axes = FALSE
)

# Add remaining curves

for (i in 2:length(means)) {
  lines(x, dnorm(x, mean = means[i], sd=sds[i]), col = cols[i], lwd = 2)
}

xticks <- pretty(x, n = 12)
#yticks <- pretty(range(yvals), n = 8)

# Axes on all 4 sides
axis(1, at=xticks)
axis(2, )
axis(3, at=xticks, labels=FALSE)
axis(4, labels=FALSE)

# Add box outline
box()

# Add grid
grid()

# Legend labels: μ = ..., σ² = ...
#legtxt <- paste0("μ = ", means, ",  σ² = ", vars)

# Legend — build expression vector
legexpr <- vector("list", length(means))
for(i in seq_along(means)) {
  legexpr[[i]] <- substitute(
    mu == M ~ "," ~ sigma^2 == V,
    list(M = means[i], V = vars[i])
  )
}

# Add legend
legend(
  "topright",
  legend = legexpr,
  col = cols,
  lwd = 2
)

# Close PDF
dev.off()

