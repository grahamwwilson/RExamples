#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Plot normal density curves for selected degrees of freedom
# and save result to PDF.
# ------------------------------------------------------------

# Degrees of freedom
ms <- c(10, 20, 40)
ns <- c(10, 20, 40)
cols <- c("red", "blue", "darkgreen")

# x-range — choose a range that covers the relevant density
x <- seq(0, 1000, length.out = 2000)

# PDF output
pdf("wilcox_densities.pdf", width = 6, height = 5)

# Plot first curve
plot(
  x,
  dwilcox(x, m= ms[1], n = ns[1]),
  type = "l",
  col = cols[1],
  lwd = 2,
  main = expression(paste("Wilcox rank sum")),
  xlab = 'U',
  ylab = "Probability density function",
  axes = FALSE
)

# Add remaining curves

for (i in 2:length(means)) {
  lines(x, dwilcox(x, m = ms[i], n=ns[i]), col = cols[i], lwd = 2)
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
    m == M ~ "," ~ n == N,
    list(M = ms[i], N = ns[i])
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

