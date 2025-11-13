#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Plot non-central chi-square density curves for selected degrees of freedom
# and save result to PDF.
# ------------------------------------------------------------

# Degrees of freedom
dofs <- c(5, 10, 20)
ncps <- c(5, 10, 20)
cols <- c("red", "blue", "darkgreen")

# x-range — choose a range that covers the relevant density
x <- seq(0, 50, length.out = 2000)

# PDF output
pdf("nchisq_densities.pdf", width = 6, height = 5)

# Plot first curve
plot(
  x,
  dchisq(x, df = dofs[1], ncp = ncps[1]),
  type = "l",
  col = cols[1],
  lwd = 2,
  main = expression(paste("Non-central chi-squared probability density functions")),
  xlab = expression(chi^2),
  ylab = "Probability density function",
  axes = FALSE
)

# Add remaining curves

for (i in 2:length(dofs)) {
  lines(x, dchisq(x, df = dofs[i], ncp=ncps[i]), col = cols[i], lwd = 2)
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

# Legend — build expression vector
legexpr <- vector("list", length(dofs))
for(i in seq_along(dofs)) {
  legexpr[[i]] <- substitute(
    k == M ~ "," ~ lambda == L,
    list(M = dofs[i], L = ncps[i])
  )
}


#  legend = paste(dofs, "dof", "ncp=", ncps),

# Add legend
legend(
  "topright",
  legend = legexpr,
  col = cols,
  lwd = 2
)

# Close PDF
dev.off()

