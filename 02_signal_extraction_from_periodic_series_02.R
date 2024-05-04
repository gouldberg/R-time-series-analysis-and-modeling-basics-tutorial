
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate periodic series:  3 time series  (cos + sine)
# ------------------------------------------------------------------------------


# time length
n <- 100


# cycles per unit time (here: t)
cyc <- c(n * 0.06, n * 0.1, n * 0.4)


# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)



# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)




# ----------
# generate time series
# different frequency and amplitude

x1 <- a[1] * cos(2 * pi * 1:n * cyc[1] / n) + b[1] * sin(2 * pi * 1:n * cyc[1] / n)


x2 <- a[2] * cos(2 * pi * 1:n * cyc[2] / n) + b[2] * sin(2 * pi * 1:n * cyc[2] / n)


x3 <- a[3] * cos(2 * pi * 1:n * cyc[3] / n) + b[3] * sin(2 * pi * 1:n * cyc[3] / n)




# ----------
# mixtures of periodic series with multiple frequencies and amplitudes

xall <- x1 + x2 + x3




# ----------
# A = sqrt(a^2 + b^2):  hight of amplitude for each time series

( A <- sqrt(a^2 + b^2) )




# ----------
# plot x1, x2, x3 and xall

graphics.off()

par(mfrow = c(2,2))

plot.ts(x1, ylim = c(-10, 10), main = paste0("freq: ", cyc[1], "/", n, "  A^2: ", A[1]^2))
abline(h = c(A[1], -A[1]), lty = 2, col = "gray")

plot.ts(x2, ylim = c(-10, 10), main = paste0("freq: ", cyc[2], "/", n, "  A^2: ", A[2]^2))
abline(h = c(A[2], -A[2]), lty = 2, col = "gray")

plot.ts(x3, ylim = c(-10, 10), main = paste0("freq: ", cyc[3], "/", n, "  A^2: ", A[3]^2))
abline(h = c(A[3], -A[3]), lty = 2, col = "gray")

plot.ts(xall, ylim = c(-16, 16), main = "sum")




# ------------------------------------------------------------------------------
# extract signals from noisy periodic data
# ------------------------------------------------------------------------------


graphics.off()

astsa::SigExtract(xall)

