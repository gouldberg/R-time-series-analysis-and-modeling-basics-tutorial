
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate periodic series:  3 time series  (cos + sine)
# ------------------------------------------------------------------------------


##### chenge the time length 100 --> 128
# n <- 100
n <- 128


t <- 1:n


# omega:  cycels per unit time
omega <- c(0.06, 0.1, 0.4)


# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)


# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)




# ----------
# generate time series
# different frequency and amplitude

x1 <- a[1] * cos(2 * pi * omega[1] * t) + b[1] * sin(2 * pi * omega[1] * t)


x2 <- a[2] * cos(2 * pi * omega[2] * t) + b[2] * sin(2 * pi * omega[3] * t)


x3 <- a[3] * cos(2 * pi * omega[3] * t) + b[3] * sin(2 * pi * omega[3] * t)




# ----------
# mixtures of periodic series with multiple frequencies and amplitudes

xall <- x1 + x2 + x3




# ------------------------------------------------------------------------------
# plot time domain and frequency domain
# ------------------------------------------------------------------------------

# when n = 100:  the phase of x1, x2, and x3 are all 0 at the end of time 100

100 * omega


# when n = 128:  the phase of x1, x2, and x3 are all different at the end of time 128

128 * omega



graphics.off()

par(mfcol = c(2,4))

plot.ts(x1, ylim = c(-10, 10), main = paste0("omega: ", omega[1], "  A^2: ", A[1]^2))
abline(v = seq(0, n, 1 / 0.06), lty = 2, col = "gray")

mvspec(x1, log = "no", type = "h")


plot.ts(x2, ylim = c(-10, 10), main = paste0("omega: ", omega[2], "  A^2: ", A[2]^2))
abline(v = seq(0, n, 1 / 0.1), lty = 2, col = "gray")

mvspec(x2, log = "no", type = "h")


plot.ts(x3, ylim = c(-10, 10), main = paste0("omega: ", omega[3], "  A^2: ", A[3]^2))
abline(v = seq(0, n, 1 / 0.4), lty = 2, col = "gray")

mvspec(x3, log = "no", type = "h")

plot.ts(xall, ylim = c(-16, 16), main = "sum")

mvspec(xall, log = "no", type = "h")





# ------------------------------------------------------------------------------
# Apply tapering
# ------------------------------------------------------------------------------


par(mfrow = c(3,2))

spc0 <- mvspec(x1, type = "h")

spc1 <- mvspec(x1, taper = 0.1, type = "h")

spc2 <- mvspec(x1, taper = 0.2, type = "h")

spc3 <- mvspec(x1, taper = 0.3, type = "h")

spc4 <- mvspec(x1, taper = 0.4, type = "h")

# full taper (taper = 0.5)
spc5 <- mvspec(x1, taper = 0.5, type = "h")



# requreis "*2" since mvspec gives only at frequency up to 0.5
sum(spc0$spec) * 2
sum(spc1$spec) * 2
sum(spc2$spec) * 2
sum(spc3$spec) * 2
sum(spc4$spec) * 2
sum(spc5$spec) * 2

sum(x1^2)



# note that function "mvspec" produces little different output from periodogram generated from fft()





# ----------
par(mfrow = c(3,2))

spc0 <- mvspec(xall, type = "h")

spc1 <- mvspec(xall, taper = 0.1, type = "h")

spc2 <- mvspec(xall, taper = 0.2, type = "h")

spc3 <- mvspec(xall, taper = 0.3, type = "h")

spc4 <- mvspec(xall, taper = 0.4, type = "h")

# full taper (taper = 0.5)
spc5 <- mvspec(xall, taper = 0.5, type = "h")



sum(spc0$spec) * 2
sum(spc1$spec) * 2
sum(spc2$spec) * 2
sum(spc3$spec) * 2
sum(spc4$spec) * 2
sum(spc5$spec) * 2

sum(xall^2)





# ------------------------------------------------------------------------------
# parametric spectral estimation
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,4))


mvspec(x1, type = "h")
spec.ar(x1, method = "burg")


mvspec(x2, type = "h")
spec.ar(x2, method = "burg")


mvspec(x3, type = "h")
spec.ar(x3, method = "burg")


mvspec(xall, type = "h")
spec.ar(xall, method = "burg")



