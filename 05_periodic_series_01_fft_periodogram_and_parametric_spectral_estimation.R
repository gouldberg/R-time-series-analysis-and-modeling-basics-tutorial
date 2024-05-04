
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate periodic series:  3 time series  (cos + sine)
# ------------------------------------------------------------------------------


# time length
n <- 50


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


x2 <- a[2] * cos(2 * pi * omega[2] * t) + b[2] * sin(2 * pi * omega[2] * t)


x3 <- a[3] * cos(2 * pi * omega[3] * t) + b[3] * sin(2 * pi * omega[3] * t)




# ----------
# A = sqrt(a^2 + b^2):  hight of amplitude for each time series

( A <- sqrt(a^2 + b^2) )




# ----------
# plot x1, x2, x3

graphics.off()

par(mfrow = c(2,2))

plot.ts(x1, ylim = c(-10, 10), main = paste0("omega: ", omega[1], "  A: ", round(A[1], 2)))
abline(h = c(A1, -A1), lty = 2, col = "gray")

plot.ts(x2, ylim = c(-10, 10), main = paste0("omega: ", omega[2], "  A: ", round(A[2], 2)))
abline(h = c(A2, -A2), lty = 2, col = "gray")

plot.ts(x3, ylim = c(-10, 10), main = paste0("omega: ", omega[3], "  A: ", round(A[3], 2)))
abline(h = c(A3, -A3), lty = 2, col = "gray")




# ----------
# A^2 / 2 = x's variance

A[1]^2 / 2

var(x1) * (n - 1) / n


A[2]^2 / 2

var(x2) * (n - 1) / n



A[3]^2 / 2

var(x3) * (n - 1) / n




# ----------
graphics.off()

par(mfcol = c(2,3))


# theoretical auto-correlation
# try when n = 100 and n = 1000
# when n = 100 (number of time points are small), the auto-correlation is decaying,
# since samples are to be smaller for calculation of auto-correlation


plot(cos(2 * pi * omega[1] * 1:100), type = "h")

acf(x1, lag = 100)


plot(cos(2 * pi * omega[2] * 1:100), type = "h")

acf(x2, lag = 100)


plot(cos(2 * pi * omega[3] * 1:100), type = "h")

acf(x3, lag = 100)




# ------------------------------------------------------------------------------
# Mixture of 3 periodic series
# ------------------------------------------------------------------------------


xall <- x1 + x2 + x3




# ----------
# plot x1, x2, x3 and xall

graphics.off()

par(mfrow = c(2,2))

plot.ts(x1, ylim = c(-10, 10), main = paste0("omega: ", omega[1], "  A: ", round(A[1], 2)))
abline(h = c(A1, -A1), lty = 2, col = "gray")

plot.ts(x2, ylim = c(-10, 10), main = paste0("omega: ", omega[2], "  A: ", round(A[2], 2)))
abline(h = c(A2, -A2), lty = 2, col = "gray")

plot.ts(x3, ylim = c(-10, 10), main = paste0("omega: ", omega[3], "  A: ", round(A[3], 2)))
abline(h = c(A3, -A3), lty = 2, col = "gray")

plot.ts(xall, ylim = c(-16, 16), main = "sum")




# ----------
# sum of A^2 / 2 = time series variance

sum(A^2 / 2)

var(xall) * (n - 1) / n





# ------------------------------------------------------------------------------
# only cosine:  coef (A = sqrt(a^2 + b^2)) and phi
# ------------------------------------------------------------------------------


# ( phi <- acos(a / A) )


# ( phi <- asin(b / A) )


( phi <- atan(b / a) )




# ----------
# only cosine but with A and phi (phase)

x1_2 <- A[1] * cos(2 * pi * 1:n * cyc[1] / n - phi[1])

x2_2 <- A[2] * cos(2 * pi * 1:n * cyc[2] / n - phi[2])

x3_2 <- A[3] * cos(2 * pi * 1:n * cyc[3] / n - phi[3])



# ----------
# combine all time series

xall_2 <- x1_2 + x2_2 + x3_2




# ----------
# plot x1_2, x2_2, x3_2 and xall_2  --> same time series with above

par(mfrow = c(2,2))

plot.ts(x1_2, ylim = c(-10, 10), main = paste0("freq: ", cyc[1], "/", n, "  A^2: ", A[1]^2))
abline(h = c(A[1], -A[1]), lty = 2, col = "gray")

plot.ts(x2_2, ylim = c(-10, 10), main = paste0("freq: ", cyc[2], "/", n, "  A^2: ", A[2]^2))
abline(h = c(A[2], -A[2]), lty = 2, col = "gray")

plot.ts(x3_2, ylim = c(-10, 10), main = paste0("freq: ", cyc[3], "/", n, "  A^2: ", A[3]^2))
abline(h = c(A[3], -A[3]), lty = 2, col = "gray")

plot.ts(xall_2, ylim = c(-16, 16), main = "sum")




# ------------------------------------------------------------------------------
# Fast Discrete Fourier Transform by fft
# ------------------------------------------------------------------------------

# Discrete Fourier Transform (DFT):
# d(omega(j)) = 1 / sqrt(n)  *  sum from t = 1 to n { x(t) * exp(- 2 * pi * i * omega(j) * t) }
# j = 0, 1 ..., n-1
# omega(j) = j / n  (Fourier or Functamental frequencies)


# R function fft() computes without "1 / sqrt(n)"



# ----------
# Discrete Fourier Transform (DFT)

dft1 <- fft(x1) / sqrt(n)

dft2 <- fft(x2) / sqrt(n)

dft3 <- fft(x3) / sqrt(n)

dftall <- fft(xall) / sqrt(n)




# ----------
# Periodogram = vector length ^ 2

Re(dft1)^2 + Im(dft1)^2

abs(dft1)^2

Mod(dft1)^2


P1 <- abs(dft1)^2

P2 <- abs(dft2)^2

P3 <- abs(dft3)^2

Pall <- abs(dftall)^2




# ----------
# plot raw periodogram
# you can see the x_all have all three components

omega


graphics.off()
par(mfcol = c(2,2))

Fr <- t/n

plot(Fr, P1, type = "h", xlab = "frequency", ylab = "periodogram")

plot(Fr, P2, type = "h", xlab = "frequency", ylab = "periodogram")

plot(Fr, P3, type = "h", xlab = "frequency", ylab = "periodogram")

plot(Fr, Pall, type = "h", xlab = "frequency", ylab = "periodogram")



# -->
# There is a mirrorring effect at the folding frequency of 0.5:  P(j/n) = P(1 - j/n)



# ----------
# average of Periodogram  =  time series variance

sum(abs(dft1)^2) / n
var(x1) * { (n - 1) / n }


sum(abs(dft2)^2) / n
var(x2) * { (n - 1) / n }


sum(abs(dft3)^2) / n 
var(x3) * { (n - 1) / n }


sum(abs(dftall)^2) / n
var(xall) * { (n - 1) / n }




# ----------
# sum of periodogram  =  time series energy (sum of time series ^ 2)

sum(x1^2)

sum(abs(dft1)^2)


sum(x2^2)

sum(abs(dft2)^2)


sum(x3^2)

sum(abs(dft3)^2)


sum(xall^2)

sum(abs(dftall)^2)




# ----------
# periodogram by astsa::mvspec

graphics.off()

par(mfrow = c(2,2))

x1.per <- astsa::mvspec(x1, log = "no", type = "h")

x2.per <- astsa::mvspec(x2, log = "no", type = "h")

x3.per <- astsa::mvspec(x3, log = "no", type = "h")

xall.per <- astsa::mvspec(xall, log = "no", type = "h")


x1.per$bandwidth



# -->
# bandwidth = 1 / n  (this is raw periodogram, no smoothing)


x1.per$details





# ------------------------------------------------------------------------------
# Scaled Periodgram
# ------------------------------------------------------------------------------


sP1 <- abs(dft1)^2 * 4 / n

sP2 <- abs(dft2)^2 * 4 / n

sP3 <- abs(dft3)^2 * 4 / n

sPall <- abs(dftall)^2 * 4 / n



# ----------
graphics.off()

par(mfrow = c(2,2))

Fr <- 0:(n-1)/n

plot(Fr, sP1, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sP2, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sP3, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sPall, type = "h", xlab = "frequency", ylab = "scaled periodogram")




# ----------
# sum of scaled periodogram = A^2 = time series variance * 4

sum(sP1)
A[1]^2 * 2
var(x1) * { (n - 1) / n } * 4


sum(sP2)
A[2]^2 * 2
var(x2) * { (n - 1) / n } * 4


sum(sP3)
A[3]^2 * 2
var(x3) * { (n - 1) / n } * 4


sum(sPall)
sum(A^2) * 2
var(xall) * { (n - 1) / n } * 4






# ------------------------------------------------------------------------------
# Inverse back to original time series
# ------------------------------------------------------------------------------

idft1 <- fft(dft1, inverse = TRUE) / sqrt(n)

idft2 <- fft(dft2, inverse = TRUE) / sqrt(n)

idft3 <- fft(dft3, inverse = TRUE) / sqrt(n)

idftall <- fft(dftall, inverse = TRUE) / sqrt(n)



# ----------
idft1



# -->
# note that imagenary part is zero-i



# ----------
graphics.off()

par(mfrow = c(2,2))

plot(Re(idft1), type = "l")

plot(Re(idft2), type = "l")

plot(Re(idft3), type = "l")

plot(Re(idftall), type = "l")





# ----------
# manual calculation

# time point 1 and frequency = 1/n
t <- 1
j <- 1

1 / sqrt(n) * dftall[j] %*% exp(2 * pi * 1i * (j-1) / n * t)



# ----
# time point 1 and sum of all j
t <- 1

1 / sqrt(n) * dftall %*% exp(2 * pi * 1i * 0:(n-1) / n * t)



# ---
xall_inverse <- sapply(1:n, function(t){ 1 / sqrt(n) * dftall %*% exp(2 * pi * 1i * 0:(n-1) / n * t) } )


par(mfrow = c(1,1))

plot(Re(xall_inverse), type = "l")





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


