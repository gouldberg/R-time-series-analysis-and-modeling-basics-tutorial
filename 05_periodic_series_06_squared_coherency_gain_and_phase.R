
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# set parameters
# ------------------------------------------------------------------------------


# omega:  cycels per unit time
omega <- c(0.06, 0.1, 0.4)


# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)


# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)




# ------------------------------------------------------------------------------
# n = 100, without noise
# ------------------------------------------------------------------------------

n <- 100

t <- 1:n


x1 <- a[1] * cos(2 * pi * omega[1] * t)

x2 <- a[2] * cos(2 * pi * omega[2] * t)

x3 <- a[3] * cos(2 * pi * omega[3] * t)

xall <- x1 + x2 + x3



# ----------
( ker <- kernel("modified.daniell", c(1,1)) )

# ( ker <- kernel("modified.daniell", c(7,7)) )

plot(ker)



# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(x3, log = "no", type = "h")
mvspec(x3, log = "no", type = "h", kernel = ker)

mvspec(xall, log = "no", type = "h")
mvspec(xall, log = "no", type = "h", kernel = ker)

mvspec(cbind(x3, xall), plot.type = "coh", kernel = ker)




# ------------------------------------------------------------------------------
# n = 128, without noise
# ------------------------------------------------------------------------------

n <- 128

t <- 1:n


x1 <- a[1] * cos(2 * pi * omega[1] * t)

x2 <- a[2] * cos(2 * pi * omega[2] * t)

x3 <- a[3] * cos(2 * pi * omega[3] * t)

xall <- x1 + x2 + x3



# ----------
( ker <- kernel("modified.daniell", c(1,1)) )

# ( ker <- kernel("modified.daniell", c(7,7)) )

plot(ker)



# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(x3, log = "no", type = "h")
mvspec(x3, log = "no", type = "h", kernel = ker)

mvspec(xall, log = "no", type = "h")
mvspec(xall, log = "no", type = "h", kernel = ker)

mvspec(cbind(x3, xall), plot.type = "coh", kernel = ker)






# ------------------------------------------------------------------------------
# n = 128, with noise
# ------------------------------------------------------------------------------

n <- 128

t <- 1:n


x1 <- a[1] * cos(2 * pi * omega[1] * t) + rnorm(n = n, mean = 0, sd = 1)

x2 <- a[2] * cos(2 * pi * omega[2] * t) + rnorm(n = n, mean = 0, sd = 1)

x3 <- a[3] * cos(2 * pi * omega[3] * t) + rnorm(n = n, mean = 0, sd = 1)

xall <- x1 + x2 + x3 + rnorm(n = n, mean = 0, sd = 1)



# ----------
( ker <- kernel("modified.daniell", c(1,1)) )

# ( ker <- kernel("modified.daniell", c(7,7)) )

plot(ker)


# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(x3, log = "no", type = "h")
mvspec(x3, log = "no", type = "h", kernel = ker)

mvspec(xall, log = "no", type = "h")
mvspec(xall, log = "no", type = "h", kernel = ker)

mvspec(cbind(x3, xall), plot.type = "coh", kernel = ker)




# ------------------------------------------------------------------------------
# n = 1280, with noise
# ------------------------------------------------------------------------------

n <- 1280

t <- 1:n


x1 <- a[1] * cos(2 * pi * omega[1] * t) + rnorm(n = n, mean = 0, sd = 1)

x2 <- a[2] * cos(2 * pi * omega[2] * t) + rnorm(n = n, mean = 0, sd = 1)

x3 <- a[3] * cos(2 * pi * omega[3] * t) + rnorm(n = n, mean = 0, sd = 1)

xall <- x1 + x2 + x3 + rnorm(n = n, mean = 0, sd = 1)



# ----------
# ( ker <- kernel("modified.daniell", c(1,1)) )

( ker <- kernel("modified.daniell", c(7,7)) )

plot(ker)



# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(x3, log = "no", type = "h")
mvspec(x3, log = "no", type = "h", kernel = ker)

mvspec(xall, log = "no", type = "h")
mvspec(xall, log = "no", type = "h", kernel = ker)

mvspec(cbind(x3, xall), plot.type = "coh", kernel = ker)




# ------------------------------------------------------------------------------
# Cross-spectrum, Squared Coherency  (and Gain, Phase)
# ------------------------------------------------------------------------------


n <- 1000

t <- 1:n


# change the noise level  (sd = 1, 2, 2.5)

set.seed(12345)

x1 <- a[1] * cos(2 * pi * omega[1] * t) + rnorm(n = n, mean = 0, sd = 1)

x2 <- a[2] * cos(2 * pi * omega[2] * t) + rnorm(n = n, mean = 0, sd = 2)

x3 <- a[3] * cos(2 * pi * omega[3] * t) + rnorm(n = n, mean = 0, sd = 2.5)

xall <- x1 + x2 + x3 + rnorm(n = n, mean = 0, sd = 1)


( ker <- kernel("modified.daniell", c(7,7)) )

mvspec(x3, log = "no", type = "h")
mvspec(x3, log = "no", type = "h", kernel = ker)

mvspec(xall, log = "no", type = "h")
mvspec(xall, log = "no", type = "h", kernel = ker)




# ----------
# x3 and xall

sr <- mvspec(cbind(x3, xall), kernel = ker, plot = FALSE)


names(sr)


dim(sr$fxx)



# ----------
sr$fxx[,,1]



# ----------
fxx <- sr$fxx[1,1,]

fyy <- sr$fxx[2,2,]



# ----------
# cross-spectrum
fyx <- sr$fxx[1,2,]



# frequency response
Ayx <- fyx / fxx



# cospectrum and quadspectrum
cyx <- Re(fyx)

qyx <- - Im(fyx)



# ----------
# Amplitude and Gain
Ayx_norm <- abs(Ayx)


Gain <- 20 * log10(abs(Ayx)^2)




# ----------
# phase
Phiyx <- atan(- qyx / cyx)  / pi * 180



# ----------
# squared coherence
rho2yx <- Re(abs(fyx)^2 / (fxx * fyy))




# ----------
# C:  approximate value that must be exceeded for the original squared coherency
# to be able to reject squared coherency = 0

f <- qf(0.999, 2, sr$df - 2)

L <- 2 * 7 + 1

C <- f / (L - 1 + f)



# ----------
graphics.off()

par(mfrow = c(2,2))


plot(Gain ~ sr$freq, ylim = c(-20, 30),
     type = "l", lty = 1, lwd = 2,
     xlab = "frequency", ylab = "Gain", main = "Frequency Response: Gain (db)")

abline(h = 0, lty = 2, col = "blue")

plot(Phiyx ~ sr$freq, ylim = c(-100, 100),
     type = "l", lty = 1, lwd = 2,
     xlab = "frequency", ylab = "Phase", main = "Frequency Response: Phase (degree)")

abline(h = 0, lty = 2, col = "blue")

plot(rho2yx ~ sr$freq, ylim = c(0, 1),
     type = "l", lty = 1, lwd = 2,
     xlab = "frequency", ylab = "Squared Coherency", main = "Squared Coherency")
abline(h = C, lty = 2, col = "blue")


plot(sr, plot.type = "coh")
abline(h = C, lty = 2, col = "blue")




# ------------------------------------------------------------------------------
# Gain and Phase
# ------------------------------------------------------------------------------


library(sysid)


# convert to idframe
ze <- idframe(output = ts(xall), input = ts(x3), Ts = 1)



str(ze)



# ----------
plot(ze)



# ----------
# Estimate Frequency Response

# default lag size of the Hanning window (Default: min (length(x)/10,30))
# frequency points at which the response is evaluated (Default: seq(1,128)/128*pi/Ts --> up to 3.14)

sp <- spa(ze)


# bode plot
#  - Y: amplitude in decibel, phase in degree
#  - X: rad / unit time, 1 means 1 rad/unit time

plot(sp)

