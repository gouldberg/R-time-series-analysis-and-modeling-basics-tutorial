
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# signal in noise in sinusoidal waveform
#   - Sinusoidal waveform:  A * cos(2 * pi * omega * t + phi)
#     A:  amplitude    omega:  frequency of oscillation    phi:  phase shift
# ------------------------------------------------------------------------------


# A = 2,  omega = 1/50 (1 cycle every 50 time points),  phi = 2 * pi * 15 / 50 = 0.6 * pi

cs <- 2 * cos(2 * pi * 1:500 / 50 + 0.6 * pi)



# noise
w <- rnorm(500, 0, 1)



# ----------
graphics.off()

par(mfcol = c(3,2))

plot(cs, type = "l", lty = 1, lwd = 1, main = expression(2 * cos(2 * pi * t/50 + 0.6 * pi)), cex.main = 2)

plot(cs + w, type = "l", lty = 1, lwd = 1, main = expression(2 * cos(2 * pi * t/50 + 0.6 * pi) + N(0,1)), cex.main = 2)

plot(cs + 5 * w, type = "l", lty = 1, lwd = 1, main = expression(2 * cos(2 * pi * t/50 + 0.6 * pi) + N(0,25)), cex.main = 2)


astsa::mvspec(cs, log = "no", lwd = 1, type = "h")

astsa::mvspec(cs+w, log = "no", lwd = 1, type = "h")

astsa::mvspec(cs+5*w, log = "no", lwd = 1, type = "h")




# ------------------------------------------------------------------------------
# extract signals from noisy periodic data by SigExtract  (frequency domain method)
# ------------------------------------------------------------------------------


csw <- cs + w

cs5w <- cs + 5 * w




# ----------
graphics.off()

astsa::SigExtract(csw)




astsa::SigExtract(cs5w)




# ------------------------------------------------------------------------------
# extract signals from noisy periodic data by time domain method
#  - smooth.spline  --> better
#  - low-pass fileter by moving average
# ------------------------------------------------------------------------------


par(mfrow=c(2,1))


# smooth.spline:
# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

plot(cs5w, type = "l", main = "cs + 5w and smooth.spline", col = "gray")

lines(smooth.spline(time(cs5w), cs5w, spar = 0.25), lwd = 2, col = "blue")

lines(smooth.spline(time(cs5w), cs5w, spar = 0.5), lwd = 2, col = "red")




# moving average low-pass filter
m <- 10

k <- kernel("modified.daniell", m)

cs5wf <- kernapply(cs5w, k)


# need add NA at start and end
cs5wf2 <- c(rep(NA, m), cs5wf, rep(NA, m))

plot(cs5w, type = "l", main = "cs + 5w and low-pass filter by moving average", col = "gray")

lines(cs5wf2, lwd = 2, col = "red")




