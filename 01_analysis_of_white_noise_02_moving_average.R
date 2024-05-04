
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# moving average calculated from white noise
# ------------------------------------------------------------------------------


ma <- data.frame(ts.intersect(lead0 = stats::lag(ts1, 0), lead1 = stats::lag(ts1, -1), lead2 = stats::lag(ts1, -2)))


head(ts1)

head(ma)



# moving average
theta <- c(0.6, 0.3)

ma$ma <- 1 * ma$lead0 + theta[1] * ma$lead1 + theta[2] * ma$lead2


nrow(ma)



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(ts1[3:500], type = "l", lty = 1, lwd = 2, main = "ts1", cex.main = 2)

plot(ma$ma, type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-3, 3), main = "moving average of ts1", cex.main = 2)




# ------------------------------------------------------------------------------
# Autocorrelation and partial-autocorrelation
# ------------------------------------------------------------------------------


astsa::acf2(ma$ma)



sarima(ma$ma, p = 2, d = 0, q = 2, no.constant = TRUE)


sarima(ma$ma, p = 0, d = 0, q = 2, no.constant = TRUE)




# ------------------------------------------------------------------------------
# Raw periodogram
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

astsa::mvspec(ma$ma, log = "no", type = "h")




# ------------------------------------------------------------------------------
# Periodogram with more and more smoothing
# ------------------------------------------------------------------------------

# more smoothing by spans
# spans:  vector of odd integers, given in terms of L = 2 * m + 1 instead of m

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(ma$ma, log = "no", type = "h")
astsa::mvspec(ma$ma, log = "no", type = "h", spans = c(10,10), ylim = c(0,4))
astsa::mvspec(ma$ma, log = "no", type = "h", spans = c(30,30), ylim = c(0,4))
astsa::mvspec(ma$ma, log = "no", type = "h", spans = c(50,50), ylim = c(0,4), taper = 0.25)




# ------------------------------------------------------------------------------
# Parametric Spectral Estimation
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,2))


astsa::mvspec(ma$ma, log = "no", type = "h")

spec.ar(ma$ma)




# ------------------------------------------------------------------------------
# Theoretical spectral density
# ------------------------------------------------------------------------------

# Note that sigma_w^2 = 1 is assumed  (variance of noise is 1)

par(mfrow = c(1,1))

arma.spec(ma = c(theta[1], theta[2]), log = "no", main = "moving average")




# ------------------------------------------------------------------------------
# Theoretical spectral density: MA variation
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,2))

arma.spec(ma = c(0.9), log = "no", main = "MA(0.9)")
x <- arima.sim(list(order = c(0,0,1), ma = 0.9), n = 100)
plot(x, type = "l")

arma.spec(ma = c(-0.9), log = "no", main = "MA(-0.9)")
x <- arima.sim(list(order = c(0,0,1), ma = -0.9), n = 100)
plot(x, type = "l")



# ----------
# change MA coefficient increasing from negative to positive
# The series are getting smoother
graphics.off()

par(mfcol = c(4,3))

arma.spec(ma = c(-0.8), log = "no", main = "MA(-0.8)")
x <- arima.sim(list(order = c(0,0,1), ma = -0.8), n = 100)
plot(x, type = "l")

arma.spec(ma = c(-0.5), log = "no", main = "MA(-0.5)")
x <- arima.sim(list(order = c(0,0,1), ma = -0.5), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.3), log = "no", main = "MA(-0.3)")
x <- arima.sim(list(order = c(0,0,1), ma = -0.3), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.3), log = "no", main = "MA(0.3)")
x <- arima.sim(list(order = c(0,0,1), ma = 0.3), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5), log = "no", main = "MA(0.5)")
x <- arima.sim(list(order = c(0,0,1), ma = 0.5), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.8), log = "no", main = "MA(0.8)")
x <- arima.sim(list(order = c(0,0,1), ma = 0.8), n = 100)
plot(x, type = "l")



# ----------
# Add second-order and change second-order coefficients

# ma = c(0.5, -0.5) is not invertible: there are abs(root) > 1
abs(polyroot(c(1,0.5,-0.5)))


graphics.off()

par(mfcol = c(4,3))

arma.spec(ma = c(0.5, -0.4), log = "no", main = "MA(0.5, -0.4)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, -0.4)), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5, -0.2), log = "no", main = "MA(0.5, -0.2)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, -0.2)), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5, -0.1), log = "no", main = "MA(0.5, -0.1)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, -0.1)), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5, 0.1), log = "no", main = "MA(0.5, 0.1)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, 0.1)), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5, 0.4), log = "no", main = "MA(0.5, 0.4)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, 0.4)), n = 100)
plot(x, type = "l")

arma.spec(ma = c(0.5, 0.8), log = "no", main = "MA(0.5, 0.8)")
x <- arima.sim(list(order = c(0,0,2), ma = c(0.5, 0.8)), n = 100)
plot(x, type = "l")



