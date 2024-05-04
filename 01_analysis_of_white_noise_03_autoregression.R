
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# auto-regressive time series calculated from white noise (ts2)
# ------------------------------------------------------------------------------


ar <- ts2

phi <- c(0.5, -0.9)


for(i in 3:length(ts1)){
  
  # AR(2)
  ar[i] <- ts2[i] + phi[1] * ar[i-1] + phi[2] * ar[i-2] + ts1[i]
}



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(ts2, type = "l", lty = 1, lwd = 2, main = "ts2", cex.main = 2)

plot(ar, type = "l", lty = 1, lwd = 2, col = "blue", main = "autoregressive series", cex.main = 2)





# ------------------------------------------------------------------------------
# autoregression by "filter"
# ------------------------------------------------------------------------------


ar2 <- stats::filter(ts2, filter = c(phi[1], phi[2]), method = "recursive")




# ----------
graphics.off()

par(mfrow = c(2,1))

plot(ar, type = "l", lty = 1, lwd = 2, main = "AR series", cex.main = 2)

plot(ar2, type = "l", lty = 1, lwd = 2, col = "blue", main = "AR series by filter", cex.main = 2)





# ------------------------------------------------------------------------------
# Auto-correlation and partial autocorrelation
# ------------------------------------------------------------------------------


astsa::acf2(ar)





# ------------------------------------------------------------------------------
# Raw Periodogram
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,1))

astsa::mvspec(ar, log = "no", type = "h")




# ------------------------------------------------------------------------------
# Periodogram with more and more smoothing
# ------------------------------------------------------------------------------

# more smoothing by spans
# spans:  vector of odd integers, given in terms of L = 2 * m + 1 instead of m

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(ar, log = "no", type = "h")
astsa::mvspec(ar, log = "no", type = "h", spans = c(10,10), ylim = c(0,20000))
astsa::mvspec(ar, log = "no", type = "h", spans = c(10,10), ylim = c(0,20000))
astsa::mvspec(ar, log = "no", type = "h", spans = c(10,10), ylim = c(0,20000), taper = 0.25)





# ------------------------------------------------------------------------------
# Parametric Spectral Estimation
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,2))


astsa::mvspec(ar, log = "no", type = "h")

spec.ar(ar)




# ------------------------------------------------------------------------------
# Theoretical spectral density
# ------------------------------------------------------------------------------

# Note that sigma_w^2 = 1 is assumed  (variance of noise is 1)


par(mfrow = c(1,1))

arma.spec(ar = c(phi[1], phi[2]), log = "no", main = "autoregression")




# ------------------------------------------------------------------------------
# Theoretical spectral density: AR variation
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,2))

arma.spec(ar = c(0.9), log = "no", main = "AR(0.9)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.9), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.9), log = "no", main = "AR(-0.9)")
x <- arima.sim(list(order = c(1,0,0), ar = -0.9), n = 100)
plot(x, type = "l")




# ----------
# change AR coefficient increasing from negative to positive
graphics.off()

par(mfcol = c(4,3))

arma.spec(ar = c(-0.8), log = "no", main = "AR(-0.8)")
x <- arima.sim(list(order = c(1,0,0), ar = -0.8), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.5), log = "no", main = "AR(-0.5)")
x <- arima.sim(list(order = c(1,0,0), ar = -0.5), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.3), log = "no", main = "AR(-0.3)")
x <- arima.sim(list(order = c(1,0,0), ar = -0.3), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.3), log = "no", main = "AR(0.3)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.3), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.5), log = "no", main = "AR(0.5)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.8), log = "no", main = "AR(0.8)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = 100)
plot(x, type = "l")




# ----------
# change second-order coefficients
graphics.off()

par(mfcol = c(4,2))

arma.spec(ar = c(0.5), log = "no", main = "AR(0.5)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.5, 0.2), log = "no", main = "AR(0.5, 0.2)")
x <- arima.sim(list(order = c(2,0,0), ar = c(0.5, 0.2)), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.5, 0.4), log = "no", main = "AR(0.5, 0.4)")
x <- arima.sim(list(order = c(2,0,0), ar = c(0.5, 0.4)), n = 100)
plot(x, type = "l")



# ----------
# this pattern concentrate power in narrow band of frequency
arma.spec(ar = c(0.5, -0.8), log = "no", main = "AR(0.5, -0.8)")
xobj <- arima.sim(list(order = c(2,0,0), ar = c(0.5, -0.8)), n = 100)
plot(xobj, type = "l")



# AR(2) process have cyclic autocorrelation
astsa::acf2(xobj, max.lag = 80)





# ----------
# adding orders

graphics.off()

par(mfcol = c(4,2))

arma.spec(ar = c(0.2), log = "no", main = "AR(0.2)")
x <- arima.sim(list(order = c(1,0,0), ar = 0.2), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.2, -0.1), log = "no", main = "AR(0.2, -0.1)")
x <- arima.sim(list(order = c(2,0,0), ar = c(0.2, -0.1)), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.2, -0.1, 0.3), log = "no", main = "AR(0.2, -0.1, 0.3)")
x <- arima.sim(list(order = c(3,0,0), ar = c(0.2, -0.1, 0.3)), n = 100)
plot(x, type = "l")


# this pattern concentrate power in narrow band of frequency
arma.spec(ar = c(0.2, -0.1, 0.3, -0.4), log = "no", main = "AR(0.2, -0.1, 0.3, -0.4)")
x <- arima.sim(list(order = c(4,0,0), ar = c(0.2, -0.1, 0.3, -0.4)), n = 100)
plot(x, type = "l")


