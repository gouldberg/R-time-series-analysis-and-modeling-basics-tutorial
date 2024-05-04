
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate AR time series
# ------------------------------------------------------------------------------


# all AR(1)

set.seed(1234)

x1 <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = 100)


x2 <- arima.sim(list(order = c(1,0,0), ar = 0.3), n = 100)


x3 <- arima.sim(list(order = c(1,0,0), ar = -0.3), n = 100)


x4 <- arima.sim(list(order = c(1,0,0), ar = -0.8), n = 100)


# x5 <- arima.sim(list(order = c(1,0,0), ar = 1), n = 100)


# x6 <- arima.sim(list(order = c(1,0,0), ar = 1.1), n = 100)



# ----------
ts_ar <- data.frame(x1, x2, x3, x4)



# ----------
graphics.off()

par(mfrow = c(1,1))

MTSplot(ts_ar)




# ----------
# all AR(2)

set.seed(1234)

x11 <- arima.sim(list(order = c(2,0,0), ar = c(0.8, -0.4)), n = 100)


x12 <- arima.sim(list(order = c(2,0,0), ar = c(0.3, -0.4)), n = 100)


x13 <- arima.sim(list(order = c(2,0,0), ar = c(-0.3, -0.4)), n = 100)


x14 <- arima.sim(list(order = c(2,0,0), ar = c(-0.8, -0.4)), n = 100)




# ----------
ts_ar2 <- data.frame(x11, x12, x13, x14)



# ----------
graphics.off()

par(mfrow = c(1,1))

MTSplot(ts_ar2)




# ------------------------------------------------------------------------------
# Simulate AR time series from white noize:  filtering
# ------------------------------------------------------------------------------

set.seed(12345)


ts1 <- rnorm(n = 200, mean = 0, sd = 1)



# filter white noise by AR coefficient --> AR series
x16 <- stats::filter(ts1, filter = c(0.8, -0.4), method = "recursive", sides = 1)



# filter white noize by MA coefficient, equivalent to AR series  --> AR series
ARMAtoMA(ar = c(0.8, -0.4), lag = 10)

x16_2 <- stats::filter(ts1, filter = ARMAtoMA(ar = c(0.8, -0.4), lag = 10), method = "convolution", sides = 1)



# inversely filter AR series by AR coefficient --> white noize
x16_3 <- stats::filter(x16, filter = c(1, -0.8, 0.4), sides = 1)




# ----------
graphics.off()

par(mfrow = c(2,2))

plot(ts1, type = "l", lty = 2, lwd = 1, 
     main = "white noise and filtered AR(2) series", cex.main = 1.5, ylim = c(-4, 4))

lines(x16, lty = 1, lwd = 2, col = "blue")

plot(x16, type = "l", lty = 1, lwd = 1, col = "blue",
     main = "AR(2) and AR(2) constructed from MA", cex.main = 1.5, ylim = c(-4, 4))

lines(x16_2, lty = 1, lwd = 2, col = "red")


plot(ts1, type = "l", lty = 2, lwd = 1, 
     main = "original white noise and inversely filtered AR series", cex.main = 1.5, ylim = c(-4, 4))

lines(x16_3, lty = 1, lwd = 2, col = "red")





# ------------------------------------------------------------------------------
# Auto-correlation and partial auto-correlation
# ------------------------------------------------------------------------------


acf2(x1, max.lag = 40)


acf2(x2, max.lag = 40)


acf2(x3, max.lag = 40)


acf2(x4, max.lag = 40)




# ----------

acf2(x11, max.lag = 40)


acf2(x12, max.lag = 40)


acf2(x13, max.lag = 40)


acf2(x14, max.lag = 40)





# ------------------------------------------------------------------------------
# Theoretical spectral density: AR variation
# ------------------------------------------------------------------------------

# Note that sigma_w^2 = 1 is assumed  (variance of noise is 1)


# change AR coefficient increasing from negative to positive
graphics.off()

par(mfrow = c(2,3))

arma.spec(ar = c(-0.8), log = "no", main = "AR(-0.8)")

arma.spec(ar = c(-0.5), log = "no", main = "AR(-0.5)")

arma.spec(ar = c(-0.3), log = "no", main = "AR(-0.3)")

arma.spec(ar = c(0.3), log = "no", main = "AR(0.3)")

arma.spec(ar = c(0.5), log = "no", main = "AR(0.5)")

arma.spec(ar = c(0.8), log = "no", main = "AR(0.8)")




# ----------
# change second-order coefficients
graphics.off()

par(mfrow = c(2,2))

arma.spec(ar = c(0.5), log = "no", main = "AR(0.5)")

arma.spec(ar = c(0.5, 0.1), log = "no", main = "AR(0.5, 0.1)")

arma.spec(ar = c(0.5, 0.2), log = "no", main = "AR(0.5, 0.2)")

arma.spec(ar = c(0.5, 0.4), log = "no", main = "AR(0.5, 0.4)")




# ----------
# this pattern concentrate power in narrow band of frequency
graphics.off()

par(mfrow = c(2,1))

arma.spec(ar = c(0.5, -0.8), log = "no", main = "AR(0.5, -0.8)")

xobj <- arima.sim(list(order = c(2,0,0), ar = c(0.5, -0.8)), n = 100)
plot(xobj, type = "l")





# ----------
# adding orders

graphics.off()

par(mfrow = c(2,2))

arma.spec(ar = c(0.2), log = "no", main = "AR(0.2)")

arma.spec(ar = c(0.2, -0.1), log = "no", main = "AR(0.2, -0.1)")

arma.spec(ar = c(0.2, -0.1, 0.3), log = "no", main = "AR(0.2, -0.1, 0.3)")


# this pattern concentrate power in narrow band of frequency
arma.spec(ar = c(0.2, -0.1, 0.3, -0.4), log = "no", main = "AR(0.2, -0.1, 0.3, -0.4)")




# ------------------------------------------------------------------------------
# Periodogram and smoothed periodgram
# ------------------------------------------------------------------------------

# more smoothing by spans
# spans:  vector of odd integers, given in terms of L = 2 * m + 1 instead of m

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(x11, log = "no", type = "h")


astsa::mvspec(x12, log = "no", type = "h")


astsa::mvspec(x13, log = "no", type = "h")


astsa::mvspec(x14, log = "no", type = "h")




# ----------
graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(x13, log = "no", type = "h")

astsa::mvspec(x13, log = "no", type = "h", spans = c(3,3))

astsa::mvspec(x13, log = "no", type = "h", spans = c(5,5))

astsa::mvspec(x13, log = "no", type = "h", spans = c(5,5), taper = 0.5)


