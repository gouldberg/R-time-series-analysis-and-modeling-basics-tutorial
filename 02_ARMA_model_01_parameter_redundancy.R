packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Generate white noise data
# ------------------------------------------------------------------------------


# this seed is required
set.seed(8675309)


w <- rnorm(n = 150, mean = 5, sd = 1)



# ----------
par(mfrow = c(1,1))

plot(w, type = "l")




# ----------
astsa::acf2(w)




# ----------
par(mfrow = c(1,1))

astsa::mvspec(w, log = "no")




# ------------------------------------------------------------------------------
# Fit ARMA(1,1) model to white noise
# ------------------------------------------------------------------------------


# mod <- arima(w, order = c(1, 0, 1))
# summary(mod)

sarima(w, p = 1, d = 0, q = 1)




# -->
# We fit arima(1,0,1) model to white noise data and find that the parameter estimates are significant ....

# We note that almost:  coeffs of ar1  =  minus of coeffs of ma1
# --> ARMA(p, q) model can be written as:  phi(B) * x(t) = theta(B) * w(t)

# Here:  (1 + 0.96B) * x(t) = (1 + 0.95B) * w(t)

# phi(B) and theta(B) has almost common factor.


# -->  x(t) = w(t)




# ------------------------------------------------------------------------------
# Parameter redundancy:
#   - Estimate of coefficients are very different for each time series
# ------------------------------------------------------------------------------


# Generate n = 500 observations from the ARMA(1,0,1) 3 times

p <- 1;  d <- 0;  q <- 1;

phi = 0.9

theta = -0.9


ts1 <- arima.sim(list(order = c(p, d, q), ar = phi, ma = theta), n = 500)

ts2 <- arima.sim(list(order = c(p, d, q), ar = phi, ma = theta), n = 500)

ts3 <- arima.sim(list(order = c(p, d, q), ar = phi, ma = theta), n = 500)




# ----------
graphics.off()
par(mfrow = c(3,1))

plot(ts1, type = "l")

plot(ts2, type = "l")

plot(ts3, type = "l")




# ----------
astsa::acf2(ts1)

astsa::acf2(ts2)

astsa::acf2(ts3)



# -->
# ACF and PACF is very similar for each time series
# No suggestions for ARMA model




# ----------
# fit ARM(1,1) model

summary(arima(ts1, order = c(1, 0, 1)))

summary(arima(ts2, order = c(1, 0, 1)))

summary(arima(ts3, order = c(1, 0, 1)))



# -->
# Note that coefficients of ar1 and ma1 is very different in each model ...
# We note that almost:  coeffs of ar1  =  minus of coeffs of ma1
# --> ARMA(p, q) model can be written as:  phi(B) * x(t) = theta(B) * w(t)




# ------------------------------------------------------------------------------
# Theoretical spectral density: ARMA variation
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,4))

arma.spec(ar = c(0.7), ma = c(-0.2), log = "no", main = "ARMA(0.7, -0.2)")
x <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma = -0.2), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.7), ma = c(-0.4), log = "no", main = "ARMA(0.7, -0.4)")
x <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma = -0.4), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.7), ma = c(-0.699), log = "no", main = "ARMA(0.7, -0.699)")
x <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma = -0.699), n = 100)
plot(x, type = "l")

arma.spec(ar = c(0.7), ma = c(-0.7), log = "no", main = "ARMA(0.7, -0.7)")
x <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma = -0.7), n = 100)
plot(x, type = "l")




# ----------
graphics.off()

par(mfcol = c(2,4))

arma.spec(ar = c(-0.7), ma = c(0.2), log = "no", main = "ARMA(-0.7, 0.4)")
x <- arima.sim(list(order = c(1,0,1), ar = -0.7, ma = 0.2), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.7), ma = c(0.4), log = "no", main = "ARMA(-0.7, 0.4)")
x <- arima.sim(list(order = c(1,0,1), ar = -0.7, ma = 0.4), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.7), ma = c(0.699), log = "no", main = "ARMA(-0.7, 0.699)")
x <- arima.sim(list(order = c(1,0,1), ar = -0.7, ma = 0.699), n = 100)
plot(x, type = "l")

arma.spec(ar = c(-0.7), ma = c(0.7), log = "no", main = "ARMA(-0.7, 0.7)")
x <- arima.sim(list(order = c(1,0,1), ar = -0.7, ma = 0.7), n = 100)
plot(x, type = "l")



