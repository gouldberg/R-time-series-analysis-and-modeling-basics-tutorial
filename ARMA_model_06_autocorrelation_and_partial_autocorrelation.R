setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Autocorrelation and Partial Autocorrelation:  AR, MA, ARMA process
# ------------------------------------------------------------------------------

phi = 0.6

theta = 0.9


p <- 1;  d <- 0;  q <- 0;
ar_ts <- arima.sim(list(order = c(p, d, q), ar = phi), n = 500)


p <- 0;  d <- 0;  q <- 1;
ma_ts <- arima.sim(list(order = c(p, d, q), ma = theta), n = 500)


p <- 1;  d <- 0;  q <- 1;
arma_ts <- arima.sim(list(order = c(p, d, q), ar = phi, ma = theta), n = 500)




# ----------
graphics.off()
par(mfrow = c(3,1))

plot(ar_ts, type = "l")

plot(ma_ts, type = "l")

plot(arma_ts, type = "l")



# ----------
# compute ACF and PACF
astsa::acf2(ar_ts)

astsa::acf2(ma_ts)

astsa::acf2(arma_ts)



# -->
# AR(1):  ACF tails off + PACF cuts off after lag 1
# MA(1):  ACF cuts off after lag 1 + PACF tails lff
# ARMA(1):  ACF and PACF tails off



# ------------------------------------------------------------------------------
# Theoretical Autocorrelation and Partial Autocorrelation
# ------------------------------------------------------------------------------

# AR model

ACF <- ARMAacf(ar = c(1.5, -0.75), ma = 0, 24)[-1]

PACF <- ARMAacf(ar = c(1.5, -0.75), ma = 0, 24, pacf = TRUE)



# ----------
par(mfrow=c(1,2))

plot(ACF, type = "h", xlab = "lag", ylim = c(-0.8, 1))
abline(h = 0)

plot(PACF, type = "h", xlab = "lag", ylim = c(-0.8, 1))
abline(h = 0)



# -->
# Note that AR(2) can model the time series with cyclic ACF




# ----------
# ARMA model

ACF <- ARMAacf(ar = c(1.5, -0.75), ma = c(1.5, -0.75), 24)[-1]

PACF <- ARMAacf(ar = c(1.5, -0.75), ma = c(1.5, -0.75), 24, pacf = TRUE)



# ----------
par(mfrow=c(1,2))

plot(ACF, type = "h", xlab = "lag", ylim = c(-0.8, 1))
abline(h = 0)

plot(PACF, type = "h", xlab = "lag", ylim = c(-0.8, 1))
abline(h = 0)



