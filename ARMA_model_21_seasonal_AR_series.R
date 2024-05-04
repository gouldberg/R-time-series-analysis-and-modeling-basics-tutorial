setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Seasonal AR Series
# ------------------------------------------------------------------------------

set.seed(666)


# First-order seasonal autoregressive series that might run over months
# This model exhibits the series x(t) in terms of past lags at the multiple of the yearly seasonal period s = 12 months.
phi <- c(rep(0, 11), 0.9)


# x(t) = 0.9 * x(t-12) + w(t)
sAR <- arima.sim(list(order = c(12, 0, 0), ar = phi), n = 37)

( sAR <- ts(sAR, freq = 12) )



# ----------
# Theoretical ACF and PACF
ACF <- ARMAacf(ar = phi, ma = 0, 100)

PACF <- ARMAacf(ar = phi, ma = 0, 100, pacf = TRUE)



# ----------
layout(matrix(c(1,1,2,1,1,3), nc = 2))
par(mar = c(3,3,2,1), mgp = c(1.6, 0.6, 0))

plot(sAR, axes = FALSE, main = "seasonal AR(1)", xlab = "year", type = "c")

Months <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

points(sAR, pch = Months, cex = 1.25, font = 4, col = 1:4)

axis(1, 1:4)
abline(v = 1:4, lty = 2, col = gray(0.7))
axis(2)
box()



# ----------
plot(ACF, type = "h", xlab = "LAG", ylim = c(-0.1, 1))
abline(h = 0)

plot(PACF, type = "h", xlab = "LAG", ylim = c(-0.1, 1))
abline(h = 0)
