setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Mixed Seasonal ARMA Series
# ------------------------------------------------------------------------------

set.seed(666)


phi <- c(rep(0, 11), 0.8)
theta <- -0.5

# x(t) = 0.8 * x(t-12) + w(t) - 0.5 * w(t-1)
sAR <- arima.sim(list(order = c(12, 0, 0), ar = phi, ma = theta), n = 37)

( sAR <- ts(sAR, freq = 12) )



# ----------
# Theoretical ACF and PACF
ACF <- ARMAacf(ar = phi, ma = theta, 100)

PACF <- ARMAacf(ar = phi, ma = theta, 100, pacf = TRUE)



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
