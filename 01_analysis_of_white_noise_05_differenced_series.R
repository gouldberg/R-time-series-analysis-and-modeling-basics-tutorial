
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# white noise
# ------------------------------------------------------------------------------

n <- 500

mu <- 0

sd <- 1


wt <- rnorm(n = n, mean = mu, sd = sd)



# ----------
# its difference

wt_d <- diff(wt)



# ------------------------------------------------------------------------------
# random walk
# ------------------------------------------------------------------------------


rw <- cumsum(rnorm(n = n, mean = mu, sd = sd))



# ----------
# its difference

rw_d <- diff(rw)



# ------------------------------------------------------------------------------
# deterministic trend
# ------------------------------------------------------------------------------


phi <- c(0.5, 0.3)

p <- length(phi)

theta <- c(0.3, 0.2)

q <- length(theta)


pdq <- list(order = c(p, 0, q), ar = phi, ma = theta)


arma_noise <- arima.sim(n, model = pdq, sd = sd)


dt <- mu + drift * 1:n + arma_noise



# ----------
# its difference

dt_d <- diff(dt)




# ------------------------------------------------------------------------------
# Simulate periodic series
# ------------------------------------------------------------------------------


# cycles per unit time (here: t)
cyc <- c(n * 0.06, n * 0.1, n * 0.4)


# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)


# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)


# ----------
# generate time series
# different frequency and amplitude

x1 <- a[1] * cos(2 * pi * 1:n * cyc[1] / n) + b[1] * sin(2 * pi * 1:n * cyc[1] / n)

x2 <- a[2] * cos(2 * pi * 1:n * cyc[2] / n) + b[2] * sin(2 * pi * 1:n * cyc[2] / n)

x3 <- a[3] * cos(2 * pi * 1:n * cyc[3] / n) + b[3] * sin(2 * pi * 1:n * cyc[3] / n)



# ----------
# mixtures of periodic series with multiple frequencies and amplitudes

xall <- x1 + x2 + x3



# ----------
# its difference

xall_d <- diff(xall)




# ------------------------------------------------------------------------------
# plot time series and its original
# ------------------------------------------------------------------------------


graphics.off()


par(mfcol = c(4,2))


plot(wt, type = "l")

plot(wt_d, type = "l")

plot(rw, type = "l")

plot(rw_d, type = "l")

plot(dt, type = "l")

plot(dt_d, type = "l")

plot(xall, type = "l")

plot(xall_d, type = "l")




# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(4,2))

mvspec(wt, log = "no")

mvspec(wt_d, log = "no")

mvspec(rw, log = "no")

mvspec(rw_d, log = "no")

mvspec(dt, log = "no")

mvspec(dt_d, log = "no")

mvspec(xall, log = "no")

mvspec(xall_d, log = "no")



# -->
# Note that in raw periodogram of differenced series, the part of high-frequencies are remained.

# white noise series (wt) are over-differenced.


