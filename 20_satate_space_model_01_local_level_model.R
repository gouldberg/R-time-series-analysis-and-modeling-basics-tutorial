
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate random walk data
# ------------------------------------------------------------------------------


set.seed(1)

num <- 50

w <- rnorm(num + 1, mean = 0, sd = 1)

v <- rnorm(num, mean = 0, sd = 1)



# ----------
# state: mu[0], mu[1], ..., mu[50]
mu <- cumsum(w)



# ----------
# observation
y <- mu[-1] + v




# ----------
graphics.off()


par(mfrow = c(1,1))

plot(mu, type = "o")

lines(time(y), y, col = "blue", lwd = 2)





# ------------------------------------------------------------------------------
# Filtering and Smoothing
# ------------------------------------------------------------------------------

# filter and smooth  (Ksmooth0 does both)
# Ksmooth0 calls Kfilter0 for the filtering part.
# These scripts use a Cholesky-type decomposition of Q and R;  they are denoted by cQ and cR


# A: time-invariant observation matrix
# mu0: initial state mean vector
# Sigma0:  initial state covariance matrix
# Phi:  state transition matrix
# cQ:  Cholesky-type decomposition of state error covariance matrix Q
# cR:  Cholesky-type decomposition of observation error covariance matrix R
ks <- astsa::Ksmooth0(num, y, A = 1, mu0 = 0, Sigma0 = 1, Phi = 1, cQ = 1, cR = 1)



# letters p, f, s denote prediction, filter, and smooth, respectively

names(ks)




# ----------
par(mfrow = c(3, 1))

Time <- 1:num


plot(Time, mu[-1], main = "Precdict", ylim = c(-5, 10))
lines(Time, y, col = "black")
lines(ks$xp, col = "blue", lwd = 2)
lines(ks$xp + 2 * sqrt(ks$Pp), lty = 2, col = 4)
lines(ks$xp - 2 * sqrt(ks$Pp), lty = 2, col = 4)


plot(Time, mu[-1], main = "Filter", ylim = c(-5, 10))
lines(Time, y, col = "black")
lines(ks$xf, col = "blue", lwd = 2)
lines(ks$xf + 2 * sqrt(ks$Pf), lty = 2, col = 4)
lines(ks$xf - 2 * sqrt(ks$Pf), lty = 2, col = 4)


plot(Time, mu[-1], main = "Smooth", ylim = c(-5, 10))
lines(Time, y, col = "black")
lines(ks$xs, col = "blue", lwd = 2)
lines(ks$xs + 2 * sqrt(ks$Ps), lty = 2, col = 4)
lines(ks$xs - 2 * sqrt(ks$Ps), lty = 2, col = 4)




# -->
# Note that one-step-aheaad prediction is more uncertain than the corresponding filtered value,
# which, in turn, is more uncertain than the corresponding smoother value
# Also, in each case, the error variances stabilize quickly



# ----------
# Initial value info.

mu[1]

ks$x0n

sqrt(ks$P0n)

