setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate random walk data
# ------------------------------------------------------------------------------

set.seed(123)

num <- 50

w <- rnorm(num, 0, 0.1)

x <- cumsum(cumsum(w))

y <- x + rnorm(num, 0, 1)



# ----------
plot.ts(x, ylab = "", lwd = 2, ylim = c(-1, 8))

lines(y, type = "o", col = 8)




# ------------------------------------------------------------------------------
# for reference:  estimate by function sarima
# ------------------------------------------------------------------------------


astsa::acf2(diff(y))


astsa::sarima(y, p = 2, d = 1, q = 1)


astsa::sarima(y, p = 0, d = 1, q = 1)


# -->
# ARIMA(0, 1, 1) ??


# note that sigma^2 = 1.2




# ------------------------------------------------------------------------------
# State Space
# ------------------------------------------------------------------------------

# A: time-invariant observation matrix
# mu0: initial state mean vector
# Sigma0:  initial state covariance matrix
# Phi:  state transition matrix
# cQ:  Cholesky-type decomposition of state error covariance matrix Q
# cR:  Cholesky-type decomposition of observation error covariance matrix R

( Phi <- matrix(c(2, 1, -1, 0), 2) )

( A <- matrix(c(1, 0), 1) )

( mu0 <- matrix(0, 2) )

( Sigma0 <- diag(1, 2) )




# ------------------------------------------------------------------------------
# Function to evaluate the likelihood and estimation
# ------------------------------------------------------------------------------

Linn <- function(para){
  
  sigw <- para[1]
  
  sigv <- para[2]
  
  cQ <- diag(c(sigw, 0))
  
  kf <- astsa::Kfilter0(num, y, A, mu0, Sigma0, Phi, cQ, sigv)

  return(kf$like)
}



# ----------
# Estimation

init.par <- c(0.1, 1)

( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)) )


SE <- sqrt(diag(solve(est$hessian)))




# ----------
# Summary of estimation

estimate <- est$par

u <- cbind(estimate, SE)


rownames(u) <- c("sigw", "sigv")

u



# ------------------------------------------------------------------------------
# Smoothing
# ------------------------------------------------------------------------------


sigw <- est$par[1]

cQ <- diag(c(sigw, 0))

sigv <- est$par[2]

ks <- astsa::Ksmooth0(num, y, A, mu0, Sigma0, Phi, cQ, sigv)



# ----------
# smoothing parameter lambda = sigv^2 / sigw^2
sigv^2 / sigw^2




# -----------
xsmoo <- ts(ks$xs[1,1,])

psmoo <- ts(ks$Ps[1,1,])

upp <- xsmoo + 2 * sqrt(psmoo)

low <- xsmoo - 2 * sqrt(psmoo)



# ----------
graphics.off()

# state
plot.ts(x, ylab = "", lwd = 2, ylim = c(-1, 8))

# observations
lines(y, type = "o", col = 8)

# Kalman Smoother, parameter estimated using Newton-Raphson techniques
lines(xsmoo, col = 4, lty = 2, lwd = 3)

lines(upp, col = 4, lty = 2)
lines(low, col = 4, lty = 2)

# Smoothing Splines based on the method of generalized cross-validation (GCV)
lines(smooth.spline(y), lty = 1, col = 2)

legend("topleft", c("Observations", "State"), pch = c(1, -1), lty = 1, lwd = c(1, 2), col = c(8, 1))
legend("bottomright", c("Smoother", "GCV Spline"), lty = c(2, 1), lwd = c(3, 1), col = c(4, 2))



# -->
# Kalman smoother and Smoothing splines are very close to each other.



# ----------
# for reference:
smooth.spline(y)



