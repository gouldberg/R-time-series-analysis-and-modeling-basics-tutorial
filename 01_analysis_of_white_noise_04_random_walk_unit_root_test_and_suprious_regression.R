
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# set parameter
# ------------------------------------------------------------------------------


n <- 500


mu <- 0


sd <- 1


drift <- 0.1




# ------------------------------------------------------------------------------
# random walk
# ------------------------------------------------------------------------------


rw_mat <- sapply(1:100, function(x) cumsum(rnorm(n = n, mean = mu, sd = sd)))


matplot(rw_mat, type = "l")
abline(h = mu, col = "black", lwd = 2)





# ------------------------------------------------------------------------------
# random walk with drift
# ------------------------------------------------------------------------------


# random walk

w <- rnorm(n = 500, mean = mu, sd = sd)

rw <- cumsum(w)



# ----------
# random walk with drift looks like trend series
# x(t) = drift + sum from j = 1 to t ( w(j) )

rw_drift <- cumsum(w + drift)




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




# ------------------------------------------------------------------------------
# plot time series
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,1))

plot(rw_drift, type = "l", lty = 1, lwd = 2, col = "blue", main = "random walk (black), random walk with drift (blue), deterministic trend (red)", 
     cex.main = 1.2, ylim = c(-20, 80))

lines(rw, col = "black", lwd = 2)

lines(dt, col = "red", lwd = 2)

abline(h = mu, col = "black", lty = 1, lwd = 1)

abline(a = mu, b = drift, col = "blue", lty = 2)




# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------


# other than white noise, the time series are not stationary, so the periodogram is not well-defined.

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(w, log = "no", lwd = 2, cex.main = 2, main = "white noise")

astsa::mvspec(rw, log = "no", lwd = 2, cex.main = 2, main = "random walk")

astsa::mvspec(dt, log = "no", lwd = 2, cex.main = 2, main = "deterministic trend")

astsa::mvspec(rw_drift, log = "no", lwd = 2, cex.main = 2, main = "random walk with drift")




# ------------------------------------------------------------------------------
# take the 1st difference
# ------------------------------------------------------------------------------


rw_d <- diff(rw)

dt_d <- diff(dt)

rw_drift_d <- diff(rw_drift)




# ----------
# all the differened time series are white noise

graphics.off()

par(mfrow = c(2,2))


plot(rw_d, type = "l")

plot(dt_d, type = "l")

plot(rw_drift_d, type = "l")






# ------------------------------------------------------------------------------
# testing unit roots for stationarity
# ADF test (Augmented Dickey Fuller test):
#   - the default numbber of AR components included in the model, say k, is [[(n-1)^(1/3)]],
#     which corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with
#     the sample size for the general ARMA(p, q) setup.
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

# Note that in each of DF, ADF and PP tests, the general regression equation incorporates a constant and a linear trend.


# k: the lag order to calculate the test statistic

# DF test:  reject the null hypothesis that this time series has a unit root

tseries::adf.test(w, k = 0)

tseries::adf.test(dt, k = 0)

tseries::adf.test(rw, k = 0)

tseries::adf.test(rw_drift, k = 0)





# ----------
# ADF test

tseries::adf.test(w)

tseries::adf.test(dt)

tseries::adf.test(rw)

tseries::adf.test(rw_drift)





# ------------------------------------------------------------------------------
# testing unit roots for stationarity
# PP test (Phillips-Perron test):
#   - the default value of k is [[0.04 * n^(1/4)]]
#   - differs from the ADF tests mainly in how to deal with serial correaltion and heteroskedasticity in the errors
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------


tseries::pp.test(w)

tseries::pp.test(dt)

tseries::pp.test(rw)

tseries::pp.test(rw_drift)




# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# KPSS test:  Null hypothesis is stationarity
# ------------------------------------------------------------------------------


library(urca)


# we need "summary" for details
# type:  deterministic part, mu for a constant and tau for a constant with linear trend

summary(ur.kpss(w, type = "mu"))


summary(ur.kpss(dt, type = "tau"))


summary(ur.kpss(rw, type = "tau"))


summary(ur.kpss(rw_drift, type = "tau"))



# -->
# for 'dt', the test does not reject,
# but for 'rw' and 'rw_drift', the test rejects




# ------------------------------------------------------------------------------
# Suprious Regression
# ------------------------------------------------------------------------------

# generate 2 random walk processes

graphics.off()

par(mfrow = 2,1)

rw_1 <- cumsum(rnorm(n = n, mean = mu, sd = sd))

rw_2 <- cumsum(rnorm(n = n, mean = mu, sd = sd))

rw_mat <- matrix(cbind(rw_1, rw_2), ncol = 2)

matplot(rw_mat, type = "l")

abline(h = mu, col = "black", lwd = 2)




# ----------

summary(lm(rw_2 ~ rw_1))





# ----------
# simulate how many times the regression coefficients is significant


misekake <- function(n){
  
  p <- numeric(n)
  
  cnta <- 0
  
  r <- 0.05
  
  for(j in 1:n){
    
    rw_1 <- cumsum(rnorm(n = n, mean = mu, sd = sd))
    
    rw_2 <- cumsum(rnorm(n = n, mean = mu, sd = sd))
    
    result <- summary(lm(rw_2 ~ rw_1))
    
    q <- result$coefficients[[2,4]]
    
    p[j] <- q
    
    if(q < r){ cnta <- cnta + 1}
  }
  
  rate <- cnta / n * 100
  
  print(paste("n = ", n, "rate = ", rate))
}


for(j in 1:10) misekake(100)



# -->
# regression coefficient is significant in around 70 times


