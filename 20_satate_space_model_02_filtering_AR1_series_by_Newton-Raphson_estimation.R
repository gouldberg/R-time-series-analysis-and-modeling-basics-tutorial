
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate AR(1) series
# ------------------------------------------------------------------------------


set.seed(999)


num <- 100

x = arima.sim(n = num + 1, list(ar = 0.8), sd = 1)

y = ts(x[-1] + rnorm(num, 0, 1))



# ----------
graphics.off()

par(mfrow = c(1,1))

plot(1:num, y, type = "l")




# ------------------------------------------------------------------------------
# Initial estimate
# ------------------------------------------------------------------------------


u <- ts.intersect(y, stats::lag(y, -1), stats::lag(y, -2))


varu <- var(u)


coru <- cor(u)


phi <- coru[1,3] / coru[1,2]


q <- (1 - phi^2) * varu[1,2] / phi


r <- varu[1,1] - q / (1 - phi^2)


( init.par <- c(phi, sqrt(q), sqrt(r)) )




# ------------------------------------------------------------------------------
# for reference:  estimate by function sarima
# ------------------------------------------------------------------------------


astsa::sarima(y, p = 1, d = 0, q = 0, no.constant = TRUE)



# -->
# AR1 coefficient = 0.59, not 0.8



# ------------------------------------------------------------------------------
# Function to evaluate the likelihood and estimation
# ------------------------------------------------------------------------------

Linn <- function(para){
  
  phi <- para[1]
  
  sigw <- para[2]
  
  sigv <- para[3]
  
  Sigma0 <- (sigw^2) / (1 - phi^2)
  
  Sigma0[Sigma0 < 0] <- 0
  
  kf <- astsa::Kfilter0(num, y, A = 1, mu0 = 0, Sigma0 = Sigma0, Phi = phi, cQ = sigw, cR = sigv)
  
  return(kf$like)
}




# ----------
# Estimation

( est <- optim(init.par, Linn, gr = NULL, method = 'BFGS', hessian = TRUE, control = list(trace = 1, REPORT = 1)))



names(est)



SE <- sqrt(diag(solve(est$hessian)))


cbind(estimate = c(phi = est$par[1], sigw = est$par[2], sigv = est$par[3]), SE)




# -->
# here the estimated AR1 coefficient is 0.81

