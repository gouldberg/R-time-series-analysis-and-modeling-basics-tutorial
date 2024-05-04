packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# generate ARMA(1,1) and ARMA(2,2) data
# ------------------------------------------------------------------------------


set.seed(123456)



# 2 series:
# 1.  (1 - 0.4B - 0.45B^2) x(t) = (1 + B + 0.25B^2) w(t)
# 2.  --> (1 + 0.5B) (1 - 0.9B) x(t) = (1 + 0.5B) (1 + 0.5B) w(t)
#     --> (1 - 0.9B) x(t) = (1 + 0.5B) w(t)


# ar = c(0.4, 0.45) is not invertible:  there are abs(root) > 1.0
abs(polyroot(c(1, -0.4, -0.45)))




# generate 2 series
x1 <- arima.sim(list(order = c(2, 0, 2), ar = c(0.4, 0.45), ma = c(1, 0.25)), n = 1000)

x2 <- arima.sim(list(order = c(1, 0, 1), ar = 0.9, ma = 0.5), n = 1000)




# ----------
graphics.off()

par(mfrow = c(2,1))


plot(x, type = "l")

plot(x2, type = "l")




# ------------------------------------------------------------------------------
# Fit ARMA(2,2) model and ARMA(1,1) model to x1
# ------------------------------------------------------------------------------


astsa::acf2(x1, max.lag = 100)



sarima(x1, p = 2, d = 0, q = 2, no.constant = TRUE)



# -->
# coefficient is not significant ...
# but model fitting is good ....
# estimated coefficient is close to parameters set for simulation




# ----------
sarima(x1, p = 1, d = 0, q = 1, no.constant = TRUE)



# -->
# ARMA(1,1) model is significant and model fitting is good
# estimated coefficient is close to parameters set for simulation




# ------------------------------------------------------------------------------
# Fit ARMA(2,2) model and ARMA(1,1) model to x2
# ------------------------------------------------------------------------------


astsa::acf2(x2, max.lag = 100)


sarima(x2, p = 2, d = 0, q = 2)



# -->
# coefficient is not significant ...
# but model fitting is good ....




# ----------
sarima(x2, p = 1, d = 0, q = 1, no.constant = TRUE)



# -->
# ARMA(1,1) model is significant and model fitting is good
# estimated coefficient is close to parameters set for simulation

