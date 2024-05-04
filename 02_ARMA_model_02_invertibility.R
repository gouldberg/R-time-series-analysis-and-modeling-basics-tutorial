packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# generate ARMA(1,1) data
# ------------------------------------------------------------------------------


set.seed(123456)



# (1 - 0.9B) x(t) = (1 + 0.5B) w(t)
# --> x(t) = (1 + 0.5 B) / (1 - 0.9B) * w(t)

# this model is causal:  z = 10/9  (outside the unit circle)
# this model is invertible:  z = -2  (outside the unit circle)


x <- arima.sim(list(order = c(1, 0, 1), ar = 0.9, ma = 0.5), n = 1000)




# ----------
graphics.off()

par(mfrow = c(1,1))


plot(x, type = "l")




# ----------
# it seems like ARMA(3,0,X)
astsa::acf2(x)




# ------------------------------------------------------------------------------
# fit ARMA model
# ------------------------------------------------------------------------------


mod <- arima(x, order = c(1, 0, 1))


summary(mod)



# -->
# estimated coefficient is close to parameters set for simulation




# ------------------------------------------------------------------------------
# ARMA model can be inverted to MA model
# ------------------------------------------------------------------------------


# first 10 psi weights
ARMAtoMA(ar = 0.899, ma = 0.484, 10)



# -->
# psi1 = 0.899 + 0.484 =  1.383
# psi2 = 0.899 * phi1 = 1.2433
# ...



# -->
# x(t) = w(t) + 1.38 * { w(t-1) + 0.9 w(t-2) + 0.9^2 w(t-3) + .... }




# ------------------------------------------------------------------------------
# ARMA model can be inverted to AR model
# ------------------------------------------------------------------------------


# first 10 
ARMAtoMA(ar = -0.484, ma = -0.899, 10)



# -->
# x(t) = w(t) - 1.38 * { x(t-1) + (- 0.5) x(t-2) + (-0.5)^2 x(t-3) + .... }



