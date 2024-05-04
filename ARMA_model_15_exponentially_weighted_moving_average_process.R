setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Exponentially weighted moving average (EWMA) process
# ------------------------------------------------------------------------------


set.seed(666)


# Generate 100 observations from IMA(1,1) model with lambda = - theta = 0.8
x <- arima.sim(list(order = c(0, 1, 1), ma = -0.8), n = 100)



# Fit EWMA superimposed on the IMA(1,1)
x.ima <- HoltWinters(x, beta = FALSE, gamma = FALSE)


x.ima



# ----------
graphics.off()

par(mfrow=c(1,2))

plot(x)

plot(x.ima)
