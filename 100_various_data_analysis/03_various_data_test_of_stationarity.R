setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Test for stationarity by KPSS test for hakusan data
# ------------------------------------------------------------------------------

library(forecast)



# ----------
ndiffs(hakusan[,"YawRate"], test = "kpss")


apply(hakusan, 2, ndiffs)



# ----------
# KPSS stationarity test by unit root test:
# Null hypothesis of KPSS test:  an observable time series is stationary around a deterministic trend
# against the alternative of a unit root
# urca::ur.kpss(): the test types specify as deterministic component
# either a constant "mu" or a constant with linear trend "tau"

urca::ur.kpss(hakusan, type ="mu", lags = "long")


ndiffs(lynx, test = "kpss")




# ------------------------------------------------------------------------------
# those data are not stationary
# ------------------------------------------------------------------------------

ndiffs(maxtemp, test = "kpss")


ndiffs(whard, test = "kpss")


ndiffs(blsfood, test = "kpss")


ndiffs(nikkei225, test = "kpss")


ndiffs(lynx, test = "kpss")


urca::ur.kpss(whard, type ="tau", lags = "long")





# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))


y <- lynx
y <- sunspot
y <- my1ef


graphics.off()

par(mfrow = c(2,1))

plot(y)

ndiffs(y, test = "kpss")

plot(diff(y))




