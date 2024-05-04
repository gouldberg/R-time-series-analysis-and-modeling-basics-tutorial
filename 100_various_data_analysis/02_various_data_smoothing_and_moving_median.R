setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Data Transformation for maxtemp:  smoothing
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(3,1))



# ----------
plot(maxtemp, main = "original maxtemp and moving average")

# moving average, weights = c(1/3, 1/3, 1/3)
lines(stats::filter(maxtemp, rep(1/17,17), side = 1), lwd = 2, col = "red")



# ----------
# moving medians is better to detect structural change
plot(maxtemp, ylim=c(0,40), main = "original and moving medians")
y <- maxtemp
ndata <- length(maxtemp)
y[1:ndata] <- NA
kfilter <- 17
n0 <- kfilter+1
n1 <- ndata-kfilter
for(i in n0:n1){
  i0 <- i-kfilter
  i1 <- i+kfilter
  y[i] <- median(maxtemp[i0:i1])
}
lines(y,col=3,ylim=c(0,40),lwd=2)



# ----------
plot(maxtemp, main = "original and smooth.splines")

lines(smooth.spline(maxtemp, spar = 0.25), lwd = 2, col = "blue")




# ------------------------------------------------------------------------------
# moving average and moving median
# ------------------------------------------------------------------------------

z <- rep(0, 100)

z[51:100] <- 1

z[25] <- 1

z[75] <- 2

z <- as.ts(z)

ndata <- length(z)

y <- z

y[1:ndata] <- NA



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(z)


# moving average:  span = 9 and 3
lines(c(0, stats::filter(z, rep(1/9,9), side = 2)[2:99], 0), lwd = 2, col = "blue")

lines(c(0, stats::filter(z, rep(1/3,3), side = 2)[2:99], 0), lwd = 2, col = "red")




# ----------
# moving medians:  span = 9 and 3

plot(z)

kfilter <- 9
n0 <- kfilter + 1
n1 <- ndata - kfilter

for(i in n0:n1){
  i0 <- i - kfilter
  i1 <- i + kfilter
  y[i] <- median(z[i0:i1])
}

lines(y, col = "blue", ylim = c(0,40), lwd = 2)


kfilter <- 9
n0 <- kfilter + 1
n1 <- ndata - kfilter

for(i in n0:n1){
  i0 <- i - kfilter
  i1 <- i + kfilter
  y[i] <- median(z[i0:i1])
}

lines(y, col = "red", ylim = c(0,40), lwd = 2)




# ------------------------------------------------------------------------------
# moving average and moving median
# ------------------------------------------------------------------------------

z <- rep(0,400)

z[101:200] <- 2

z[201:300] <- -2

z <- z + rnorm(400, mean=0, sd=0.5)

z <- as.ts(z)

ndata <- length(z)

y <- z

y[1:ndata] <- NA



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(z)


# moving average:  span = 29
lines(c(0, stats::filter(z, rep(1/29,29), side = 2)[2:399], 0), lwd = 2, col = "blue")



# ----------
# moving medians:  span = 29

plot(z)

kfilter <- 29
n0 <- kfilter + 1
n1 <- ndata - kfilter

for(i in n0:n1){
  i0 <- i - kfilter
  i1 <- i + kfilter
  y[i] <- median(z[i0:i1])
}

lines(y, col = "blue", lwd = 2)

