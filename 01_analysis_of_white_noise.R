
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# generate white noise data
# ------------------------------------------------------------------------------

set.seed(12345)


n <- 500


ts1 <- rnorm(n = n, mean = 0, sd = 1)


# later we use
ts2 <- rnorm(n = n, mean = 0, sd = 10)




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(ts1, type = "l", lty = 1, lwd = 2, main = "white noise", cex.main = 1.5)




# ------------------------------------------------------------------------------
# Autocorrelation and Partial Autocorrelation
# ------------------------------------------------------------------------------

acf2(ts1)




# ----------
# if time series is white noise, then for n (= time length) large,
# the sample autocorrelation rho is approximately normally distributed with zero mean and standard deviations

1 / sqrt(n)



# 95% interval
c(1,-1) * 2 / sqrt(n)




# ------------------------------------------------------------------------------
# Raw Periodogram
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

astsa::mvspec(ts1, log = "no", type = "h")


# bandwidth = 1 / n = 0.002




# ------------------------------------------------------------------------------
# Averaged Periodgram
# ------------------------------------------------------------------------------

# modified Daniell kernel puts half weights at the end points

( k1 <- kernel("daniell", c(3)) )

( k2 <- kernel("modified.daniell", c(3)) )

( k3 <- kernel("modified.daniell", c(3, 3)) )


graphics.off()

par(mfrow = c(2,2))


plot(k1);  plot(k2);  plot(k3)




# ----------
# we apply k3 kernel

k <- k3


graphics.off()

par(mfcol = c(2,2))

ts1.per <- astsa::mvspec(ts1, log = "no", type = "h")
ts1.per2 <- astsa::mvspec(ts1, log = "no", type = "h", kernel = k)

ts2.per <- astsa::mvspec(ts2, log = "no", type = "h")
ts2.per2 <- astsa::mvspec(ts2, log = "no", type = "h", kernel = k)




# ------------------------------------------------------------------------------
# Periodogram with more and more smoothing
# ------------------------------------------------------------------------------

# more smoothing by spans
# spans:  vector of odd integers, given in terms of L = 2 * m + 1 instead of m

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(ts1, log = "no", type = "h")
astsa::mvspec(ts1, log = "no", type = "h", spans = c(10,10), ylim = c(0,2.5))
astsa::mvspec(ts1, log = "no", type = "h", spans = c(30,30), ylim = c(0,2.5))
astsa::mvspec(ts1, log = "no", type = "h", spans = c(30,30), ylim = c(0,2.5), taper = 0.25)



# -->
# the smoothed periodogram is closing to 1  (= variance of ts1)



# ---------
# ts2:  smoothed periodogram is closing to 100 (= variance of ts2)

graphics.off()

par(mfrow = c(2,2))

astsa::mvspec(ts2, log = "no", type = "h")
astsa::mvspec(ts2, log = "no", type = "h", spans = c(10,10), ylim = c(0,250))
astsa::mvspec(ts2, log = "no", type = "h", spans = c(30,30), ylim = c(0,250))
astsa::mvspec(ts2, log = "no", type = "h", spans = c(30,30), ylim = c(0,250), taper = 0.25)




# ------------------------------------------------------------------------------
# Parametric Spectral Estimation
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,2))


astsa::mvspec(ts1, log = "no", type = "h")

spec.ar(ts1)


astsa::mvspec(ts2, log = "no", type = "h")

spec.ar(ts2)





# ------------------------------------------------------------------------------
# Theoretical spectral density of white noise
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

arma.spec(log = "no", main = "white noise")




plot(ts1, type = "l")

plot(ts2, type = "l")

ar <- ts2
ar[1] <- 0
ar[2] <- 0

# phi <- c(0.5, -0.9)

phi <- c(1.1)

for(i in 2:length(ts1)){
  
  # AR(2)
  ar[i] <- phi[1] * ar[i-1] + ts1[i]
}



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(ts2, type = "l", lty = 1, lwd = 2, main = "ts2", cex.main = 2)

plot(ar, type = "l", lty = 1, lwd = 2, col = "blue", main = "autoregressive series", cex.main = 2)



