
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# generate data
# ------------------------------------------------------------------------------


n <- 1024



# xt: white noise
xt <- rnorm(n = n, mean = 0, sd = 10)




# ----------
# yt = phi * x(t-D) + w(t)

wt <- rnorm(n = n, mean = 0, sd = 10)

phi <- 0.9

D <- 10

yt <- phi * stats::lag(xt, -D) + wt





# ----------
# plot xt and yt

xyt <- ts.intersect(xt, yt, dframe = TRUE)


plot.ts(xyt)




# ------------------------------------------------------------------------------
# Squared Coherency
# ------------------------------------------------------------------------------

L <- 41


( m <- (L - 1) / 2 )


sr <- mvspec(cbind(xt, yt), kernel("daniell", m), plot = FALSE)



# ----------
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)



