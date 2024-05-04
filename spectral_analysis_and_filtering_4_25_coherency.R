setwd("//media//kswada//MyFiles//R//time_series_analysis_and_its_applications")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Two processes
# ------------------------------------------------------------------------------

n <- 1024

phi <- 0.9

sigma2 <- 1


set.seed(154)
w <- rnorm(n, mean = 0, sd = sqrt(sigma2))

set.seed(155)
v <- rnorm(n, mean = 0, sd = sqrt(sigma2))



# ----------
D <- 0


x <- w

y <- phi * stats::lag(x, -D) + v


tmp <- ts.intersect(x, y)



# ----------
par(mfrow = c(2,1))
plot(tmp[,"x"], lty = 1, type = "l", col = "black")
plot(tmp[,"y"], lty = 1, type = "l", col = "blue")




# ------------------------------------------------------------------------------
# Spectral analysis:  Coherence between two processes
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------

# choose L
L <- 1

L <- 3

L <- 41

L <- 101


# ----------
( m <- (L - 1)/2 )


par(mfrow = c(1,1))

sr <- mvspec(cbind(x, y), kernel("daniell", m), plot=FALSE)



sr$df



# ----------
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


graphics.off()
par(mfrow = c(2,1))
plot(sr)
plot(sr, plot.type = "coh", ci.lty = 2)
abline(h = C)

