
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# amplitude modulated signal
# ------------------------------------------------------------------------------


n <- 200


# cycles per unit time (here: t)
# delta should be very small

alpha <- 0.20

delta <- 0.01



# ----------
# x2 has very small difference in frequencies compared to x1

x1 <- cos(2 * pi * 1:n * (alpha + delta))

x2 <- cos(2 * pi * 1:n * (alpha - delta))




# ----------
# the x3 will oscillate at frequency alpha, but the amplitude will be modulated by cos(2*pi*delta*(1:n))
# 200 * 0.2 = 40 cycles per 200 time points
# 200 * 0.01 = 2 cycles per 200 time points

x3 <- x1 + x2




# ----------
# 2 * cos(alpha) * cos(delta) = cos(alpha + delta) + cos(alpha - delta)

x4 <- 2 * cos(2 * pi * 1:n * alpha) * cos(2 * pi * 1:n * delta)



# plot x1, x2, x3 and x4

graphics.off()

par(mfrow = c(2,2))

plot.ts(x1, ylim = c(-5, 5), main = "x1")

plot.ts(x2, ylim = c(-5, 5), main = "x2")

plot.ts(x3, ylim = c(-5, 5), main = "x3 = x1 + x2")

plot.ts(x4, ylim = c(-5, 5), main = "x4")



# -->
# note that x3 and x4 is equal process





# ------------------------------------------------------------------------------
# Fast Discrete Fourier Transform by fft
# ------------------------------------------------------------------------------

# Discrete Fourier Transform (DFT):
# d(omega(j)) = 1 / sqrt(n)  *  sum from t = 1 to n { x(t) * exp(- 2 * pi * i * omega(j) * t) }
# j = 0, 1 ..., n-1
# omega(j) = j / n  (Fourier or Functamental frequencies)


# R function fft() computes without "1 / sqrt(n)"



# ----------

dft1 <- fft(x1) / sqrt(n)

dft2 <- fft(x2) / sqrt(n)

dft3 <- fft(x3) / sqrt(n)

dft4 <- fft(x4) / sqrt(n)




# ----------
graphics.off()
par(mfcol = c(3,4))

plot(abs(dft1), type = "h", main = "x1: dft Absolute")
plot(Re(dft1), type = "h", main = "x1: cosine component")
plot(Im(dft1), type = "h", main = "x1: sine component")

plot(abs(dft2), type = "h", main = "x2: dft Absolute")
plot(Re(dft2), type = "h", main = "x2: cosine component")
plot(Im(dft2), type = "h", main = "x2: sine component")

plot(abs(dft3), type = "h", main = "x3: dft Absolute")
plot(Re(dft3), type = "h", main = "x3: cosine component")
plot(Im(dft3), type = "h", main = "x3: sine component")

plot(abs(dft4), type = "h", main = "x4: dft Absolute")
plot(Re(dft4), type = "h", main = "x4: cosine component")
plot(Im(dft4), type = "h", main = "x4: sine component")




# ------------------------------------------------------------------------------
# Example:  Star Magnitude
# ------------------------------------------------------------------------------

star


n <- length(star)



# ----------
graphics.off()

par(mfrow = c(2,1), mar = c(3,3,1,1), mgp = c(1.6, 0.6, 0))

plot(star, ylab = "star magnitude", xlab = "day")



# ----------
Per <- Mod(fft(star - mean(star)))^2 / n

# Per <- Mod(fft(star))^2 / n


Freq <- (1:n - 1) / n


plot(Freq[1:50], Per[1:50], type = "h", lwd = 3, ylab = "Periodogram", xlab = "Frequency")




# ----------
( u <- which.max(Per[1:50]) )

( uu <- which.max(Per[1:50][-u]) )




# ----------
# days / cycle

1 / Freq[u]

1 / Freq[uu]



