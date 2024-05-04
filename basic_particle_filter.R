
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate data
# ------------------------------------------------------------------------------

rm(list=ls())


library(dlm)


# state follows N(10, 9)

mod <- dlmModPoly(1, dV = 2, dW = 1, m0 = 10, C0 = 9)


n <- 100


set.seed(23)


simData <- dlmForecast(mod = mod, nAhead = n, sampleNew = 1)


y <- simData$newObs[[1]]



# ----------
plot(y, type = "l")



# ------------------------------------------------------------------------------
# Exact filtering distribution with PF approximation
# ------------------------------------------------------------------------------


modFilt <- dlmFilter(y, mod)


( thetaHatKF <- modFilt$m[-1] )


( sdKF <- with(modFilt, sqrt(unlist(dlmSvd2var(U.C, D.C))))[-1] )




# ------------------------------------------------------------------------------
# Basic Particle Filter  -  Optimal Importance Density
# ------------------------------------------------------------------------------

# number of partiles

N <- 1000


N_0 <- N / 2


pfOut <- matrix(NA_real_, n + 1, N)


wt <- matrix(NA_real_, n + 1, N)


importanceSD <- sqrt(drop(W(mod) - W(mod)^2 / (W(mod) + V(mod))))


predSd <- sqrt(drop(W(mod) + V(mod)))




# ----------
# Initialize sampling from the prior

pfOut[1,] <- rnorm(N, mean = m0(mod), sd = sqrt(C0(mod)))


wt[1,] <- rep(1 / N, N)


for(it in 2:(n + 1)){
  
  # ----------
  # generate particle
  
  means <- pfOut[it - 1, ] + W(mod) * (y[it - 1] - pfOut[it - 1,]) / ( W(mod) + V(mod) )
  
  pfOut[it, ] <- rnorm(N, mean = means, sd = importanceSD)
  
  
  # ----------
  # update the weights
  
  wt[it, ] <- dnorm(y[it - 1], mean = pfOut[it - 1, ], sd = predSd) * wt[it - 1, ]
  
  wt[it, ] <- wt[it, ] / sum(wt[it, ])

  
  # ----------
  # resample, if needed
  
  N.eff <- 1 / crossprod(wt[it, ])
  
  if(N.eff < N_0){
    
    # ----------
    # multinomial resampling
    
    index <- sample(N, N, replace = TRUE, prob = wt[it,])
    
    pfOut[it, ] <- pfOut[it, index]
    
    wt[it, ] <- 1 / N
  }
}




# ----------
pfOut <- pfOut[-1,]


( wt <- wt[-1,] )


thetaHatPF <- sapply(1:n, function(i) weighted.mean(pfOut[i,], wt[i,]))


sdPF <- sapply(1:n, function(i) sqrt(weighted.mean((pfOut[i,] - thetaHatPF[i]) ^ 2, wt[i, ])))




# ------------------------------------------------------------------------------
# compare 
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))

plot.ts(cbind(thetaHatKF, thetaHatPF), plot.type = "s", lty = c("dotted", "longdash"), xlab = "", ylab = expression(m[t]))

legend("topleft", c("Kalman", "particle"), lty = c("dotted", "longdash"), bty = "n")



plot.ts(cbind(sdKF, sdPF), plot.type = "s", lty = c("dotted", "longdash"), xlab = "", ylab = expression(sqrt(C[t])))

legend("topleft", c("Kalman", "particle"), lty = c("dotted", "longdash"), bty = "n")















