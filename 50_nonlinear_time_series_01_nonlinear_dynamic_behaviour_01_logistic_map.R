
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Nonlinear logsitic map:  time series and cobweb plots
# ------------------------------------------------------------------------------


f.x <- function(x, r){
  r * x * (1 - x)
}



# ----------
# function to draw the time plot
f.temp <- function(xinit, nstep, r){
  
  xt <- numeric()
  
  x <- xinit
  
  xt[1] <- x
  
  for(i in 2:nstep){
    
    y <- f.x(x, r)
    
    x <- y
    
    xt[i] <- x
  }
  
#  graphics.off()
  par(mfcol = c(2,2))
  plot(xt, type = "b", xlab = "time", ylab = "x(t)", cex.lab = 1.7, cex.axis = 1.3, lwd = 2)
  eqr <- round((r - 1) / r, 5)
  abline(h = eqr, col = "blue", lty = 2)
  print(paste0("x(t) of equilibrium state, attractor: ", eqr))
  
  
  # fixed point multiplier
  fpm <- r * (1 - 2 * xt)
  plot(fpm, type = "l", xlab = "time", ylab = "fixed point multiplier", cex.lab = 1.7, cex.axis = 1.3, lwd = 2)
  abline(h = c(-1, 1), col = "red", lty = 2)
  
  
  # histogram of the positions x(t)
  hist(xt, breaks = seq(0, 1, by = 0.02))
}




# ----------
# function to draw the cobweb plot
iter <- function(xinit, nstep, r){
  
  x <- xinit
  
  y <- f.x(x, r)
  
  segments(x, 0, x, y, lty = 1, lwd = 2)
  
  for(i in 1:nstep){
    
    points(x, y, pch = 19, cex = 1.5)
    
    segments(x, y, y, y, lty = 1, lwd = 2)
    
    x <- y
    
    y <- f.x(x, r)
    
    segments(x, x, x, y, lty = 1, lwd = 2)
  }
  
}



# ----------
# parameters  and initial conditions
# m = 2 - r, so that the fixed point is stable for 1 < r < 3
# and unstable for 3 < r < 4

# stable fixed point
r <- 2.8


# introduce periodic fixed point attractors
# r <- 3.2
# r <- 3.5
r <- 3.8283


# unstable fixed point
# r <- 4


# r <- 5


# you can chance initial value with very small delta
xinit <- 0.002 + 0.00001

nstep <- 5000



# ----------
f.temp(xinit, nstep, r)



windows()

plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "x(t)", ylab = "x(t+1)", cex.lab = 1.5, cex.axis = 1.2)

curve(f.x(x, r), from = 0, to = 1, lty = 5, lwd = 2, col = "black", add = T)


segments(0, 0, 1, 1, lty = 3, lwd = 2, col = "black")

iter(xinit, nstep, r)




# ----------
# when r = 4:  trajectories are dependent on initial value.
# slight change of initial values results in very different trajectories,
# but same histograms !! = Beta(0.5, 0.5)


# when = r = 3.8283 and 5000 iterations.
# histogram of x(t) shows 3 distinct peaks appear around x(t) values = 0.163, 0.518, 0.963



# ------------------------------------------------------------------------------
# Nonlinear logsitic map:  bifurcation diagram
#   - shows the mechanism of the period doubling up to the vertical dash-dot line at r = 3.56994
#   - a dynamical system has a bifurcation when a tiny variation of a parameter causes a sudden qualitative change
#     in the system's dynamics.
# ------------------------------------------------------------------------------


# transient
ntrans <- 1000


# r varies from rin to rfin
rin <- 2.8

rfin <- 4


# number of iterations after the transient
n <- 400


# total number of iterations
nt <- ntrans + n


# number of r step
nr <- 241


xinit <- 0.2


( r <- seq(rin, rfin, length = nr) )



graphics.off()
par(mfrow = c(1,1))

plot(0, 0, type = "n", xlim = c(rin, rfin), ylim = c(0, 1), xlab = "r", ylab = "x(t)", cex.lab = 1.5,
     cex.axis = 1.2)


for(i in r){
  x <- xinit
  
  for(j in 1:nt){
    y <- f.x(x, i)
    
    if(j > ntrans) points(i, y, pch = ".", cex = 3)
    
    x <- y
  }
}




r2 <- 3.569944

segments(r2, 0, r2, 1, lty = 4, lwd = 2, col = "black")




# ------------------------------------------------------------------------------
# Nonlinear logsitic map:  M trajectories, initial distritbuion: uniform in (0,1)
# Invariant Density rho(x, x0) = density of points in the interval [x, x + dx] with initial point x0, after n --> inf iterations
# ------------------------------------------------------------------------------


r <- 4


# number of trajectories
M <- 1000


# number of iterations
nstep <- 1000

xt <- numeric()

xiniz0 <- numeric()



# ----------
# to memorize the single trajectory
xens <- matrix(, M, nstep)



# ---------
set.seed(1234)

for(l in 1:M){
  
  # uniform distribution in (0,1)
  xiniz0[l] <- runif(1)
  
  x <- xiniz0[l]
  
  xt[1] <- x
  
  for(i in 1:nstep){
    
    y <- f.x(x, r)
    
    x <- y
    
    xt[i] <- x
    
    xens[l, i] <- xt[i]
  }
}



# ----------
# evolution of the invariant density of the logistic map with r = 4

mstep1 <- 1
mstep2 <- 2
mstep3 <- 3
mstep4 <- 5
mstep5 <- nstep

lbin <- 0.02

graphics.off()

par(mfrow = c(2,3), cex.main = 0.8)

hist(xiniz0, probability = T, xlab = "x(t)", ylab = "density", main = "initial distr.",
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

hist(xens[,mstep1], probability = T, xlab = "x(t)", ylab = "density", main = "t = 1",
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

hist(xens[,mstep2], probability = T, xlab = "x(t)", ylab = "density", main = "t = 2",
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

hist(xens[,mstep3], probability = T, xlab = "x(t)", ylab = "density", main = "t = 3",
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

hist(xens[,mstep4], probability = T, xlab = "x(t)", ylab = "density", main = "t = 5",
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

hist(xens[,mstep5], probability = T, xlab = "x(t)", ylab = "density", main = paste0("t = ", nstep),
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")



# ----------
# the property of ergodicity

# 35th step point of 1000 trajectories
hist(xens[,mstep5], probability = T, xlab = "x(t)", ylab = "density", main = paste0("t = ", nstep),
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")

# 1 trajectory (1000 point)
hist(xens[1,], probability = T, xlab = "x(t)", ylab = "density", main = paste0("t = ", nstep),
     xlim = c(0,1), ylim = c(0, 6), br = seq(0, 1, by = lbin), col = "black", border = "black")



# ------------
# The invariant density describes the stationary behaviour of an ensemble of trajectories with
# different initial conditions.
# It can be derived both from the temporal evolution of a single trajectory and 
# from an ensemble of trajectories considered at fixed instants in time.


# In a chaotic system, we cannot make predictions about a sinngle trajectory
# if we address it from an exclusively deterministic point of view.
# However, if we apply a statistical approach, we can then distinguish regions that are more or less probable,
# so we should observe an ensemble of trajectories with different initial conditions,
# even though we know only one trajectory.

