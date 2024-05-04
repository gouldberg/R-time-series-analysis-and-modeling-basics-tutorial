
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Recurrence plot for logistic map
#   - Recurrence Plot shows how the trajectory repeats itself,
#     and that it returns to visit the states already visited in the past, within the margin of a tolerance eps.
#   - Note that an recurrence plot is a 2-dimensional representation of a trajectory in a higher-dimensional phase space
# ------------------------------------------------------------------------------


x <- numeric()

xt <- numeric()


f.x <- function(x, r){
  r * x * (1 - x)
}



f.temp <- function(xinit, nstep, r){
  
  x <- xinit
  
  xt[1] <- x
  
  for(i in 2:nstep){
    y <- f.x(x, r)
    
    x <- y
    
    xt[i] <- x
    
  }
  return(xt)
}



# ----------

# cycle of period 4
r <- 3.52

# chaos
r <- 4

# periodic window, a cycle of period 3
r <- 3.835



# transient
ntrans <- 100


# iterations after the transient
n <- 100

nt <- ntrans + n

xinit <- 0.2

eps <- 0.01

xt <- f.temp(xinit, nt, r)


# random deviates from the uniform distribution
# xt <- runif(nt)


# ----------
graphics.off()

par(mfrow = c(1,1))

plot(0, 0, type = "n", xlab = "i", ylab = "j", xlim = c(0, n), ylim = c(0, n), cex.lab = 1.7, cex.axis = 1.3)


x[1:n] <- xt[(ntrans + 1):nt]

for(i in 1:n){
  for(j in 1:n){
    distx <- abs(x[i] - x[j])
    
    if(distx < eps) points(i, j, pch = 19, cex = 1.2)
    
    else points(i, j, pch = 21, cex = 1.2)
  }
}




# ------------------------------------------------------------------------------
# Recurrence plot for lorenz attractor
# ------------------------------------------------------------------------------

# Lorenz Equation
# dx / dt = sigma * (y - x)
# dy / dt = x * (rho - z) - y
# dz / dt = x * y - beta * z



# parameters: sigma, beta, rho
parms <- c(10, 8/3, 28)

tinit <- 0

tfin <- 100


step <- 0.01

times <- seq(tinit, tfin, by = step)



# ----------
# system equations

funct <- function(t, integ, p){
  
  x <- integ[1]
  
  y <- integ[2]
  
  z <- integ[3]
  
  sigma <- parms[1]
  
  beta <- parms[2]
  
  rho <- parms[3]
  
  dx <- sigma * (y - x)
  
  dy <- x * (rho - z) - y
  
  dz <- x * y - beta * z
  
  list(c(dx, dy, dz))
}



# ----------

library(deSolve)


cinit <- c(1,1,1)


xyz <- lsoda(cinit, times, funct, parms)



# ----------
n <- 800

eps <- 5

distx <- matrix(, n, n)

x <- xyz[,2]

y <- xyz[,3]

z <- xyz[,4]



# ----------
# Thresholded Recurrence plot
graphics.off()

par(mfrow = c(1,1))

plot(0, 0, type = "n", xlab = "i", ylab = "j", xlim = c(0, n), ylim = c(0, n), cex.lab = 1.7, cex.axis = 1.3)


# IT TAKES TIME !!!
for(i in 1:n){
  for(j in 1:n){
    distx[i,j] <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2 + (z[i] - z[j])^2)
    
    if(distx[i,j] < eps) points(i, j, pch = 19, cex = 0.01)
    
  }
}




# ----------
# Un-Thresholded Recurrence plot

time <- seq(1:n)

library(gplots)


# normalized distances matrix previously computed
distx <- distx / max(distx)


distx <- 1 - distx


graphics.off()

par(mfrow = c(1,1))

filled.contour(time, time, distx, col = gray.colors(10, start = 1, end = 0), nlevels = 10,
               xlab = "i", ylab = "j", main = "", xlim = c(0, n), ylim = c(0, n), las = 0,
               key.axes = axis(4, las = 1))



# ------------------------------------------------------------------------------
# Recurrence plot for lorenz attractor
# ------------------------------------------------------------------------------

# IT TAKES TIME ...!!
# recurr(x, m = 3, d = 18, start.time = start(x), end.time = end(y))
