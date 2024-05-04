
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Mathematical mechanics of Toeplitz SSA
# ------------------------------------------------------------------------------


# sample time series
x <- c(1, 3, 0, -3, -2, -1)




# ------------------------------------------------------------------------------
# Mathematical mechanics of Toeplitz SSA
# Step 1:  Matrix decomposision
# ------------------------------------------------------------------------------


# ----------
# Step 1a:  trajectory matrix


X.traj <- matrix(c(x, x[2:6], 0, x[3:6], 0, 0), 6, byrow = FALSE)


nrow <- nrow(X.traj)

ncol <- ncol(X.traj)



# ----------
# Step 1b:  lagged covariance matrix

# variance
c11 <- sum(x^2) / length(x)


# co-variance between x and lagged vector one period removed
c12 <- sum((x * X.traj[,2])[1:(length(x)-1)]) / (length(x) - 1)


# co-variance between x and lagged vector two period removed
c13 <- sum((x * X.traj[,3])[1:(length(x)-2)]) / (length(x) - 2)



# lagged covariance matrix
cx <- matrix(c(c11, c12, c13, c12, c11, c12, c13, c12, c11), 3, byrow = T)




# ----------
# Step 1c:  Eigensystem of S


eigensys <- eigen(cx, symmetric = TRUE)

eigenvals <- eigensys$values

eigenvecs <- eigensys$vectors



left.1 <- X.traj %*% eigenvecs[,1] / sqrt(eigenvals[1])

left.2 <- X.traj %*% eigenvecs[,2] / sqrt(eigenvals[2])

left.3 <- X.traj %*% eigenvecs[,3] / sqrt(eigenvals[3])



# ----------
# Step 1d:  Matrices in decomposition

X1 <- sqrt(eigenvals[1]) * left.1 %*% t(eigenvecs[,1])

X2 <- sqrt(eigenvals[2]) * left.2 %*% t(eigenvecs[,2])

X3 <- sqrt(eigenvals[3]) * left.3 %*% t(eigenvecs[,3])



# ----------
# Step 1e:  Characteristics contributions of Xi

X1.c <- eigenvals[1] / sum(eigenvals)

X2.c <- eigenvals[2] / sum(eigenvals)

X3.c <- eigenvals[3] / sum(eigenvals)





# ------------------------------------------------------------------------------
# Mathematical mechanics of Toeplitz SSA
# Step 2:  Time series reconstruction (diagonal averaging)
# ------------------------------------------------------------------------------


# ----------
# Step 2a:  Averaging of minor diagonals


diag.ave <- function(mat){
  
  hold <- matrix(0, (nrow + (ncol - 1)))
  
  for(i in 1:(nrow + (ncol - 1))){
    
    if(i == 1) { d <- mat[1,1] }
    
    if(i > 1 & i <= ncol){ d <- diag(mat[i:1, 1:i]) }
    
    if(i > ncol & i <= nrow) { d <- diag(mat[i:(i - (ncol - 1)), 1:ncol]) }
    
    if(i > nrow & i < (nrow + (ncol - 1))){
      
      d <- diag(mat[nrow:(i - (ncol - 1)), (i - (nrow - 1)):ncol])
    }
    
    if(i == (nrow + (ncol - 1))) { d <- mat[nrow, ncol] }
    
    
    # average minor diagonals
    d.ave <- mean(d)
    
    hold[i,] <- d.ave
  }
  
  return(hold)
}




# ----------
# Step 2d:  Reconstructed time series

x1 <- diag.ave(X1)[1:length(x)]

x2 <- diag.ave(X2)[1:length(x)]

x3 <- diag.ave(X3)[1:length(x)]




x1

x2

x3
