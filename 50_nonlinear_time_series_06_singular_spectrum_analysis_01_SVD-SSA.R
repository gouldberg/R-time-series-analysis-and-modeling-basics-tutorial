
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Mathematical mechanics of SVD-SSA (Singular value decomposision - singular Spectrum Analysis)
# ------------------------------------------------------------------------------


# sample time series
x <- c(1, 3, 0, -3, -2, -1)




# ------------------------------------------------------------------------------
# Mathematical mechanics of SVD-SSA (Singular value decomposision - singular Spectrum Analysis)
# Step 1:  Matrix decomposision
# ------------------------------------------------------------------------------


# ----------
# Step 1a:  trajectory matrix


library(tseriesChaos)


# embedded trajectory matrix
X <- embedd(x, 3, 1)


nrow <- nrow(X)

ncol <- ncol(X)



# ----------
# Step 1b:  lagged covariance matrix

S <- t(X) %*% X




# ----------
# Step 1c:  Eigensystem of S


eigensys <- eigen(S, symmetric = TRUE)

eigenvals <- eigensys$values

eigenvecs <- eigensys$vectors



left.1 <- X %*% eigenvecs[,1] / sqrt(eigenvals[1])

left.2 <- X %*% eigenvecs[,2] / sqrt(eigenvals[2])

left.3 <- X %*% eigenvecs[,3] / sqrt(eigenvals[3])



# ----------
# Step 1d:  Matrices in decomposition

X1 <- sqrt(eigenvals[1]) * left.1 %*% t(eigenvecs[,1])

X2 <- sqrt(eigenvals[2]) * left.2 %*% t(eigenvecs[,2])

X3 <- sqrt(eigenvals[3]) * left.3 %*% t(eigenvecs[,3])



# ----------
# Step 1e:  Characteristics contributions of Xi

X1.cc <- eigenvals[1] / sum(eigenvals)

X2.cc <- eigenvals[2] / sum(eigenvals)

X3.cc <- eigenvals[3] / sum(eigenvals)





# ------------------------------------------------------------------------------
# Mathematical mechanics of SVD-SSA (Singular value decomposision - singular Spectrum Analysis)
# Step 2:  Matrix grouping
# ------------------------------------------------------------------------------

X12 <- X1 + X2


X12.cc <- (eigenvals[1] + eigenvals[2]) / sum(eigenvals)




# ------------------------------------------------------------------------------
# Mathematical mechanics of SVD-SSA (Singular value decomposision - singular Spectrum Analysis)
# Step 3:  Time series reconstruction (diagonal averaging)
# ------------------------------------------------------------------------------


# ----------
# Step 3a:  Averaging of minor diagonals


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
# Step 3d:  Reconstructed time series

x1 <- diag.ave(X1)

x2 <- diag.ave(X2)

x3 <- diag.ave(X3)

x12 <- diag.ave(X12)



x1

x2

x3

x12



x12 + x3



