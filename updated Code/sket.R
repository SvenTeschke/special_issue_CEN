



getCLS = function(XY){
  # If number of rows is less than number of columns,
  # need to transpose combination matrix, as this switches the roles of obs and variables,
  if(nrow(XY) < ncol(XY)) C = t(XY) else C = XY
  decomp = qr.Q(qr(C)) 
  
  CLS <- c() 
  # to save the matrix multiplication with Q we calculate only the scalar product of the 
  # corresponding rows.
  for(i in 1:(nrow(decomp)-1)){
    CLS[i] <- decomp[i, ] %*% decomp[nrow(decomp), ]
  }
  return(CLS)
} 

# Inv_or_MP: function to get inverse or pseudoinverse
#
# Input: R - Matrix
#
# Outout: Inverse or Pseudoinverse
Inv_or_MP <- function(R){
  tryCatch({
    solve(R)
  },
  error = function(e){
    MASS::ginv(R)
  }
  )
}


# sketch2b: to calculate the cls of a matrix X with sketching
# 
# Input: - X:  combined matrix of X and Y
#        - epsilon: parameter

# Output: - cls: cls of variable j with response y

sketching <- function(X, epsilon = 0.2, 
                     abs = T){
  p = ncol(X)
  n = nrow(X)
  X <- t(X)
  r = round((ceiling(n*log(n)))/(epsilon^2)) # Cohen
  s = round(ceiling(log(n)/epsilon^2))
  X_ <- matrix(0, nrow = r, ncol = n) 
  for(d in 1:p){
    f = sample(1:r, s)
    g = sample(c(-1,1), s, replace = T)
    for(l in 1:s){
      X_[f[l],] <- X_[f[l],] + X[d,] * g[l]}
  }
  R <- qr.R(qr(X_)) # R from QR decomp.
  R_inv <- Inv_or_MP(R)
  print('R_inv')
  
  Om <- X %*% R_inv
  cls <- c()
  for(i in 1:(p-1)){
    cls[i] <- Om[i, ] %*% Om[p,]
  }

return(Om_)
}
## geht doch auch streamwise??

