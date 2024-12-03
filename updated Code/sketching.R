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

sketching <- function(X, epsilon = 0.5){
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
  
  Om <- X %*% R_inv
  cls <- c()
  for(i in 1:(p-1)){
    cls[i] <- Om[i, ] %*% Om[p,]
  }

return(cls)
}
## geht doch auch streamwise??




sketching_fast <- function(X, epsilon = 0.5){
  p = ncol(X)
  n = nrow(X)
  X <- t(X)
  r = round((ceiling(n * log(n))) / (epsilon^2)) # Cohen
  s = round(ceiling(log(n) / epsilon^2))
  X_ <- matrix(0, nrow = r, ncol = n)
  
  for(d in 1:p){
    f = sample(1:r, s, replace = TRUE)        
    g = sample(c(-1, 1), s, replace = TRUE) 
    

    X_[f, ] <- X_[f, ] + t(X[d, ] %o% g)
    
  }
  
  
  R <- qr.R(qr(X_)) # R from QR decomp.
  R_inv <- Inv_or_MP(R)
  
  Om <- X %*% R_inv
  cls <- Om[1:(p-1), ] %*% Om[p, ]
  return(cls)

}







# Beispiel-Daten
r <- 5   # Anzahl der Zeilen in der Matrix
n <- 4   # Anzahl der Spalten in der Matrix
X_ <- matrix(0, nrow = r, ncol = n)  # Initialisiere Matrix
row_to_add <- c(1, 2, 3, 4)         # Vektor, der hinzugefügt werden soll
f <- c(1, 3, 4)                     # Zeilenindizes, zu denen addiert werden soll

# Outer verwenden, um eine Gewichtungsmatrix zu erzeugen
add_matrix <- outer(1:r, f, function(i, j) i == j)  # 1, wenn Zeile i in f
add_matrix <- add_matrix %*% row_to_add            # Skalierung nach Spalten

# Addiere zur Matrix
X_ <- X_ + add_matrix
