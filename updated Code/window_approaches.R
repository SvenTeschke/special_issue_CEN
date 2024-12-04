# Input: - XY: cbind of matrix X with response y
#
# Output: 
#         - CLS of variable X_i and Y
#


getCLS = function(XY){
  if(nrow(XY) < ncol(XY)) C = t(XY) else C = XY
  decomp = qr.Q(qr(C)) 
  
  CLS <- c() 
  
  for(i in 1:(nrow(decomp)-1)){
    CLS[i] <- decomp[i, ] %*% decomp[nrow(decomp), ]
  }
  return(CLS)
} 


# Inv_or_MP: function to get inverse or pseudo inverse
#
# Input: R - Matrix
#
# Output: Inverse or Pseudo inverse
Inv_or_MP <- function(R){
  tryCatch({
    solve(R)
  },
  error = function(e){
    MASS::ginv(R)
  }
  )
}


# vnorm: to calculate the Norm of a vector
#
# Input: x - vecor
#
# Output: the norm of the input vector x
vnorm <- function(x){
  vn <- sqrt(sum(x^2))
  return(vn)
}



RW_CLS <- function(Data, response_, w = 0, R = 100,
                   bagging = F, abs_cls = T, pr = T){
  n = nrow(Data)
  p = ncol(Data)
  
  # window width:
  if(w==0){
    w = n+1
  } else{w = w}
  
  A <- numeric(0)  # control if we catch all SNPs
  # loop:
  SNPs = colnames(Data)
  cls = data.frame('SNP' = rep(NA, p), 'Score' = rep(NA, p))
  
  i = 0
  while (length(A) < p & i < R) { # break if R is reached or all variables are chosen 
    # at least once
    
    s = sample(1:p, w)  # sample w SNPs in each step independently
    A <- c(A, s) %>% unique() # count how many different SNPs we consider
    
    if(pr == T){i = i+1
    print(i)
    pp = paste((100*length(A))/p, '%')
    print(pp)}
    
    # bagging yes or no:
    if(bagging == TRUE){
      n_ <- sample(1:n, n, TRUE)
    } else{n_ = 1:n}
    
    # construct window, contains of w SNPs and response y:
    XY_ = cbind(Data[n_ ,s], response_[n_]) 
    # calc all scores for the subset:
    levs_ = getCLS(XY_) 
    
    if(abs_cls == T){ # abs or not?
      levs_ <- abs(levs_)
    } else{levs_ <- levs_}
    
    cls <- cbind(cls, 0)
    cls[s,3] <- levs_
    cls$SNP[s] <- SNPs[s]
    
    
    cls$Score[s] = apply(cls[s,2:3], 1, function(x) min(x, na.rm = T))
    
    cls <- cls[,-3]
    
  }  
  
  return(cls)
  
}

########################################################################################
SW_CLS <- function(Data, response_, w = 0,
                   abs_cls = T){
  n = nrow(Data)
  p = ncol(Data)
  
  # window width:
  if(w<n){
    w = n+1
  } else{w = w}
  
  SNPs <- colnames(Data)
  cls <- data.frame('SNP' = rep(NA, p), 'Score' = rep(NA, p))
  
  fin <- w  # end of the first window 
  mi <- 1   # begin of the first window
  ma <- fin  # end of window
  # start with loop:
  
  while(fin < (p+1)){ 
    
    print(c(mi,fin)) 
    mi <- mi
    ma <- fin
    
    # construct window, contains of w SNPs and response y
    XY_ = cbind(Data[ ,mi:ma], response_)
    # calc all scores for the subset:
    levs_ = getCLS(XY_)    
    
    # absolute cls?
    if(abs_cls == T){
      levs_ <- abs(levs_)
    } else{levs_ <- levs_}
    
    
    cls$Score[mi:ma] <- levs_    
    cls$SNP[mi:ma] <- SNPs[mi:ma]
    
    # updating window indices:
    fin <- fin + w
    mi <- mi + w
  }
  
  if(fin != (p+w)){ # if it does not fit, calculate the last window in the same size as 
    # the previous ones
    
    mi = p-w+1
    ma = p
    
    print(c(mi,ma)) 
    
    # construct window, contains of w SNPs and response y
    XY_ = cbind(Data[ ,mi:ma], response_)
    # calc all scores for the subset:
    levs_ = getCLS(XY_) 
    
    
    # absolute cls?
    if(abs_cls == T){
      levs_ <- abs(levs_)
    } else{levs_ <- levs_}
    
    
    cls$Score[mi:ma] <- levs_    
    cls$SNP[mi:ma] <- SNPs[mi:ma]
    
    
  }else{print('Geht auf')}
  
  
  return(cls)
}