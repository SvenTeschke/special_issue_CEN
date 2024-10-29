### all functions ####


# getCLS: function to get the CLS (only the CLS of variable X_i with Y)
#               of a matrix X together with response y 
# 
# Input: - XY: cbind of matrix X with response y
#
# Output: 
#         - CLS of variable X_i and Y
#


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


# sketch2b: to calculate the cls of a matrix X with sketching
# 
# Input: - X:  combined matrix of X and Y
#        - epsilon: parameter
#        - vi: how many important variables (optional, but must be specified)
#        - abs: abs or no abs cls?
#
# Output: - cls: cls of variable j with response y
#         - rank: the rank of the important variables by cls

sketch2b <- function(X, epsilon = 0.2, vi = 2, 
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
  print('X_')
  rm(f)  # we remove things we do not need in the following steps to save memory 
  rm(g)
  gc()
  print('del f,g')
  R <- qr.R(qr(X_)) # R from QR decomp.
  R_inv <- Inv_or_MP(R)
  print('R_inv')
  
  Om <- X %*% R_inv
  print('om')
  Om_ <- c()
  for(i in 1:(p-1)){
    Om_[i] <- Om[i, ] %*% Om[p,]
  }
  if(abs == T){
    Om_ = abs(Om_)
  }
  CLS_rank <- rank(-Om_)[1:vi]
  
  return(list('cls' = Om_,
              'rank' = CLS_rank))
}



# randomCLS_general_CLS_sample: calculate cls with RW approach
#
# Input: - Data: Data matrix X
#        - response_: response Y
#        - w: window size
#        - R: max number of samples
#        - bagging: bagging step in each sample (default: F)
#        - abs_cls: abs or no abs cls
#        - pr: print iterations?
#
# Output: - cls: dataframe with all variables and their cls (if they are chosen at 
#               least once)


randomCLS_general_CLS_sample <- function(Data, response_, w = 0, R = 100,
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
    
    
    cls$Score[s] = apply(cls[s,2:3], 1, function(x) max(x, na.rm = T))

    cls <- cls[,-3]
    
  }  
  
  return(cls)
  
}


# Sliding_general_CLS_sampling: calculate cls with SW approach
#
# Input: - Data: Data matrix X
#        - response_: response Y
#        - w: window size
#        - bagging: bagging step in each sample (default: F)
#        - abs_cls: abs or no abs cls
#        - samp_: should the columns sampled before start?
#
# Output: - cls: dataframe with all variables and their cls 




Sliding_general_CLS_sampling <- function(Data, response_, w = 0,
                                            bagging = F, abs_cls = T,
                                            samp_ = F, pr = T){
  n = nrow(Data)
  p = ncol(Data)
  
  # window width:
  if(w==0){
    w = n+1
  } else{w = w}
  
  s = w # steps forward -> no overlapping
  
  
  if(samp_ == T){
    print('sample')
    Data <- Data[, sample(1:p)]
  } else{Data = Data
  print('normal')}
  
  SNPs = colnames(Data)
  cls = data.frame('SNP' = rep(NA, p), 'Score' = rep(NA, p))
  
  fin <- w  # end of the first window 
  mi <- 1   # begin of the first window
  ma <- fin  # end of window
  # start with loop:
  
  while(fin < (p+(s/2))){ 
    if(pr == T){
    print(c(mi,fin))} 
    mi <- mi
    ma <- fin
    
    # BAGGING yes or no?
    if(bagging == T){
      n_ = sample(1:n, n, replace = T)
    } else{n_ = 1:n}
    
    
    # construct window, contains of w SNPs and response y
    XY_ = cbind(Data[n_ ,mi:ma], response_[n_])
    # calc all scores for the subset:
    levs_ = getCLS(XY_)    
    
    # absolute cls?
    if(abs_cls == T){
      levs_ <- abs(levs_)
    } else{levs_ <- levs_}
    
    
    cls$Score[mi:ma] <- levs_    
    cls$SNP[mi:ma] <- SNPs[mi:ma]
    
    # updating window indices:
    fin <- fin + s
    mi <- mi + s
    }
  return(cls)
}





# how_many2: calculate cls with all methods:
#
# Input: - Data: combinded data of X and Y
#        - res_number: which column is Y?
#        - abs: abs cls or not?
#        - whole: calc cls of the whole matrix?
#        - RW: calc CLS with RW approach?
#        - w_RW, R, pr_RW: parameter of RW approach -> see above
#        - SW: calc CLS with SW approach?
#        - w_SW: window size for SW approach
#        - corr: calc corr?
#        - sketch: calc CLS with sketching?
#        - eps: eps in sketching approach
#
# Output: - list of cls calculated by the different approaches and corr


# 2w
how_many2 <- function(Data, res_number = 2001, abs =T,
                          #whole:
                          whole = T,
                          #RW:
                          RW = T, w_RW = 200, R = 200, pr_RW = T,
                          #SW:
                          SW = T, w_SW = 200, pr_SW = T,
                          #corr:
                          corr_ = T, pr_corr = T,
                          # sketch:
                          sketch = T, eps = 0.5){
  
  
  
  # whole cls:
  if(whole == T){
    CCC <- getCLS(Data)
    
    if(abs == T){
      CCC = abs(CCC)
      print('abs')} else{print('not abs')}
    
    CCC = data.frame('SNP' = paste0('SNP', 1:length(CCC)), 'Score' = CCC)
    CCC = arrange(CCC, by = -CCC$Score)}else{CCC = NA}
  
  print('whole done')
  # RW:
  if(RW ==T){
    CCC_RW = randomCLS_general_CLS_sample(Data[,-res_number], Data[,res_number], 
                                          w = w_RW, R = R,
                                          bagging = F, abs_cls = abs, pr = pr_RW)

   CCC_RW = arrange(CCC_RW, by = -CCC_RW$Score)}else{CCC_RW = NA}

  print('RW done')
  # SW:
  if(SW == T){
    CCC_SW = Sliding_general_CLS_sampling(Data[,-res_number], Data[,res_number], w=w_SW, 
                                          abs_cls = abs,
                                          pr = pr_SW)

    CCC_SW = arrange(CCC_SW, by = -CCC_SW$Score)}else{CCC_SW = NA}

  print('SW done')

 # correlation:
 if(corr_ == T){
   
   # direkt:
   res = as.numeric(Data[,res_number])
   corr_d <- apply(Data[,-res_number], 2, function(x) cor(x, res)) 
   if(abs == T){
     corr_d = abs(corr_d)
     print('corr abs')
   }
   corr_d <- data.frame('SNP' = names(corr_d), 'Score' = as.numeric(corr_d))
   corr_d <- arrange(corr_d, by = -corr_d$Score)}else{corr_d = NA}

  print('corr done')

# sketch:
  if(sketch == T){
  CCC_sketch = sketch2b(Data, epsilon = eps, abs = abs, vi = 2)
  CCC_sketch = data.frame('SNP' = paste0('SNP', 1:length(CCC_sketch$cls)), 
                          'Score' = CCC_sketch$cls)
  CCC_sketch = arrange(CCC_sketch, by = -CCC_sketch$Score)}else{CCC_sketch = NA}
  
  
  
  
  return(list('p_whole' = CCC,
              'p_RW' = CCC_RW,
              'p_SW' = CCC_SW,
              'p_corr' = corr_d,
              'p_sketch' = CCC_sketch))
  
}



###############################  further updated versions: ####
# not used in manusscript, but improved coding. nevertheless these functions
# lead to the same results as the above functions
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