library('tidyverse')
library('ggplot2')
library('ggpubr') ## for ggrange

# Read the functions we need:
source('all_functions.R')

if (!dir.exists("results/4w")) {
  dir.create("results/4w", recursive = TRUE)
}

#### how many ####


##### p = 2k #####
load('results/4w/selected_4w_2k_not_abs.RData')
q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$whole[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$whole[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$whole[,j] == 'SNP4')) %>% sort()
  whole = rep(0,2000)
  whole[w1[1]:2000] <- 1
  whole[w1[2]:2000] <- 2
  whole[w1[3]:2000] <- 3
  whole[w1[4]:2000] <- 4
  
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$RW[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$RW[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  RW = rep(0,2000)
  RW[w1[1]:2000] <- 1
  RW[w1[2]:2000] <- 2
  RW[w1[3]:2000] <- 3
  RW[w1[4]:2000] <- 4
  
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$SW[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$SW[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  SW = rep(0,2000)
  SW[w1[1]:2000] <- 1
  SW[w1[2]:2000] <- 2
  SW[w1[3]:2000] <- 3
  SW[w1[4]:2000] <- 4
  
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$e_0.5[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$e_0.5[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  e_0.5 = rep(0,2000)
  e_0.5[w1[1]:2000] <- 1
  e_0.5[w1[2]:2000] <- 2
  e_0.5[w1[3]:2000] <- 3
  e_0.5[w1[4]:2000] <- 4
  
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$e_0.2[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$e_0.2[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  e_0.2 = rep(0,2000)
  e_0.2[w1[1]:2000] <- 1
  e_0.2[w1[2]:2000] <- 2
  e_0.2[w1[3]:2000] <- 3
  e_0.2[w1[4]:2000] <- 4
  
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$e_0.1[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$e_0.1[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  e_0.1 = rep(0,2000)
  e_0.1[w1[1]:2000] <- 1
  e_0.1[w1[2]:2000] <- 2
  e_0.1[w1[3]:2000] <- 3
  e_0.1[w1[4]:2000] <- 4
  
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_4w_2k_not_abs$corr[,j] == 'SNP2'),
         which(selected_4w_2k_not_abs$corr[,j] == 'SNP3'),
         which(selected_4w_2k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  corr = rep(0,2000)
  corr[w1[1]:2000] <- 1
  corr[w1[2]:2000] <- 2
  corr[w1[3]:2000] <- 3
  corr[w1[4]:2000] <- 4
  
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_2k_not_abs <- data.frame('q' = 4:1000,
                                 'whole' = colMeans(whole_)[-(1:3)],
                                 'RW' = colMeans(RW_)[-(1:3)],
                                 'SW' = colMeans(SW_)[-(1:3)],
                                 'corr' = colMeans(corr_)[-(1:3)],
                                 'e0.5' = colMeans(e_0.5_)[-(1:3)],
                                 'e0.2' = colMeans(e_0.2_)[-(1:3)],
                                 'e0.1' = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])




##### p = 20k #####
load('results/4w/selected_4w_20k_not_abs.RData')
q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$whole[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$whole[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$whole[,j] == 'SNP4')) %>% sort()
  whole = rep(0,10000)
  whole[w1[1]:10000] <- 1
  whole[w1[2]:10000] <- 2
  whole[w1[3]:10000] <- 3
  whole[w1[4]:10000] <- 4
  }, error=function(e){})
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}



# RW:
RW_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$RW[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$RW[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  RW = rep(0,10000)
  RW[w1[1]:10000] <- 1
  RW[w1[2]:10000] <- 2
  RW[w1[3]:10000] <- 3
  RW[w1[4]:10000] <- 4
  }, error=function(e){})
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$SW[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$SW[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  SW = rep(0,10000)
  SW[w1[1]:10000] <- 1
  SW[w1[2]:10000] <- 2
  SW[w1[3]:10000] <- 3
  SW[w1[4]:10000] <- 4
  }, error=function(e){})
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$e_0.5[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$e_0.5[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  e_0.5 = rep(0,10000)
  e_0.5[w1[1]:10000] <- 1
  e_0.5[w1[2]:10000] <- 2
  e_0.5[w1[3]:10000] <- 3
  e_0.5[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$e_0.2[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$e_0.2[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  e_0.2 = rep(0,10000)
  e_0.2[w1[1]:10000] <- 1
  e_0.2[w1[2]:10000] <- 2
  e_0.2[w1[3]:10000] <- 3
  e_0.2[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$e_0.1[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$e_0.1[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  e_0.1 = rep(0,10000)
  e_0.1[w1[1]:10000] <- 1
  e_0.1[w1[2]:10000] <- 2
  e_0.1[w1[3]:10000] <- 3
  e_0.1[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_4w_20k_not_abs$corr[,j] == 'SNP2'),
         which(selected_4w_20k_not_abs$corr[,j] == 'SNP3'),
         which(selected_4w_20k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  corr = rep(0,10000)
  corr[w1[1]:10000] <- 1
  corr[w1[2]:10000] <- 2
  corr[w1[3]:10000] <- 3
  corr[w1[4]:10000] <- 4
}, error=function(e){})
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_20k_not_abs <- data.frame('q' = 4:1000,
                                 'whole' = colMeans(whole_)[-(1:3)],
                                 'RW' = colMeans(RW_)[-(1:3)],
                                 'SW' = colMeans(SW_)[-(1:3)],
                                 'corr' = colMeans(corr_)[-(1:3)],
                                 'e0.5' = colMeans(e_0.5_)[-(1:3)],
                                 'e0.2' = colMeans(e_0.2_)[-(1:3)],
                                 'e0.1' = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])




##### p = 200k #####
load('results/4w/selected_4w_200k_not_abs.RData')

q = 10000
# whole:
whole_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$whole[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$whole[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$whole[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$whole[,j] == 'SNP4')) %>% sort()
    whole = rep(0,10000)
    whole[w1[1]:10000] <- 1
    whole[w1[2]:10000] <- 2
    whole[w1[3]:10000] <- 3
    whole[w1[4]:10000] <- 4
  }, error=function(e){})
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$RW[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$RW[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$RW[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$RW[,j] == 'SNP4')) %>% sort()
    RW = rep(0,10000)
    RW[w1[1]:10000] <- 1
    RW[w1[2]:10000] <- 2
    RW[w1[3]:10000] <- 3
    RW[w1[4]:10000] <- 4
  }, error=function(e){})
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$SW[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$SW[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$SW[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$SW[,j] == 'SNP4')) %>% sort()
    SW = rep(0,10000)
    SW[w1[1]:10000] <- 1
    SW[w1[2]:10000] <- 2
    SW[w1[3]:10000] <- 3
    SW[w1[4]:10000] <- 4
  }, error=function(e){})
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.5[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$e_0.5[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$e_0.5[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
    e_0.5 = rep(0,10000)
    e_0.5[w1[1]:10000] <- 1
    e_0.5[w1[2]:10000] <- 2
    e_0.5[w1[3]:10000] <- 3
    e_0.5[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.2[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$e_0.2[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$e_0.2[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
    e_0.2 = rep(0,10000)
    e_0.2[w1[1]:10000] <- 1
    e_0.2[w1[2]:10000] <- 2
    e_0.2[w1[3]:10000] <- 3
    e_0.2[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.1[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$e_0.1[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$e_0.1[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
    e_0.1 = rep(0,10000)
    e_0.1[w1[1]:10000] <- 1
    e_0.1[w1[2]:10000] <- 2
    e_0.1[w1[3]:10000] <- 3
    e_0.1[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$corr[,j] == 'SNP1'), 
           which(selected_4w_200k_not_abs$corr[,j] == 'SNP2'),
           which(selected_4w_200k_not_abs$corr[,j] == 'SNP3'),
           which(selected_4w_200k_not_abs$corr[,j] == 'SNP4')) %>% sort()
    corr = rep(0,10000)
    corr[w1[1]:10000] <- 1
    corr[w1[2]:10000] <- 2
    corr[w1[3]:10000] <- 3
    corr[w1[4]:10000] <- 4
  }, error=function(e){})
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_200k_not_abs <- data.frame('q' = 4:10000,
                                  'whole' = colMeans(whole_)[-(1:3)],
                                  'RW' = colMeans(RW_)[-(1:3)],
                                  'SW' = colMeans(SW_)[-(1:3)],
                                  'corr' = colMeans(corr_)[-(1:3)],
                                  'e0.5' = colMeans(e_0.5_)[-(1:3)],
                                  'e0.2' = colMeans(e_0.2_)[-(1:3)],
                                  'e0.1' = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])
