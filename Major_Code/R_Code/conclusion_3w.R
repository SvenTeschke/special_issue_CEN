library('tidyverse')
library('ggplot2')
library('ggpubr') ## for ggrange

# Read the funtions we need:
source('all_functions.R')

if (!dir.exists("results/3w")) {
  dir.create("results/3w", recursive = TRUE)
}

#### how many ####

#### 2k: ####
load('results/3w/selected_3w_2k_not_abs.RData')

# table out of 3:
q = 575
how_whole = c()
for(i in seq(1,1999,2)){
  how_whole = c(how_whole, sum(any(selected_3w_2k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_3w_2k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_3w_2k_not_abs$whole[,i][1:q] == 'SNP3')))
  print(i)
}
how_whole %>% median()
how_whole %>% mean()

how_RW = c()
for(i in seq(1,1999,2)){
  how_RW = c(how_RW, sum(any(selected_3w_2k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_2k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_3w_2k_not_abs$RW[,i][1:q] == 'SNP3')))
  print(i)
}
how_RW %>% median()
how_RW %>% mean()

how_SW = c()
for(i in seq(1,1999,2)){
  how_SW = c(how_SW, sum(any(selected_3w_2k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_2k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_3w_2k_not_abs$SW[,i][1:q] == 'SNP3')))
  print(i)
}
how_SW %>% median()
how_SW %>% mean()

how_corr = c()
for(i in seq(1,1999,2)){
  how_corr = c(how_corr, sum(any(selected_3w_2k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_3w_2k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_3w_2k_not_abs$corr[,i][1:q] == 'SNP3')))
  print(i)
}
how_corr %>% median()
how_corr %>% mean()

how_e_0.5 = c()
for(i in seq(1,1999,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_3w_2k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_3w_2k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_3w_2k_not_abs$e_0.5[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.5 %>% median()
how_e_0.5 %>% mean()

how_e_0.2 = c()
for(i in seq(1,1999,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_3w_2k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_3w_2k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_3w_2k_not_abs$e_0.2[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.2 %>% median()
how_e_0.2 %>% mean()

how_e_0.1 = c()
for(i in seq(1,1999,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_3w_2k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_3w_2k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_3w_2k_not_abs$e_0.1[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.1 %>% median()
how_e_0.1 %>% mean()




# how many for different q:
q = 1000
# whole:
whole1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$whole[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$whole[,j] == 'SNP3')) %>% sort()
  whole1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
             rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  whole1_ = rbind(whole1_, whole1)
  
  print(j)
}

# RW:
RW1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$RW[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$RW[,j] == 'SNP3')) %>% sort()
  RW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
             rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  RW1_ = rbind(RW1_, RW1)
  
  print(j)
}


# SW:
SW1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$SW[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$SW[,j] == 'SNP3')) %>% sort()
  SW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
          rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  SW1_ = rbind(SW1_, SW1)
  
  print(j)
}


# corr:
corr1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$corr[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$corr[,j] == 'SNP3')) %>% sort()
  corr1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
          rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  corr1_ = rbind(corr1_, corr1)
  
  print(j)
}


# e_0.51:
e_0.51_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$e_0.5[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$e_0.5[,j] == 'SNP3')) %>% sort()
  e_0.51 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
            rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  e_0.51_ = rbind(e_0.51_, e_0.51)
  
  print(j)
}


# e_0.21:
e_0.21_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$e_0.2[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$e_0.2[,j] == 'SNP3')) %>% sort()
  e_0.21 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
             rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  e_0.21_ = rbind(e_0.21_, e_0.21)
  
  print(j)
}


# e_0.11:
e_0.11_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_2k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_3w_2k_not_abs$e_0.1[,j] == 'SNP2'),
         which(selected_3w_2k_not_abs$e_0.1[,j] == 'SNP3')) %>% sort()
  e_0.11 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0, w1[3]-w1[2])),
             rep(3, max(0,(q-w1[3]+1))))[1:q]
  
  e_0.11_ = rbind(e_0.11_, e_0.11)
  
  print(j)
}



mean_3w_2k_not_abs <-data.frame('q' = 3:1000,
                                'whole' = colMeans(whole1_)[-c(1:2)],
                                'RW' = colMeans(RW1_)[-c(1:2)],
                                'SW' = colMeans(SW1_)[-c(1:2)],
                                'corr' = colMeans(corr1_)[-c(1:2)],
                                'e0.5' = colMeans(e_0.51_)[-c(1:2)],
                                'e0.2' = colMeans(e_0.21_)[-c(1:2)],
                                'e0.1' = colMeans(e_0.11_)[-c(1:2)])



#### 20k: ####
load('results/3w/selected_3w_20k_not_abs.RData')

# table out of 3:
q = 575
how_whole = c()
for(i in seq(1,1999,2)){
  how_whole = c(how_whole, sum(any(selected_3w_20k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_3w_20k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_3w_20k_not_abs$whole[,i][1:q] == 'SNP3')))
  print(i)
}
how_whole %>% median()
how_whole %>% mean()

how_RW = c()
for(i in seq(1,1999,2)){
  how_RW = c(how_RW, sum(any(selected_3w_20k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_20k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_3w_20k_not_abs$RW[,i][1:q] == 'SNP3')))
  print(i)
}
how_RW %>% median()
how_RW %>% mean()

how_SW = c()
for(i in seq(1,1999,2)){
  how_SW = c(how_SW, sum(any(selected_3w_20k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_20k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_3w_20k_not_abs$SW[,i][1:q] == 'SNP3')))
  print(i)
}
how_SW %>% median()
how_SW %>% mean()

how_corr = c()
for(i in seq(1,1999,2)){
  how_corr = c(how_corr, sum(any(selected_3w_20k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_3w_20k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_3w_20k_not_abs$corr[,i][1:q] == 'SNP3')))
  print(i)
}
how_corr %>% median()
how_corr %>% mean()

how_e_0.5 = c()
for(i in seq(1,1999,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_3w_20k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_3w_20k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_3w_20k_not_abs$e_0.5[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.5 %>% median()
how_e_0.5 %>% mean()

how_e_0.2 = c()
for(i in seq(1,1999,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_3w_20k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_3w_20k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_3w_20k_not_abs$e_0.2[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.2 %>% median()
how_e_0.2 %>% mean()

how_e_0.1 = c()
for(i in seq(1,1999,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_3w_20k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_3w_20k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_3w_20k_not_abs$e_0.1[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.1 %>% median()
how_e_0.1 %>% mean()


# how many for different q:
q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  w1 = c(which(selected_3w_20k_not_abs$whole[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$whole[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$whole[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    whole = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
            rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){whole = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){whole = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{whole = rep(0,q)}}}
  whole_ = rbind(whole_, whole)
  
  print(j)
}
whole_ %>% colMeans()

# RW:
RW_ = c()
for(j in seq(1,1999,2)){

  w1 = c(which(selected_3w_20k_not_abs$RW[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$RW[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$RW[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    RW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
            rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){RW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){RW = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{RW = rep(0,q)}}}
  RW_ = rbind(RW_, RW)

  print(j)
}
RW_ %>% colMeans()


# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_20k_not_abs$SW[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$SW[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$SW[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    SW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
           rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){SW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){SW = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{SW = rep(0,q)}}}
  SW_ = rbind(SW_, SW)
  
  print(j)
}
SW_ %>% colMeans()


# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_20k_not_abs$corr[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$corr[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$corr[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    corr = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
           rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){corr = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){corr = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{corr = rep(0,q)}}}
  corr_ = rbind(corr_, corr)
  
  print(j)
}
corr_ %>% colMeans()

# e_0.5:
e_0.5_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_20k_not_abs$e_0.5[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$e_0.5[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$e_0.5[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    e_0.5 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
             rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){e_0.5 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){e_0.5 = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{e_0.5 = rep(0,q)}}}
  e_0.5_ = rbind(e_0.5_, e_0.5)
  
  print(j)
}
e_0.5_ %>% colMeans()


# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_20k_not_abs$e_0.2[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$e_0.2[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$e_0.2[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    e_0.2 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
             rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){e_0.2 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){e_0.2 = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{e_0.2 = rep(0,q)}}}
  e_0.2_ = rbind(e_0.2_, e_0.2)
  
  print(j)
}
e_0.2_ %>% colMeans()



# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_3w_20k_not_abs$e_0.1[,j] == 'SNP1'),
         which(selected_3w_20k_not_abs$e_0.1[,j] == 'SNP2'),
         which(selected_3w_20k_not_abs$e_0.1[,j] == 'SNP3')) %>% sort()
  if(length(w1) == 3){
    e_0.1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, w1[3]-w1[2]),
             rep(3, max(0,(q-w1[3]+1))))[1:q]}
  else{if(length(w1) == 2){e_0.1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]}
    else{if(length(w1) == 1){e_0.1 = c(rep(0, w1[1]-1), rep(1, max(0,q-w1[1])))[1:q]}
      else{e_0.1 = rep(0,q)}}}
  e_0.1_ = rbind(e_0.1_, e_0.1)
  
  print(j)
}
e_0.1_ %>% colMeans()




mean_3w_20k_not_abs <- data.frame('q' = 3:1000,
                                 'whole' = colMeans(whole_)[-c(1:2)],
                                 'RW' = colMeans(RW_)[-c(1:2)],
                                 'SW' = colMeans(SW_)[-c(1:2)],
                                 'corr' = colMeans(corr_)[-c(1:2)],
                                 'e0.5' = colMeans(e_0.5_)[-c(1:2)],
                                 'e0.2' = colMeans(e_0.2_)[-c(1:2)],
                                 'e0.1' = colMeans(e_0.1_)[-c(1:2)])



#### 200k: ####
load('results/3w/selected_3w_200k_not_abs.RData')

q = 575
how_whole = c()
for(i in seq(1,199,2)){
  how_whole = c(how_whole, sum(any(selected_3w_200k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_3w_200k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_3w_200k_not_abs$whole[,i][1:q] == 'SNP3')))
  print(i)
}
how_whole %>% median()
how_whole %>% mean()

how_RW = c()
for(i in seq(1,199,2)){
  how_RW = c(how_RW, sum(any(selected_3w_200k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_200k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_3w_200k_not_abs$RW[,i][1:q] == 'SNP3')))
  print(i)
}
how_RW %>% median()
how_RW %>% mean()

how_SW = c()
for(i in seq(1,199,2)){
  how_SW = c(how_SW, sum(any(selected_3w_200k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_3w_200k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_3w_200k_not_abs$SW[,i][1:q] == 'SNP3')))
  print(i)
}
how_SW %>% median()
how_SW %>% mean()

how_corr = c()
for(i in seq(1,199,2)){
  how_corr = c(how_corr, sum(any(selected_3w_200k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_3w_200k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_3w_200k_not_abs$corr[,i][1:q] == 'SNP3')))
  print(i)
}
how_corr %>% median()
how_corr %>% mean()

how_e_0.5 = c()
for(i in seq(1,199,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_3w_200k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_3w_200k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_3w_200k_not_abs$e_0.5[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.5 %>% median()
how_e_0.5 %>% mean()

how_e_0.2 = c()
for(i in seq(1,199,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_3w_200k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_3w_200k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_3w_200k_not_abs$e_0.2[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.2 %>% median()
how_e_0.2 %>% mean()

how_e_0.1 = c()
for(i in seq(1,199,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_3w_200k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_3w_200k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_3w_200k_not_abs$e_0.1[,i][1:q] == 'SNP3')))
  print(i)
}
how_e_0.1 %>% median()
how_e_0.1 %>% mean()

