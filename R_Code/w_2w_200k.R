# 2w_200k
library(tidyverse)

# read in the functions we need:
source('/home/teschke/Promotion/Research/Cross_Lev/merging_CLS_paper/CLS_Paper/Code/neu/all_functions_neu.R')

# read in the data
load('/home/teschke/Promotion/Research/Cross_Lev/merging_CLS_paper/Data/neu/Szen2w_200k.RData')


set.seed(132)
#### influence or the parameter w in the RW and SW approach: ####
h <- c()
r <- c()
for(i in 1:100){
  h_ <- c()
  r_ <- c()
  for(j in c(200,500,1000,1250,2000,2500,4000,5000,10000, 20000)){
    xyz <- how_many2(Data = cbind(Szen2w_200k[[i]]$x, Szen2w_200k[[i]]$y), res_number = 200001,abs = F,
                     whole = F, SW=T, RW = T, R = 1000, w_RW = j, corr_ = F, sketch = F,
                     w_SW = j)
    q = 575
    h_ <- c(h_, sum(any(xyz$p_SW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_SW$SNP[1:q] == "SNP2")))
    r_ <- c(r_, sum(any(xyz$p_RW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_RW$SNP[1:q] == "SNP2")))}
  
  h <- rbind(h, h_)
  r <- rbind(r, r_)
  print(i)
save(h,r, file = "results/w_2w_200k.RData")

}




