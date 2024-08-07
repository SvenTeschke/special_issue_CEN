# 2w2w_2000k
library('tidyverse')

# read in the functions we need
source('all_functions.R')


# read in data
load('../Data/Szen2w2w_2000k_90.RData')


set.seed(12039) 


p_whole_ = c()
p_RW_ = c()
p_SW_ = c()
p_corr_ = c()
p_sketch_0.5_ = c()
p_sketch_0.2_ = c()
p_sketch_0.1_ = c()
for(i in 1:10){
  xyz <- how_many2(Data = cbind(Szen2w2w_2000k[[i]]$x, Szen2w2w_2000k[[i]]$y),
                   res_number = 2000001,abs = F,
                   whole =F,
                   w_RW = 2000, R = 2000,
                   w_SW = 2000,
                   eps = 0.5)
  xyz2 <- how_many2(Data = cbind(Szen2w2w_2000k[[i]]$x, Szen2w2w_2000k[[i]]$y), 
                    res_number = 2000001,abs = F,
                    whole = F, RW = F, SW = F, corr_ = F, sketch = T, eps = 0.2)
  xyz3 <- how_many2(Data = cbind(Szen2w2w_2000k[[i]]$x, Szen2w2w_2000k[[i]]$y),
                    res_number = 2000001,abs = F,
                    whole = F, RW = F, SW = F, corr_ = F, sketch = T, eps = 0.1)
  
  # just consider the SNPs with the 10000 largest CLS
  #  p_whole_ = cbind(p_whole_, xyz$p_whole$SNP[1:10000], xyz$p_whole$Score[1:10000])
  p_RW_ = cbind(p_RW_, xyz$p_RW$SNP[1:10000], xyz$p_RW$Score[1:10000]) 
  p_SW_ = cbind(p_SW_, xyz$p_SW$SNP[1:10000], xyz$p_SW$Score[1:10000])
  p_corr_ =cbind(p_corr_, xyz$p_corr$SNP[1:10000], xyz$p_corr$Score[1:10000])
  p_sketch_0.5_ = cbind(p_sketch_0.5_, xyz$p_sketch$SNP[1:10000], 
                        xyz$p_sketch$Score[1:10000])
  p_sketch_0.2_ = cbind(p_sketch_0.2_, xyz2$p_sketch$SNP[1:10000], 
                        xyz2$p_sketch$Score[1:10000])
  p_sketch_0.1_ = cbind(p_sketch_0.1_, xyz3$p_sketch$SNP[1:10000], 
                        xyz3$p_sketch$Score[1:10000])
  
  selected_2w2w_2000k_not_abs_90 = list('whole' = NA,
                                   'RW' = p_RW_,
                                   'SW' = p_SW_,
                                   'corr' = p_corr_,
                                   'e_0.5' = p_sketch_0.5_,
                                   'e_0.2' = p_sketch_0.2_,
                                   'e_0.1' = p_sketch_0.1_)
  
  Szen2w2w_2000k[[i]] <- 'done'
  gc()
  save(selected_2w2w_2000k_not_abs_90, 
       file = 'results/selected_2w2w_2000k_not_abs_90.RData')
  print(i)
}