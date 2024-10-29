# 2w2w_2k
library('tidyverse')

# Read in the functions we need:
source('all_functions.R')

if (!dir.exists("results/2w2w")) {
  dir.create("results/2w2w", recursive = TRUE)
}

# Read in the data:
load('../Data/Szen2w2w_2k.RData')

set.seed(1146) 
p_whole_ = c()
p_RW_ = c()
p_SW_ = c()
p_corr_ = c()
p_sketch_0.5_ = c()
p_sketch_0.2_ = c()
p_sketch_0.1_ = c()
for(i in 1:1000){
  xyz <- how_many2(Data = cbind(Szen2w2w_2k[[i]]$x, Szen2w2w_2k[[i]]$y), 
                   res_number = 2001,abs = F,
                   w_RW = 200, R = 200,
                   w_SW = 200,
                   eps = 0.5)
  xyz2 <- how_many2(Data = cbind(Szen2w2w_2k[[i]]$x, Szen2w2w_2k[[i]]$y), 
                    res_number = 2001,abs = F,
                    whole = F, RW = F, SW = F, corr_ = F, sketch = T, eps = 0.2)
  xyz3 <- how_many2(Data = cbind(Szen2w2w_2k[[i]]$x, Szen2w2w_2k[[i]]$y),
                    res_number = 2001,abs = F,
                    whole = F, RW = F, SW = F, corr_ = F, sketch = T, eps = 0.1)
  
  p_whole_ = cbind(p_whole_, xyz$p_whole$SNP, xyz$p_whole$Score)
  p_RW_ = cbind(p_RW_, xyz$p_RW$SNP, xyz$p_RW$Score)
  p_SW_ = cbind(p_SW_, xyz$p_SW$SNP, xyz$p_SW$Score)
  p_corr_ =cbind(p_corr_, xyz$p_corr$SNP, xyz$p_corr$Score)
  p_sketch_0.5_ = cbind(p_sketch_0.5_, xyz$p_sketch$SNP, 
                        xyz$p_sketch$Score)
  p_sketch_0.2_ = cbind(p_sketch_0.2_, xyz2$p_sketch$SNP, 
                        xyz2$p_sketch$Score)
  p_sketch_0.1_ = cbind(p_sketch_0.1_, xyz3$p_sketch$SNP, 
                        xyz3$p_sketch$Score)
  
  selected_2w2w_2k_not_abs = list('whole' = p_whole_,
                                   'RW' = p_RW_,
                                   'SW' = p_SW_,
                                   'corr' = p_corr_,
                                   'e_0.5' = p_sketch_0.5_,
                                   'e_0.2' = p_sketch_0.2_,
                                   'e_0.1' = p_sketch_0.1_)
  
  save(selected_2w2w_2k_not_abs, file = 'results/2w2w/selected_2w2w_2k_not_abs_neu.RData')
  
  print(i)
}