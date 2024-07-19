# special_issue_CEN

PC
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
[3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.utf8    

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] logicDT_1.0.3        randomForest_4.7-1.1 GLDEX_2.0.0.9.3     
 [4] spacefillr_0.3.2     cluster_2.1.4        SNPassoc_2.1-0      
 [7] logicFS_2.20.0       mcbiopi_1.1.6        LogicReg_1.6.6      
[10] survival_3.5-7       ranger_0.16.0        ggpubr_0.6.0        
[13] scrime_1.3.5         lubridate_1.9.3      forcats_1.0.0       
[16] stringr_1.5.0        dplyr_1.1.3          purrr_1.0.2         
[19] readr_2.1.4          tidyr_1.3.0          tibble_3.2.1        
[22] ggplot2_3.4.4        tidyverse_2.0.0     

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0   fastmap_1.1.1      TH.data_1.1-2      digest_0.6.33     
 [5] rpart_4.1.21       timechange_0.2.0   lifecycle_1.0.4    magrittr_2.0.3    
 [9] compiler_4.3.2     rlang_1.1.2        Hmisc_5.1-1        tools_4.3.2       
[13] utf8_1.2.4         data.table_1.14.8  knitr_1.45         ggsignif_0.6.4    
[17] htmlwidgets_1.6.2  plyr_1.8.9         arsenal_3.6.3      abind_1.4-5       
[21] multcomp_1.4-25    polspline_1.1.24   withr_2.5.2        foreign_0.8-85    
[25] nnet_7.3-19        grid_4.3.2         fansi_1.0.5        poisbinom_1.0.1   
[29] colorspace_2.1-0   iterators_1.0.14   scales_1.3.0       MASS_7.3-60       
[33] haplo.stats_1.9.3  cli_3.6.1          mvtnorm_1.2-3      rmarkdown_2.25    
[37] rms_6.7-1          generics_0.1.3     rstudioapi_0.15.0  tzdb_0.4.0        
[41] splines_4.3.2      parallel_4.3.2     base64enc_0.1-3    vctrs_0.6.4       
[45] glmnet_4.1-8       Matrix_1.6-1.1     sandwich_3.0-2     carData_3.0-5     
[49] SparseM_1.81       car_3.1-2          hms_1.1.3          rstatix_0.7.2     
[53] Formula_1.2-5      htmlTable_2.4.2    foreach_1.5.2      glue_1.6.2        
[57] codetools_0.2-19   shape_1.4.6        stringi_1.7.12     gtable_0.3.4      
[61] munsell_0.5.0      pillar_1.9.0       htmltools_0.5.7    quantreg_5.97     
[65] R6_2.5.1           evaluate_0.23      lattice_0.21-9     backports_1.4.1   
[69] broom_1.0.5        MatrixModels_0.5-3 Rcpp_1.0.11        gridExtra_2.3     
[73] nlme_3.1-163       checkmate_2.3.0    xfun_0.41          zoo_1.8-12        
[77] pkgconfig_2.0.3


sever:
R version 4.3.3 (2024-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 12 (bookworm)

# Here the complete R Code according to the paper is stored

- simulated RData data is not included due to memory capacity and must be run first or downloaded 
  from Zenodo and save it at "'../Data/"


# Descriptions:

all_functions_neu - File with all functions i used

simulation_scenarios - file with the simulation of all scenarios 
   --> this takes long time and a lot of memory so we recommmend to download the simulated Data from Zenodo or 
       go directly to the conclusion section

2w_xxx - Evaluation of scenario 1 (one 2-way interaction), for p={2000, 20000, 200000, 2000000}; we splittet the analysis for
         p=2000000 due to memory reasons

2w2w_xxx - Evaluation of scenario 2 (two 2-way interactions), for p={2000, 20000, 200000, 2000000}; we splittet the analysis for
           p=2000000 due to memory reasons

3w_xxx - Evaluation of scenario 3 (one 3-way interactions), for p={2000, 20000, 200000}

4w_xxx - Evaluation of scenario 4 (one 4-way interactions), for p={2000, 20000, 200000}

   --> these calculations takes a lot aof time. We therefore recommend downloading the results.zip file from Zenodo, unpacking it and saving the whole file in the current working directory

conclusion_2w - Results of above Calculations of scenario 1 -> here calcultaions for Figure 1,2,4,7 and for Table 3 are done

conclusion_2w2w - Results of above Calculations of scenario 2 -> here calcultaions for Figure 5,10,11,12 and for Table 4 are done

conclusion_3w - Results of above Calculations of scenario 3 

conclusion_4w - Results of above Calculations of scenario 4 

hapmap - Evaluation of the hap map data with random Forests with focus on prediction performance  -> here calculations for Table 5

2w500_logicDT - Evaluation of scenario 1 with p=500, with logicDT and focus on variable importance and prediction performance -> here calculations for Figure 06
     
toyexample - Rcode for the toyexample in the motivation section -> here calculations for Figure 08

paramtertuning_w - consider the influence or the parameter w in the RW and SW approach -> here calculations for Figure 14

