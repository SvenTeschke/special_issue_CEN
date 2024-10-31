# special_issue_CEN


# Order Structure:

Data/  - This is where the simulated data is stored. If you download it from Zenodo, you have to save it there.
          If you run the Code of the simulation studies, the data is automatically saved there.

R_Code/  -  This is where the full R code for the paper is stored.

R_Code/plots - This is where the Figures are stored according to the paper.

R_Code/results - All results and intermediate results are stored here. If you don't want to run all the code and 
                 want to use the (intermediate) results, you have to download the results.zip file from 
                 Zenodo (https://doi.org/10.5281/zenodo.12742957) and unzip it into R_Code/ 
                 (Only) then can you delete the R_Code/results folder.



# Descriptions of the Code files:

The simulated RData data is not included due to memory capacity and must be run first or downloaded 
  from Zenodo https://doi.org/10.5281/zenodo.12742957

- masterscript.R - This file can be used to perform all calculations and analyses at once. 
                   If you only want to view single calculations, I recommend viewing the corresponding files separately.

- all_functions.R - File with all functions i used

- simulation_scenarios.R - file with the simulation of all scenarios 
                            --> This takes long time and uses a lot of memory, so I recommend downloading the simulated data from Zenodo or 
                                skip directly to the conclusion section

- 2w_xxx.R - Evaluation of scenario 1 (one 2-way interaction), for p={2000, 20000, 200000, 2000000}; we split the analysis for
         p=2000000 due to memory reasons
         --> these calculations takes a lot of time. We therefore recommend downloading the results.zip file from Zenodo, 
          unpacking it and saving the whole file in the current working directory

- 2w2w_xxx.R - Evaluation of scenario 2 (two 2-way interactions), for p={2000, 20000, 200000, 2000000}; we split the analysis for
           p=2000000 due to memory reasons
           --> these calculations takes a lot of time. We therefore recommend downloading the results.zip file from Zenodo, 
          unpacking it and saving the whole file in the current working directory

- 3w_xxx.R - Evaluation of scenario 3 (one 3-way interactions), for p={2000, 20000, 200000}
             --> these calculations takes a lot of time. We therefore recommend downloading the results.zip file from Zenodo, 
          unpacking it and saving the whole file in the current working directory

- 4w_xxx.R - Evaluation of scenario 4 (one 4-way interactions), for p={2000, 20000, 200000}
      --> these calculations takes a lot of time. We therefore recommend downloading the results.zip file from Zenodo, 
          unpacking it and saving the whole file in the current working directory

- conclusion_2w.R - The results of the above calculations for Scenario 1 -> here calculations for Figure 1,2,4,7 and for Table 3 are done

- conclusion_2w2w.R - The results of the above calculations for Scenario 2 -> here calculations for Figure 5,10,11,12 and for Table 4 are done

- conclusion_3w.R - The results of the above calculations for Scenario 3 

- conclusion_4w.R - The results of the above calculations for Scenario 4 

- hapmap.R - Evaluation of the hap map data with random Forests with focus on prediction performance  -> here calculations for Table 5

- 2w500_logicDT.R - Evaluation of scenario 1 with p=500, with logicDT and focus on variable 
                    importance and prediction performance -> here calculations for Figure 06 and Table 06
     
- toyexample.R - Code for the toyexample in the motivation section -> here calculations for Figure 08

- paramtertuning_w.R -Consider the influence on parameter w in the RW and SW approach and R in the RW approach -> here calculations for Figure 14 and Table 02


# sessionInfo():

Some of the calculations were carried out on the authors own PC, others on the server of the Faculty of Statistics at TU Dortmund University. 
In addition, the R version was updated over time. None of these points have any impact on the results, but it should be mentioned here.

## PC:
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


## sever:
R version 4.3.3 (2024-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 12 (bookworm)

Matrix products: default
BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.21.so;  
        LAPACK version 3.11.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

time zone: Europe/Berlin
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] GLDEX_2.0.0.9.3      spacefillr_0.3.3     cluster_2.1.6
 [4] SNPassoc_2.1-0       lubridate_1.9.3      forcats_1.0.0
 [7] stringr_1.5.1        dplyr_1.1.4          purrr_1.0.2
[10] readr_2.1.5          tidyr_1.3.1          tibble_3.2.1
[13] tidyverse_2.0.0      scrime_1.3.5         ggplot2_3.5.0
[16] ranger_0.16.0        randomForest_4.7-1.1

loaded via a namespace (and not attached):
 [1] gtable_0.3.4        xfun_0.43           htmlwidgets_1.6.4
 [4] lattice_0.22-6      tzdb_0.4.0          vctrs_0.6.5
 [7] tools_4.3.3         generics_0.1.3      parallel_4.3.3
[10] sandwich_3.1-0      fansi_1.0.6         pkgconfig_2.0.3
[13] haplo.stats_1.9.5.1 Matrix_1.6-5        data.table_1.15.4
[16] checkmate_2.3.1     lifecycle_1.0.4     compiler_4.3.3
[19] MatrixModels_0.5-3  munsell_0.5.1       codetools_0.2-20
[22] poisbinom_1.0.1     SparseM_1.81        quantreg_5.97
[25] htmltools_0.5.8.1   htmlTable_2.4.2     Formula_1.2-5
[28] pillar_1.9.0        MASS_7.3-60.0.1     rms_6.8-0
[31] Hmisc_5.1-2         multcomp_1.4-25     rpart_4.1.23
[34] nlme_3.1-164        tidyselect_1.2.1    digest_0.6.35
[37] mvtnorm_1.2-4       polspline_1.1.24    stringi_1.8.3
[40] splines_4.3.3       fastmap_1.1.1       grid_4.3.3
[43] colorspace_2.1-0    cli_3.6.2           magrittr_2.0.3
[46] base64enc_0.1-3     arsenal_3.6.3       survival_3.5-8
[49] utf8_1.2.4          TH.data_1.1-2       foreign_0.8-86
[52] withr_3.0.0         scales_1.3.0        backports_1.4.1
[55] timechange_0.3.0    rmarkdown_2.26      nnet_7.3-19
[58] gridExtra_2.3       zoo_1.8-12          hms_1.1.3
[61] evaluate_0.23       knitr_1.46          rlang_1.1.3
[64] Rcpp_1.0.12         glue_1.7.0          rstudioapi_0.16.0
[67] plyr_1.8.9          R6_2.5.1
