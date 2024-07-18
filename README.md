# special_issue_CEN

PC
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)
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

