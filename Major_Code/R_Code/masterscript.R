# masterscript:



# The first step is to load the file that contains all the functions we need:
source("all_functions.R")


# simulate data:
source("simulation_scenarios.R") 
# This takes a very long time and requires a lot of storage.
# I recommend to download the data from ZENODO or to go directly to 
# the conclusions section where the calculated results are analysed.  



# Download simulated data from ZENODO and store the data in ("../Data/")





# Calculation of the scores for the simulation scenarios:
# This takes a very long time, so I recommend to download the results 
# from ZENODO go directly to the conclusion section.


# for the 2-way interactions:
# p = 2,000
source(file = "2w_2k.R")
# p = 20,000
source(file = "2w_20k.R")
# p = 200,000
source(file = "2w_200k.R")
# p = 2,000,000  
# We had to split it up, otherwise the server capacity would not be sufficient 
source(file = "2w_2000k_10.R")
source(file = "2w_2000k_20.R")
source(file = "2w_2000k_30.R")
source(file = "2w_2000k_40.R")
source(file = "2w_2000k_50.R")
source(file = "2w_2000k_60.R")
source(file = "2w_2000k_70.R")
source(file = "2w_2000k_80.R")
source(file = "2w_2000k_90.R")
source(file = "2w_2000k_100.R")


# for the two 2-way interactions:
source(file = "2w2w_2k.R")
# p = 20,000
source(file = "2w2w_20k.R")
# p = 200,000
source(file = "2w2w_200k.R")
# p = 2,000,000  
# We had to split it up, otherwise the server capacity would not be sufficient 
source(file = "2w2w_2000k_10.R")
source(file = "2w2w_2000k_20.R")
source(file = "2w2w_2000k_30.R")
source(file = "2w2w_2000k_40.R")
source(file = "2w2w_2000k_50.R")
source(file = "2w2w_2000k_60.R")
source(file = "2w2w_2000k_70.R")
source(file = "2w2w_2000k_80.R")
source(file = "2w2w_2000k_90.R")
source(file = "2w2w_2000k_100.R")

# for the 3-way interactions:
# p = 2,000
source(file = "3w_2k.R")
# p = 20,000
source(file = "3w_20k.R")
# p = 200,000
source(file = "3w_200k.R")

# for the 4-way interactions:
# p = 2,000
source(file = "4w_2k.R")
# p = 20,000
source(file = "4w_20k.R")
# p = 200,000
source(file = "4w_200k.R")


### toyexample:
source("toyexample.r")


### Conclusion 2w:
source(file = "conclusion_2w.R")

### Conclusion 2w2w:
source(file = "conclusion_2w2w.R")

### Conclusion 3w:
source(file = "conclusion_3w.R")

### Conclusion 4w:
source(file = "conclusion_4w.R")

### real data scenario HapMap
source(file = "hap_map.R")


### influence on the variable importance when we do logic regression in a second step:
source(file = "2w500_logicDT.R")

### parameter tuning analysis:
source(file = "paramtertuning_w.R")
