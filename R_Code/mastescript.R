# masterscript:



# In a first step load the file containing all functions we need:
source("all_functions_neu.R")


# simulate data:
source("simulation_scenarios.R")




# calculation of the simulation scenarios: 

# -> load the in .........


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
