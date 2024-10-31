# In this file we will build the simulation scenarios.

library('tidyverse')
library('scrime')

# We will use the simulateSNPglm function from the scrime package

if (!dir.exists("../Data")) {
  dir.create("../Data", recursive = TRUE)
}




### 2way interaction: ####

# In the first scenario, we have only one important 2-way interaction

#### p = 2k ####
list_int <- list(c(-1,-1)) 
list_snp <- list(1:2)


set.seed(17)
mafs = runif(2000,0.15,0.45)
Szen2w_2k <- list()
for(i in 1:1000){  
  Szen2w_2k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2k[[i]]$x <- ifelse(Szen2w_2k[[i]]$x==1, 0, Szen2w_2k[[i]]$x)
  Szen2w_2k[[i]]$x <- ifelse(Szen2w_2k[[i]]$x==2, 1, Szen2w_2k[[i]]$x)
  Szen2w_2k[[i]]$x <- ifelse(Szen2w_2k[[i]]$x==3, 2, Szen2w_2k[[i]]$x)
}

 save(Szen2w_2k, file = '../Data/Szen2w_2k.RData')


#### p = 20k ####
list_int <- list(c(-1,-1)) 
list_snp <- list(1:2)
mafs = runif(20000,0.15,0.45)
Szen2w_20k <- list()
for(i in 1:1000){  
  Szen2w_20k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 20000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_20k[[i]]$x <- ifelse(Szen2w_20k[[i]]$x==1, 0, Szen2w_20k[[i]]$x)
  Szen2w_20k[[i]]$x <- ifelse(Szen2w_20k[[i]]$x==2, 1, Szen2w_20k[[i]]$x)
  Szen2w_20k[[i]]$x <- ifelse(Szen2w_20k[[i]]$x==3, 2, Szen2w_20k[[i]]$x)
}

save(Szen2w_20k, file = '../Data/Szen2w_20k.RData')


#### p = 200k ####

list_int <- list(c(-1,-1)) 
list_snp <- list(1:2)

set.seed(19)
mafs = runif(200000,0.15,0.45)
Szen2w_200k <- list()
for(i in 1:100){  
  Szen2w_200k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 200000,
                                    list.ia = list_int,
                                    list.snp = list_snp,
                                    maf = mafs,
                                    beta0 = log(0.3/(1-0.3)),
                                    beta = log(50)
  )
  print(i)
}
for(i in 1:100){
  print(i)
#  recode from 1,2,3 to 0,1,2:
  Szen2w_200k[[i]]$x <- ifelse(Szen2w_200k[[i]]$x==1, 0, Szen2w_200k[[i]]$x)
  Szen2w_200k[[i]]$x <- ifelse(Szen2w_200k[[i]]$x==2, 1, Szen2w_200k[[i]]$x)
  Szen2w_200k[[i]]$x <- ifelse(Szen2w_200k[[i]]$x==3, 2, Szen2w_200k[[i]]$x)
}
save(Szen2w_200k, file = '../Data/Szen2w_200k.RData')




#### p = 2000k ####
list_int <- list(c(-1,-1)) 
list_snp <- list(1:2)
# We have to split it up. Otherwise the data will be too big.
set.seed(10)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_10.RData')


set.seed(20)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_20.RData')


set.seed(30)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_30.RData')

set.seed(40)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_40.RData')

set.seed(50)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_50.RData')

set.seed(60)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_60.RData')

set.seed(70)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_70.RData')


set.seed(80)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_80.RData')

set.seed(90)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_90.RData')

set.seed(100)
mafs = runif(2000000,0.15,0.45)
Szen2w_2000k <- list()
for(i in 1:10){  
  Szen2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = log(50)
  )
  print(i)
}
for(i in 1:10){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==1, 0, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==2, 1, Szen2w_2000k[[i]]$x)
  Szen2w_2000k[[i]]$x <- ifelse(Szen2w_2000k[[i]]$x==3, 2, Szen2w_2000k[[i]]$x)
}

save(Szen2w_2000k, file =  '../Data/Szen2w_2000k_100.RData')


### 3way  ####

#### p = 2k ####
set.seed(1619)
list_int <- list(c(-1,-1,-1)) 
list_snp <- list(1:3)

mafs = runif(2000,0.15,0.45)
Szen3w_2k <- list()

for(i in 1:1000){  
  Szen3w_2k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen3w_2k[[i]]$x <- ifelse(Szen3w_2k[[i]]$x==1, 0, Szen3w_2k[[i]]$x)
  Szen3w_2k[[i]]$x <- ifelse(Szen3w_2k[[i]]$x==2, 1, Szen3w_2k[[i]]$x)
  Szen3w_2k[[i]]$x <- ifelse(Szen3w_2k[[i]]$x==3, 2, Szen3w_2k[[i]]$x)
}

save(Szen3w_2k, file = '../Data/Szen3w_2k.RData')



#### p = 20k ####
set.seed(1054)
list_int <- list(c(-1,-1,-1))
list_snp <- list(1:3)



mafs = runif(20000,0.15,0.45)
Szen3w_20k <- list()

for(i in 1:1000){
  Szen3w_20k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 20000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen3w_20k[[i]]$x <- ifelse(Szen3w_20k[[i]]$x==1, 0, Szen3w_20k[[i]]$x)
  Szen3w_20k[[i]]$x <- ifelse(Szen3w_20k[[i]]$x==2, 1, Szen3w_20k[[i]]$x)
  Szen3w_20k[[i]]$x <- ifelse(Szen3w_20k[[i]]$x==3, 2, Szen3w_20k[[i]]$x)
}

save(Szen3w_20k, file = '../Data/Szen3w_20k.RData')


#### p = 200k ####

list_int <- list(c(-1,-1,-1))
list_snp <- list(1:3)

set.seed(858)

mafs = runif(200000,0.15,0.45)
Szen3w_200k <- list()
for(i in 1:100){
  Szen3w_200k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 200000,
                                    list.ia = list_int,
                                    list.snp = list_snp,
                                    maf = mafs,
                                    beta0 = log(0.3/(1-0.3)),
                                    beta = log(50)
  )

  print(i)
}
for(i in 1:100){
  Szen3w_200k[[i]]$x <- ifelse(Szen3w_200k[[i]]$x==1, 0, Szen3w_200k[[i]]$x)
  Szen3w_200k[[i]]$x <- ifelse(Szen3w_200k[[i]]$x==2, 1, Szen3w_200k[[i]]$x)
  Szen3w_200k[[i]]$x <- ifelse(Szen3w_200k[[i]]$x==3, 2, Szen3w_200k[[i]]$x)
  print(i)
}

save(Szen3w_200k, file = '../Data/Szen3w_200k.RData')



### 2way2way interaction: #### 

#### p = 2k #### 
set.seed(1546)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000,0.15,0.45)
Szen2w2w_2k <- list()

for(i in 1:1000){  
  Szen2w2w_2k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                     list.ia = list_int,
                                     list.snp = list_snp,
                                     maf = mafs,
                                     beta0 = log(0.3/(1-0.3)),
                                     beta = c(log(50), log(50))
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w2w_2k[[i]]$x <- ifelse(Szen2w2w_2k[[i]]$x==1, 0, Szen2w2w_2k[[i]]$x)
  Szen2w2w_2k[[i]]$x <- ifelse(Szen2w2w_2k[[i]]$x==2, 1, Szen2w2w_2k[[i]]$x)
  Szen2w2w_2k[[i]]$x <- ifelse(Szen2w2w_2k[[i]]$x==3, 2, Szen2w2w_2k[[i]]$x)
}

save(Szen2w2w_2k, file = '../Data/Szen2w2w_2k.RData')



#### p = 20k ####
set.seed(1737)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)



mafs = runif(20000,0.15,0.45)
Szen2w2w_20k <- list()

for(i in 1:1000){  
  Szen2w2w_20k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 20000,
                                      list.ia = list_int,
                                      list.snp = list_snp,
                                      maf = mafs,
                                      beta0 = log(0.3/(1-0.3)),
                                      beta = c(log(50), log(50))
  )
  print(i)
  
}
for(i in 1:1000){
  Szen2w2w_20k[[i]]$x <- ifelse(Szen2w2w_20k[[i]]$x==1, 0, Szen2w2w_20k[[i]]$x)
  Szen2w2w_20k[[i]]$x <- ifelse(Szen2w2w_20k[[i]]$x==2, 1, Szen2w2w_20k[[i]]$x)
  Szen2w2w_20k[[i]]$x <- ifelse(Szen2w2w_20k[[i]]$x==3, 2, Szen2w2w_20k[[i]]$x)
}

save(Szen2w2w_20k, file = '../Data/Szen2w2w_20k.RData')


#### p = 200k ####
set.seed(1709)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)




mafs = runif(200000,0.15,0.45)
Szen2w2w_200k <- list()

for(i in 1:100){  
  Szen2w2w_200k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 200000,
                                       list.ia = list_int,
                                       list.snp = list_snp,
                                       maf = mafs,
                                       beta0 = log(0.3/(1-0.3)),
                                       beta = c(log(50), log(50))
  )
  print(i)
  
}

for(i in 1:100){
  Szen2w2w_200k[[i]]$x <- ifelse(Szen2w2w_200k[[i]]$x==1, 0, Szen2w2w_200k[[i]]$x)
  Szen2w2w_200k[[i]]$x <- ifelse(Szen2w2w_200k[[i]]$x==2, 1, Szen2w2w_200k[[i]]$x)
  Szen2w2w_200k[[i]]$x <- ifelse(Szen2w2w_200k[[i]]$x==3, 2, Szen2w2w_200k[[i]]$x)
}

save(Szen2w2w_200k, file = '../Data/Szen2w2w_200k.RData')


#### p = 2000k ####
# We have to split it up. Otherwise the data will be too big.
set.seed(1514) 
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_10.RData')


set.seed(1524)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_20.RData')


set.seed(1534) 
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_30.RData')


set.seed(1544)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)

mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_40.RData')


set.seed(1554)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_50.RData')

set.seed(1564)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_60.RData')

set.seed(1574) 
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_70.RData')

set.seed(1584) 
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_80.RData')

set.seed(1594)
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_90.RData')


# 100
set.seed(15100) 
list_int <- list(c(-1,-1), c(-1,-1)) 
list_snp <- list(1:2,3:4)


mafs = runif(2000000,0.15,0.45)
Szen2w2w_2000k <- list()
for(i in 1:10){  
  Szen2w2w_2000k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000000,
                                        list.ia = list_int,
                                        list.snp = list_snp,
                                        maf = mafs,
                                        beta0 = log(0.3/(1-0.3)),
                                        beta = c(log(50), log(50))
  )
  print(i)
}

for(i in 1:10){
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==1, 0, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==2, 1, Szen2w2w_2000k[[i]]$x)
  Szen2w2w_2000k[[i]]$x <- ifelse(Szen2w2w_2000k[[i]]$x==3, 2, Szen2w2w_2000k[[i]]$x)
  print(i)
}

save(Szen2w2w_2000k, file = '../Data/Szen2w2w_2000k_100.RData')




### 4way interaction ####
#### p = 2k ####
list_int <- list(c(-1,-1,-1, -1)) 
list_snp <- list(1:4)


set.seed(1748)
mafs = runif(2000,0.15,0.45)
Szen4w_2k <- list()

for(i in 1:1000){  
  Szen4w_2k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode:
  Szen4w_2k[[i]]$x <- ifelse(Szen4w_2k[[i]]$x==1, 0, Szen4w_2k[[i]]$x)
  Szen4w_2k[[i]]$x <- ifelse(Szen4w_2k[[i]]$x==2, 1, Szen4w_2k[[i]]$x)
  Szen4w_2k[[i]]$x <- ifelse(Szen4w_2k[[i]]$x==3, 2, Szen4w_2k[[i]]$x)
}

save(Szen4w_2k, file = '../Data/Szen4w_2k.RData')




#### p = 20k ####
set.seed(1639)
list_int <- list(c(-1,-1,-1,-1))
list_snp <- list(1:4)



mafs = runif(20000,0.15,0.45)
Szen4w_20k <- list()

for(i in 1:1000){
  Szen4w_20k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 20000,
                                    list.ia = list_int,
                                    list.snp = list_snp,
                                    maf = mafs,
                                    beta0 = log(0.3/(1-0.3)),
                                    beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # umcodieren von 1,2,3 to 0,1,2:
  Szen4w_20k[[i]]$x <- ifelse(Szen4w_20k[[i]]$x==1, 0, Szen4w_20k[[i]]$x)
  Szen4w_20k[[i]]$x <- ifelse(Szen4w_20k[[i]]$x==2, 1, Szen4w_20k[[i]]$x)
  Szen4w_20k[[i]]$x <- ifelse(Szen4w_20k[[i]]$x==3, 2, Szen4w_20k[[i]]$x)
}

save(Szen4w_20k, file = '../Data/Szen4w_20k.RData')



#### p = 200k ####

list_int <- list(c(-1,-1,-1,-1))
list_snp <- list(1:4)

set.seed(917)

mafs = runif(200000,0.15,0.45)
Szen4w_200k <- list()
for(i in 1:100){
  Szen4w_200k[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 200000,
                                     list.ia = list_int,
                                     list.snp = list_snp,
                                     maf = mafs,
                                     beta0 = log(0.3/(1-0.3)),
                                     beta = log(50)
  )
  
  print(i)
}
for(i in 1:100){
  Szen4w_200k[[i]]$x <- ifelse(Szen4w_200k[[i]]$x==1, 0, Szen4w_200k[[i]]$x)
  Szen4w_200k[[i]]$x <- ifelse(Szen4w_200k[[i]]$x==2, 1, Szen4w_200k[[i]]$x)
  Szen4w_200k[[i]]$x <- ifelse(Szen4w_200k[[i]]$x==3, 2, Szen4w_200k[[i]]$x)
  print(i)
}

save(Szen4w_200k, file = '../Data/Szen4w_200k.RData')

### negative beta ####
list_int <- list(c(1, 1)) 
list_snp <- list(1:2)


set.seed(170)
mafs = runif(2000,0.15,0.45)
Szen2w_2k_neg <- list()
for(i in 1:1000){  
  Szen2w_2k_neg[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_2k_neg[[i]]$x <- ifelse(Szen2w_2k_neg[[i]]$x==1, 0, Szen2w_2k_neg[[i]]$x)
  Szen2w_2k_neg[[i]]$x <- ifelse(Szen2w_2k_neg[[i]]$x==2, 1, Szen2w_2k_neg[[i]]$x)
  Szen2w_2k_neg[[i]]$x <- ifelse(Szen2w_2k_neg[[i]]$x==3, 2, Szen2w_2k_neg[[i]]$x)
}

save(Szen2w_2k_neg, file = '/Szen2w_2k_neg.RData')


### small p ####

#### 2w ####
list_int <- list(c(-1,-1)) 
list_snp <- list(1:2)


set.seed(1618)
mafs = runif(500,0.15,0.45)
Szen2w_500 <- list()
for(i in 1:1000){  
  Szen2w_500[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 500,
                                   list.ia = list_int,
                                   list.snp = list_snp,
                                   maf = mafs,
                                   beta0 = log(0.3/(1-0.3)),
                                   beta = log(50)
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  # recode from 1,2,3 to 0,1,2:
  Szen2w_500[[i]]$x <- ifelse(Szen2w_500[[i]]$x==1, 0, Szen2w_500[[i]]$x)
  Szen2w_500[[i]]$x <- ifelse(Szen2w_500[[i]]$x==2, 1, Szen2w_500[[i]]$x)
  Szen2w_500[[i]]$x <- ifelse(Szen2w_500[[i]]$x==3, 2, Szen2w_500[[i]]$x)
}
save(Szen2w_500, file = '../Data/Szen2w_500.RData')




### with interaction but low no main effect ####
list_int <- list(c(-1,-1), -1, -1) 
list_snp <- list(1:2, 1, 2)


set.seed(18222)
mafs = runif(2000,0.15,0.45)
Szen2w_2k_int_nomain4 <- list()
for(i in 1:1000){  
  Szen2w_2k_int_nomain4[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                               list.ia = list_int,
                                               list.snp = list_snp,
                                               maf = mafs,
                                               beta0 = log(0.3/(1-0.3)),
                                               beta = c(log(50), log(1.0001), 
                                                        log(1.0001))
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  Szen2w_2k_int_nomain4[[i]]$x <- ifelse(Szen2w_2k_int_nomain4[[i]]$x==1, 0, 
                                         Szen2w_2k_int_nomain4[[i]]$x)
  Szen2w_2k_int_nomain4[[i]]$x <- ifelse(Szen2w_2k_int_nomain4[[i]]$x==2, 1, 
                                         Szen2w_2k_int_nomain4[[i]]$x)
  Szen2w_2k_int_nomain4[[i]]$x <- ifelse(Szen2w_2k_int_nomain4[[i]]$x==3, 2, 
                                         Szen2w_2k_int_nomain4[[i]]$x)
}


save(Szen2w_2k_int_nomain4, file = '../Data/Szen2w_2k_int_nomain4.RData')

### only small main effect ####
list_int <- list(-1, -1) 
list_snp <- list(1, 2)


set.seed(18222)
mafs = runif(2000,0.15,0.45)
Szen2w_2k_int_nomain5 <- list()
for(i in 1:1000){  
  Szen2w_2k_int_nomain5[[i]] <- simulateSNPglm(n.obs = 120, n.snp = 2000,
                                               list.ia = list_int,
                                               list.snp = list_snp,
                                               maf = mafs,
                                               beta0 = log(0.3/(1-0.3)),
                                               beta = c(log(1.0001), log(1.0001))
  )
  print(i)
}
for(i in 1:1000){
  print(i)
  Szen2w_2k_int_nomain5[[i]]$x <- ifelse(Szen2w_2k_int_nomain5[[i]]$x==1, 0,
                                         Szen2w_2k_int_nomain5[[i]]$x)
  Szen2w_2k_int_nomain5[[i]]$x <- ifelse(Szen2w_2k_int_nomain5[[i]]$x==2, 1, 
                                         Szen2w_2k_int_nomain5[[i]]$x)
  Szen2w_2k_int_nomain5[[i]]$x <- ifelse(Szen2w_2k_int_nomain5[[i]]$x==3, 2, 
                                         Szen2w_2k_int_nomain5[[i]]$x)
}

save(Szen2w_2k_int_nomain5, file = '../Data/Szen2w_2k_int_nomain5.RData')


CLS2w_2k_int_nomain4 <- numeric()
for(i in 1:1000){
  
  CLS2w_2k_int_nomain4 <- rbind(CLS2w_2k_int_nomain4, 
                                getCLS(cbind(Szen2w_2k_int_nomain4[[i]]$x,
                                             Szen2w_2k_int_nomain4[[i]]$y)))
  print(i)
}