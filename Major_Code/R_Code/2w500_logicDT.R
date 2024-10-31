library('logicFS')
library('tidyverse')
library('logicDT')
library('ggplot2')


if (!dir.exists("results/2w_500")) {
  dir.create("results/2w_500", recursive = TRUE)
}


# Read in the functions we need:
source('all_functions.R')

# Read in the data. Here we consider one 2-way interaction with a reduced p = 500
load('../Data/Szen2w_500.RData')




##### prediction performance with logicDT ####
set.seed(1207)

err_ohne = c()
err_mit = c()
err_corr = c()
err_samp = c()

for(i in 1:1000){ 
  Data =cbind(Szen2w_500[[i]]$x, Szen2w_500[[i]]$y)
  
  test_ind = sample(1:120, 40)
  Data_train = Data[-test_ind, ]
  Data_test = Data[test_ind, ]
  
  resp_train <- Data_train[,501]
  resp_test <- Data_test[,501]
  
  # without variable selection beforehand:
  Data_train_x <- make.snp.dummy(Data_train[,-501])
  
  log.out_ohne = logicDT::logicDT.bagging(Data_train_x ,resp_train, 
                                          bagging.iter = 50, max_vars = 3,
                                          max_conj = 3, search_algo = 'greedy') 
  
  
  Data_test_x = make.snp.dummy(Data_test[ , -501])
  
  p1 = predict(log.out_ohne, Data_test_x, type = 'class')  
  err_ohne = c(err_ohne, mean(p1 != resp_test))
  
  
  # with variable selection beforehand:
  
  ## with CLS
  c500 = getCLS(Data_train)
  index = which(rank(-c500)< 25)
  
  
  Data_train_red_x <- make.snp.dummy(Data_train[,index])
  
  log.out_mit = logicDT::logicDT.bagging(Data_train_red_x ,resp_train, 
                                         bagging.iter = 50, max_vars = 3, 
                                         max_conj = 3, search_algo = 'greedy') 
  
  
  Data_test_red_x = make.snp.dummy(Data_test[ , index])
  
  
  p2 = predict(log.out_mit, Data_test_red_x, type = 'class')
  err_mit <- c(err_mit, mean(p2 != resp_test))
  
  
   ## with correlation
  corr_d <- apply(Data_train[,-501], 2, function(x) cor(x, resp_train)) 
  index = which(rank(-corr_d) < 25)
  
  Data_train_red_x <- make.snp.dummy(Data_train[,index])
  
  log.out_mit_corr = logicDT::logicDT.bagging(Data_train_red_x ,resp_train, 
                                              bagging.iter = 50, max_vars = 3, 
                                              max_conj = 3, search_algo = 'greedy') 
  
  
  Data_test_red_x = make.snp.dummy(Data_test[ , index])
  
  
  p3 = predict(log.out_mit_corr, Data_test_red_x, type = 'class')
  err_corr <- c(err_corr, mean(p3 != resp_test))
  
  
   ## with a random selection:
 
  index = sample(1:500, 25)
  
  Data_train_red_x <- make.snp.dummy(Data_train[,index])
  
  log.out_mit_samp = logicDT::logicDT.bagging(Data_train_red_x ,resp_train, 
                                              bagging.iter = 50, max_vars = 3,
                                              max_conj = 3, search_algo = 'greedy') 
  
  
  Data_test_red_x = make.snp.dummy(Data_test[ , index])
  
  
  p4 = predict(log.out_mit_samp, Data_test_red_x, type = 'class')
  err_samp  <- c(err_samp, mean(p4 != resp_test))
  

  
  
  save(err_mit, err_ohne, err_corr, err_samp, 
       file ='results/2w_500/logreg_vergleich2w.RData')

  
}
load('results/2w_500/logreg_vergleich2w.RData')


#### Table 06: ####
Table06 <- rbind(c("method", "without", "random", "CLS", "corr"),
  c("prediction error", err_ohne %>% median(), err_samp %>% median(), err_mit %>% median(), err_corr %>% median()))
Table06 <- as.data.frame(Table06)

write.table(Table06, file = "results/2w_500/Table06.txt", col.names = F, row.names = F, sep = ",",
            quote = F)


# Is there a difference with or without variable selection?
t.test(err_ohne, err_mit)



#### variable importance ####
##### Figure 06: #####
# with test and training:
gc()
set.seed(1754)
ohne = list()
mit <- list()
corr <- list()
arb <- list()
for(i in 1:1000){
  
  Data = cbind(Szen2w_500[[i]]$x, Szen2w_500[[i]]$y)
  
  test_ind = sample(1:120, 60)
  Data_train = Data[-test_ind, ]
  Data_test = Data[test_ind, ]
  
  resp_train <- Data_train[,501]
  resp_test <- Data_test[,501]
  
  # without variable selection:
  Data_train_x <- make.snp.dummy(Data_train[,-501])
  
  
  Data_test_x = make.snp.dummy(Data_test[ , -501])
  
  
  log.out_ohne_test = logicDT::logicDT.bagging(Data_test_x ,resp_test, 
                                               bagging.iter = 50, max_vars = 3, 
                                               max_conj = 3, search_algo = 'greedy',
                                               vim_type = 'logic', ave = 'before') 
  ohne[[i]] <- vim(log.out_ohne_test)$vims
  
  
  
  # with variable selection:
  
    ## with cls:
  c500 = getCLS(Data_train)
  index = which(rank(-c500) < 26)
  
  
  Data_red_x = make.snp.dummy(Data_test[, index])
  
  log.out_mit_test = logicDT::logicDT.bagging(Data_red_x ,resp_test, 
                                              bagging.iter = 50, max_vars = 3, 
                                              max_conj = 3, search_algo = 'greedy',
                                               vim_type = 'logic', ave = 'before') 
  mit[[i]] <- vim(log.out_mit_test)$vims 
  
  
    ## with correlation:
  corr_d <- apply(Data_train[,-501], 2, function(x) cor(x, resp_train)) #%>% abs()
  index = which(rank(-corr_d) < 26)
  Data_red_x <- make.snp.dummy(Data_test[,index])
  
  log.out_corr_test = logicDT::logicDT.bagging(Data_red_x ,resp_test, 
                                               bagging.iter = 50, max_vars = 3, 
                                               max_conj = 3, search_algo = 'greedy',
                                               vim_type = 'logic', ave = 'before')  
  corr[[i]] <- vim(log.out_corr_test)$vims
  
    ## with an arbitrary selection:
   index = sample(1:500, 25)
   Data_red_x <- make.snp.dummy(Data_test[,index])
  
   log.out_arb_test = logicDT::logicDT.bagging(Data_red_x ,resp_test, 
                                               bagging.iter = 50, max_vars = 3,
                                               max_conj = 3, search_algo = 'greedy',
                                               vim_type = 'logic', ave = 'before')  
   arb[[i]] <- vim(log.out_arb_test)$vims
  
  
  print(i)
}

save(ohne, mit, corr,arb, file = 'results/2w_500/var_imp_testtraining.RData')

load('results/2w_500/var_imp_testtraining.RData')



w_mit <- c()
w_ohne <- c()
w_corr <- c()
w_arb <- c()
for(i in 1:1000){
  if(length(which(ohne[[i]]$var == 'SNP1_1^SNP2_1')) == 1){w_ohne_ = which(ohne[[i]]$var == 'SNP1_1^SNP2_1')}else{w_ohne_ = 1000000}
  w_ohne = c(w_ohne, w_ohne_)
  
  
  if(length(which(mit[[i]]$var == 'SNP1_1^SNP2_1')) == 1){w_mit_ = which(mit[[i]]$var == 'SNP1_1^SNP2_1')}else{w_mit_ = 1000000}
  w_mit = c(w_mit, w_mit_)
  
  if(length(which(corr[[i]]$var == 'SNP1_1^SNP2_1')) == 1){w_corr_ = which(corr[[i]]$var == 'SNP1_1^SNP2_1')}else{w_corr_ = 1000000}
  w_corr = c(w_corr, w_corr_)
  
  if(length(which(arb[[i]] == 'SNP1_1^SNP2_1')) == 1){w_arb_ = which(arb[[i]] == 'SNP1_1^SNP2_1')}else{w_arb_ = 1000000}
  w_arb = c(w_arb, w_arb_)
  
  print(i)
}


w_ohne_ = c()
w_arb_ = c()
w_corr_ = c()
w_mit_ = c()
for(i in 2:101){
  w_ohne_ = c(w_ohne_, sum(w_ohne < i))
  w_arb_ = c(w_arb_, sum(w_arb < i))
  w_corr_ = c(w_corr_, sum(w_corr < i))
  w_mit_ = c(w_mit_, sum(w_mit < i))  
}




w_all = data.frame('q' = 1:100,
                   'ohne' = w_ohne_/1000,
                   'arb' = w_arb_/1000,
                   'corr' = w_corr_/1000,
                   'mit' = w_mit_/1000) 

w_all = w_all %>% pivot_longer(cols = c(ohne, arb, #corr, 
                                        mit))

plt_var_imp500_testtraining = ggplot(w_all, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  labs(x = 'v', y = 'presence in the top v', title = 'p=500') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(arb = 'grey',
                              # corr = 'yellow', 
                               mit = 'firebrick',
                               ohne = 'navy'),
                      labels = c('random', 
                                # 'corr', 
                                 'CLS', 'none')) +
  theme(plot.margin = margin(10,10,10,10))


# Figure 06:
ggsave(filename = 'plots/Fig06_var_imp500_testtraining.eps', plt_var_imp500_testtraining, 
       device = cairo_ps,
       width = 8.2, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)


#### How often are the two important variables in the Top2, Top10 most in important variables? ####

sum(w_ohne < 2)
sum(w_arb < 2)
sum(w_mit < 2)
sum(w_corr < 2)



w_mit <- c()
w_ohne <- c()
w_corr <- c()
w_arb <- c()
for(i in 1:1000){
  if(length(which(ohne[[i]] == 'SNP1_1')) == 1){w_ohne_ = which(ohne[[i]] == 'SNP1_1')}else{w_ohne_ = 1000000}
  w_ohne = c(w_ohne, w_ohne_)
  
  
  if(length(which(mit[[i]] == 'SNP1_1')) == 1){w_mit_ = which(mit[[i]] == 'SNP1_1')}else{w_mit_ = 1000000}
  w_mit = c(w_mit, w_mit_)
  
  if(length(which(corr[[i]] == 'SNP1_1')) == 1){w_corr_ = which(corr[[i]] == 'SNP1_1')}else{w_corr_ = 1000000}
  w_corr = c(w_corr, w_corr_)
  
  if(length(which(arb[[i]] == 'SNP1_1')) == 1){w_arb_ = which(arb[[i]] == 'SNP1_1')}else{w_arb_ = 1000000}
  w_arb = c(w_arb, w_arb_)
  
  print(i)
}

sum(w_ohne < 10)
sum(w_arb < 10)
sum(w_mit < 10)
sum(w_corr < 10)




w_mit <- c()
w_ohne <- c()
w_corr <- c()
w_arb <- c()
for(i in 1:1000){
  if(length(which(ohne[[i]] == 'SNP2_1')) == 1){w_ohne_ = which(ohne[[i]] == 'SNP2_1')}else{w_ohne_ = 1000000}
  w_ohne = c(w_ohne, w_ohne_)
  
  
  if(length(which(mit[[i]] == 'SNP2_1')) == 1){w_mit_ = which(mit[[i]] == 'SNP2_1')}else{w_mit_ = 1000000}
  w_mit = c(w_mit, w_mit_)
  
  if(length(which(corr[[i]] == 'SNP2_1')) == 1){w_corr_ = which(corr[[i]] == 'SNP2_1')}else{w_corr_ = 1000000}
  w_corr = c(w_corr, w_corr_)
  
  if(length(which(arb[[i]] == 'SNP2_1')) == 1){w_arb_ = which(arb[[i]] == 'SNP2_1')}else{w_arb_ = 1000000}
  w_arb = c(w_arb, w_arb_)
  
  print(i)
}

sum(w_ohne < 10)
sum(w_arb < 10)
sum(w_mit < 10)
sum(w_corr < 10)

