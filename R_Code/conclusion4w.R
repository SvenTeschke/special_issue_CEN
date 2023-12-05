library(tidyverse)
library(ggplot2)
library("ggpubr") ## for ggrange

# read the funtions we need
source("all_functions_neu.R")


#### density ####
load("Szen4w_2k.RData")
load("Szen4w_20k.RData")

# calculating the CLS for 2k:
CLS4w_2k <- numeric()
for(i in 1:1000){
  
  CLS4w_2k <- rbind(CLS4w_2k, getCLS(cbind(Szen4w_2k[[i]]$x, Szen4w_2k[[i]]$y)))
  print(i)
}

# calculating the CLS for 20k:
CLS4w_20k <- numeric()
for(i in 1:1000){

  CLS4w_20k <- rbind(CLS4w_20k, getCLS(cbind(Szen4w_20k[[i]]$x, Szen4w_20k[[i]]$y)))
  print(i)
}



##### 2k: ####

# not absolute cls:
vv4w_i2 = density(CLS4w_2k[,1:2])
vv4w_ni_4w = density(CLS4w_2k[, -(1:2)])


dens_4w_2k =  ggplot() +
  geom_line(aes(x= vv4w_i2$x, y = vv4w_i2$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv4w_ni_4w$x, vv4w_ni_4w$y, colour = "unimp"), linewidth = 1) +
  labs(x = "CLS", y = "estimated density", title = "Density of CLS in scenario 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="SNPs:",
                      values=c(imp2 = "darkgreen",
                               unimp = "black"),
                      labels = c( "important", "unimportant"))


ggsave(filename = "dens_4w_2k.pdf", dens_4w_2k, device = "pdf",
       width = 10, height = 5)


##### 20k: ####

# not absolute cls:
vv4w_i2 = density(CLS4w_20k[,1:2])
vv4w_ni_4w = density(CLS4w_20k[, -(1:2)])

dens_4w_20k =  ggplot() + 
  geom_line(aes(x= vv4w_i2$x, y = vv4w_i2$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv4w_ni_4w$x, vv4w_ni_4w$y), linewidth = 1) +
  labs(x = "CLS", y = "estimated density", title = "Density of CLS in scenario 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="SNPs:",
                      values=c(imp2 = "darkgreen"),
                      labels = c( "important"))

ggsave(filename = "dens_4w_20k.pdf", dens_4w_20k, device = "pdf",
       width = 10, height = 5)




#### how many ####


##### p = 2k #####
load("selected_4w_2k_not_abs.RData")
q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$whole[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$whole[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$whole[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$whole[,j] == "SNP4")) %>% sort()
  whole = rep(0,2000)
  whole[w1[1]:2000] <- 1
  whole[w1[2]:2000] <- 2
  whole[w1[3]:2000] <- 3
  whole[w1[4]:2000] <- 4
  
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$RW[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$RW[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$RW[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$RW[,j] == "SNP4")) %>% sort()
  RW = rep(0,2000)
  RW[w1[1]:2000] <- 1
  RW[w1[2]:2000] <- 2
  RW[w1[3]:2000] <- 3
  RW[w1[4]:2000] <- 4
  
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$SW[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$SW[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$SW[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$SW[,j] == "SNP4")) %>% sort()
  SW = rep(0,2000)
  SW[w1[1]:2000] <- 1
  SW[w1[2]:2000] <- 2
  SW[w1[3]:2000] <- 3
  SW[w1[4]:2000] <- 4
  
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$e_0.5[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$e_0.5[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$e_0.5[,j] == "SNP4")) %>% sort()
  e_0.5 = rep(0,2000)
  e_0.5[w1[1]:2000] <- 1
  e_0.5[w1[2]:2000] <- 2
  e_0.5[w1[3]:2000] <- 3
  e_0.5[w1[4]:2000] <- 4
  
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$e_0.2[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$e_0.2[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$e_0.2[,j] == "SNP4")) %>% sort()
  e_0.2 = rep(0,2000)
  e_0.2[w1[1]:2000] <- 1
  e_0.2[w1[2]:2000] <- 2
  e_0.2[w1[3]:2000] <- 3
  e_0.2[w1[4]:2000] <- 4
  
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$e_0.1[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$e_0.1[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$e_0.1[,j] == "SNP4")) %>% sort()
  e_0.1 = rep(0,2000)
  e_0.1[w1[1]:2000] <- 1
  e_0.1[w1[2]:2000] <- 2
  e_0.1[w1[3]:2000] <- 3
  e_0.1[w1[4]:2000] <- 4
  
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_4w_2k_not_abs$corr[,j] == "SNP1"), 
         which(selected_4w_2k_not_abs$corr[,j] == "SNP2"),
         which(selected_4w_2k_not_abs$corr[,j] == "SNP3"),
         which(selected_4w_2k_not_abs$corr[,j] == "SNP4")) %>% sort()
  corr = rep(0,2000)
  corr[w1[1]:2000] <- 1
  corr[w1[2]:2000] <- 2
  corr[w1[3]:2000] <- 3
  corr[w1[4]:2000] <- 4
  
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_2k_not_abs <- data.frame("q" = 4:1000,
                                 "whole" = colMeans(whole_)[-(1:3)],
                                 "RW" = colMeans(RW_)[-(1:3)],
                                 "SW" = colMeans(SW_)[-(1:3)],
                                 "corr" = colMeans(corr_)[-(1:3)],
                                 "e0.5" = colMeans(e_0.5_)[-(1:3)],
                                 "e0.2" = colMeans(e_0.2_)[-(1:3)],
                                 "e0.1" = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])



mean_4w_2k_not_abs = mean_4w_2k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_4w_2k_not_abs = ggplot(mean_4w_2k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 4*q / 2000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 4, linewidth = 0.5, lty =3, colour = "black") + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = "black") +
  labs(x = "q", y = "important variables found", title = "p=2000") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="Method:",
                      values=c(corr = "yellow", 
                               e0.1 = "violetred2",
                               e0.2 = "violetred",
                               e0.5 = "violet",  
                               expected = "grey",
                               RW = "firebrick",
                               SW = "navy",
                               whole = "darkgreen"),
                      labels = c("corr", 
                                 expression("Sketching"~(epsilon~"="~0.1)), expression("Sketching"~(epsilon~"="~0.2)),
                                 expression("Sketching"~(epsilon~"="~0.5)), "expected",
                                 "RW", "SW", "whole"))

ggsave(filename = "plt_mean_4w_2k_not_abs.pdf", plt_mean_4w_2k_not_abs, device = "pdf",
       width = 10, height = 5)




##### p = 20k #####
load("selected_4w_20k_not_abs.RData")
q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$whole[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$whole[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$whole[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$whole[,j] == "SNP4")) %>% sort()
  whole = rep(0,10000)
  whole[w1[1]:10000] <- 1
  whole[w1[2]:10000] <- 2
  whole[w1[3]:10000] <- 3
  whole[w1[4]:10000] <- 4
  }, error=function(e){})
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}



# RW:
RW_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$RW[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$RW[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$RW[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$RW[,j] == "SNP4")) %>% sort()
  RW = rep(0,10000)
  RW[w1[1]:10000] <- 1
  RW[w1[2]:10000] <- 2
  RW[w1[3]:10000] <- 3
  RW[w1[4]:10000] <- 4
  }, error=function(e){})
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$SW[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$SW[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$SW[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$SW[,j] == "SNP4")) %>% sort()
  SW = rep(0,10000)
  SW[w1[1]:10000] <- 1
  SW[w1[2]:10000] <- 2
  SW[w1[3]:10000] <- 3
  SW[w1[4]:10000] <- 4
  }, error=function(e){})
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$e_0.5[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$e_0.5[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$e_0.5[,j] == "SNP4")) %>% sort()
  e_0.5 = rep(0,10000)
  e_0.5[w1[1]:10000] <- 1
  e_0.5[w1[2]:10000] <- 2
  e_0.5[w1[3]:10000] <- 3
  e_0.5[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$e_0.2[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$e_0.2[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$e_0.2[,j] == "SNP4")) %>% sort()
  e_0.2 = rep(0,10000)
  e_0.2[w1[1]:10000] <- 1
  e_0.2[w1[2]:10000] <- 2
  e_0.2[w1[3]:10000] <- 3
  e_0.2[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$e_0.1[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$e_0.1[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$e_0.1[,j] == "SNP4")) %>% sort()
  e_0.1 = rep(0,10000)
  e_0.1[w1[1]:10000] <- 1
  e_0.1[w1[2]:10000] <- 2
  e_0.1[w1[3]:10000] <- 3
  e_0.1[w1[4]:10000] <- 4
}, error=function(e){})
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  tryCatch({
  w1 = c(which(selected_4w_20k_not_abs$corr[,j] == "SNP1"), 
         which(selected_4w_20k_not_abs$corr[,j] == "SNP2"),
         which(selected_4w_20k_not_abs$corr[,j] == "SNP3"),
         which(selected_4w_20k_not_abs$corr[,j] == "SNP4")) %>% sort()
  corr = rep(0,10000)
  corr[w1[1]:10000] <- 1
  corr[w1[2]:10000] <- 2
  corr[w1[3]:10000] <- 3
  corr[w1[4]:10000] <- 4
}, error=function(e){})
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_20k_not_abs <- data.frame("q" = 4:1000,
                                 "whole" = colMeans(whole_)[-(1:3)],
                                 "RW" = colMeans(RW_)[-(1:3)],
                                 "SW" = colMeans(SW_)[-(1:3)],
                                 "corr" = colMeans(corr_)[-(1:3)],
                                 "e0.5" = colMeans(e_0.5_)[-(1:3)],
                                 "e0.2" = colMeans(e_0.2_)[-(1:3)],
                                 "e0.1" = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])



mean_4w_20k_not_abs = mean_4w_20k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_4w_20k_not_abs = ggplot(mean_4w_20k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 4*q / 20000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 4, linewidth = 0.5, lty =3, colour = "black") + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = "black") +
  labs(x = "q", y = "important variables found", title = "p=20000") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="Method:",
                      values=c(corr = "yellow", 
                               e0.1 = "violetred2",
                               e0.2 = "violetred",
                               e0.5 = "violet",  
                               expected = "grey",
                               RW = "firebrick",
                               SW = "navy",
                               whole = "darkgreen"),
                      labels = c("corr", 
                                 expression("Sketching"~(epsilon~"="~0.1)), expression("Sketching"~(epsilon~"="~0.2)),
                                 expression("Sketching"~(epsilon~"="~0.5)), "expected",
                                 "RW", "SW", "whole"))

ggsave(filename = "plt_mean_4w_20k_not_abs.pdf", plt_mean_4w_20k_not_abs, device = "pdf",
       width = 10, height = 5)




##### p = 200k #####
# load("C://Users/teschke/Promotion/Research/merging_CLS_paper/Data/neu/output/4w/selected_4w_200k_not_abs.RData")
load("selected_4w_200k_not_abs.RData")

q = 10000
# whole:
whole_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$whole[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$whole[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$whole[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$whole[,j] == "SNP4")) %>% sort()
    whole = rep(0,10000)
    whole[w1[1]:10000] <- 1
    whole[w1[2]:10000] <- 2
    whole[w1[3]:10000] <- 3
    whole[w1[4]:10000] <- 4
  }, error=function(e){})
  whole_ = rbind(whole_, whole[1:q])
  
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$RW[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$RW[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$RW[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$RW[,j] == "SNP4")) %>% sort()
    RW = rep(0,10000)
    RW[w1[1]:10000] <- 1
    RW[w1[2]:10000] <- 2
    RW[w1[3]:10000] <- 3
    RW[w1[4]:10000] <- 4
  }, error=function(e){})
  RW_ = rbind(RW_, RW[1:q])
  
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$SW[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$SW[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$SW[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$SW[,j] == "SNP4")) %>% sort()
    SW = rep(0,10000)
    SW[w1[1]:10000] <- 1
    SW[w1[2]:10000] <- 2
    SW[w1[3]:10000] <- 3
    SW[w1[4]:10000] <- 4
  }, error=function(e){})
  SW_ = rbind(SW_, SW[1:q])
  
  print(j)
}

# e_0.5:
e_0.5_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.5[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$e_0.5[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$e_0.5[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$e_0.5[,j] == "SNP4")) %>% sort()
    e_0.5 = rep(0,10000)
    e_0.5[w1[1]:10000] <- 1
    e_0.5[w1[2]:10000] <- 2
    e_0.5[w1[3]:10000] <- 3
    e_0.5[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.5_ = rbind(e_0.5_, e_0.5[1:q])
  
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.2[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$e_0.2[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$e_0.2[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$e_0.2[,j] == "SNP4")) %>% sort()
    e_0.2 = rep(0,10000)
    e_0.2[w1[1]:10000] <- 1
    e_0.2[w1[2]:10000] <- 2
    e_0.2[w1[3]:10000] <- 3
    e_0.2[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.2_ = rbind(e_0.2_, e_0.2[1:q])
  
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$e_0.1[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$e_0.1[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$e_0.1[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$e_0.1[,j] == "SNP4")) %>% sort()
    e_0.1 = rep(0,10000)
    e_0.1[w1[1]:10000] <- 1
    e_0.1[w1[2]:10000] <- 2
    e_0.1[w1[3]:10000] <- 3
    e_0.1[w1[4]:10000] <- 4
  }, error=function(e){})
  e_0.1_ = rbind(e_0.1_, e_0.1[1:q])
  
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,199,2)){
  tryCatch({
    w1 = c(which(selected_4w_200k_not_abs$corr[,j] == "SNP1"), 
           which(selected_4w_200k_not_abs$corr[,j] == "SNP2"),
           which(selected_4w_200k_not_abs$corr[,j] == "SNP3"),
           which(selected_4w_200k_not_abs$corr[,j] == "SNP4")) %>% sort()
    corr = rep(0,10000)
    corr[w1[1]:10000] <- 1
    corr[w1[2]:10000] <- 2
    corr[w1[3]:10000] <- 3
    corr[w1[4]:10000] <- 4
  }, error=function(e){})
  corr_ = rbind(corr_, corr[1:q])
  
  print(j)
}




mean_4w_200k_not_abs <- data.frame("q" = 4:10000,
                                  "whole" = colMeans(whole_)[-(1:3)],
                                  "RW" = colMeans(RW_)[-(1:3)],
                                  "SW" = colMeans(SW_)[-(1:3)],
                                  "corr" = colMeans(corr_)[-(1:3)],
                                  "e0.5" = colMeans(e_0.5_)[-(1:3)],
                                  "e0.2" = colMeans(e_0.2_)[-(1:3)],
                                  "e0.1" = colMeans(e_0.1_)[-(1:3)])

# 575:
median(whole_[,572])
median(RW_[,572])
median(SW_[,572])
median(e_0.5_[,572])
median(e_0.2_[,572])
median(e_0.1_[,572])
median(corr_[,572])



mean_4w_200k_not_abs = mean_4w_200k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_4w_200k_not_abs = ggplot(mean_4w_200k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 4*q / 200000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 4, linewidth = 0.5, lty =3, colour = "black") + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = "black") +
  labs(x = "q", y = "important variables found", title = "p=200000") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="Method:",
                      values=c(corr = "yellow", 
                               e0.1 = "violetred2",
                               e0.2 = "violetred",
                               e0.5 = "violet",  
                               expected = "grey",
                               RW = "firebrick",
                               SW = "navy",
                               whole = "darkgreen"),
                      labels = c("corr", 
                                 expression("Sketching"~(epsilon~"="~0.1)), expression("Sketching"~(epsilon~"="~0.2)),
                                 expression("Sketching"~(epsilon~"="~0.5)), "expected",
                                 "RW", "SW", "whole"))

ggsave(filename = "plt_mean_4w_200k_not_abs.pdf", plt_mean_4w_200k_not_abs, device = "pdf",
       width = 10, height = 5)

