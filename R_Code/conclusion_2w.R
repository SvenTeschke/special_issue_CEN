library(tidyverse)
library(ggplot2)
library("ggpubr") ## for ggrange

# read the funtions we need
source("all_functions_neu.R")

#### density ####
load("../Data/Szen2w_2k.RData")
load("../Data/Szen2w_20k.RData")

# calculating the CLS for 2k:
CLS2w_2k <- numeric()
for(i in 1:1000){
  
  CLS2w_2k <- rbind(CLS2w_2k, getCLS(cbind(Szen2w_2k[[i]]$x, Szen2w_2k[[i]]$y)))
  print(i)
}

# calculating the CLS for 20k:
CLS2w_20k <- numeric()
for(i in 1:1000){
  
  CLS2w_20k <- rbind(CLS2w_20k, getCLS(cbind(Szen2w_20k[[i]]$x, Szen2w_20k[[i]]$y)))
  print(i)
}


#### 2k ####


# not absolute cls:
vv2w_i2 = density(CLS2w_2k[,1:2])
vv2w_ni_2w = density(CLS2w_2k[, -(1:2)])


dens_2w_2k =  ggplot() +
  geom_line(aes(x= vv2w_i2$x, y = vv2w_i2$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv2w_ni_2w$x, vv2w_ni_2w$y, colour = "unimp"), linewidth = 1) +
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

ggsave(filename = "dens_2w_2k.pdf", dens_2w_2k, device = "pdf",
       width = 10, height = 5)


# absolute cls:
vv2w_i2 = density(abs(CLS2w_2k[,1:2]))
vv2w_ni_2w = density(abs(CLS2w_2k[, -(1:2)]))

dens_2w_2k_abs =  ggplot() + 
  geom_line(aes(x= vv2w_i2$x, y = vv2w_i2$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv2w_ni_2w$x, vv2w_ni_2w$y, colour = "unimp"), linewidth = 1) +
  labs(x = "absolute CLS", y = "estimated density", title = "Density of CLS in scenario 1") +
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

ggsave(filename = "figure/dens_2w_2k_abs.pdf", dens_2w_2k_abs, device = "pdf",
        width = 10, height = 5)


# negativ:
# 'negative' influence on the response if a SNP (interaction) is present
load("Szen2w_2k_neg.RData")

CLS2w_2k_neg <- numeric()
for(i in 1:1000){
  
  CLS2w_2k_neg <- rbind(CLS2w_2k_neg, getCLS(cbind(Szen2w_2k_neg[[i]]$x, Szen2w_2k_neg[[i]]$y)))
  print(i)
}

vv2w_i2_neg = density(CLS2w_2k_neg[,1:2])
vv2w_ni_2w_neg = density(CLS2w_2k_neg[, -(1:2)])


dens_2w_2k_neg =  ggplot() +
  geom_line(aes(x= vv2w_i2_neg$x, y = vv2w_i2_neg$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv2w_ni_2w_neg$x, vv2w_ni_2w_neg$y), size = 1) +
  labs(x = "CLS", y = "estimated density", title = "Density of CLS in scenario 1 (negative)") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  scale_colour_manual(name="SNPs:",
                      values=c(imp2 = "darkgreen"),
                      labels = c( "important"))


ggsave(filename = "figure/dens_2w_2k_neg.pdf", dens_2w_2k_neg, device = "pdf",
       width = 10, height = 5)


# large interaction but low main effect
load("Szen2w_2k_int_nomain4.RData")

CLS2w_2k_int_nomain <- numeric()
for(i in 1:1000){
  
  CLS2w_2k_int_nomain <- rbind(CLS2w_2k_int_nomain, getCLS(cbind(Szen2w_2k_int_nomain4[[i]]$x,
                                                                 Szen2w_2k_int_nomain4[[i]]$y)))
  print(i)
}

vv2w_i2 = density(CLS2w_2k_int_nomain[,1:2])
vv2w_ni_2w = density(CLS2w_2k_int_nomain[, -(1:2)])


dens_2w_2k_int_nomain4 =  ggplot() +
  geom_line(aes(x= vv2w_i2$x, y = vv2w_i2$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv2w_ni_2w$x, vv2w_ni_2w$y, colour = "unimp"), linewidth = 1) +
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

# as comparison: low main effect and no interaction effect
load("Szen2w_2k_int_nomain5.RData")

CLS2w_2k_int_nomain5 <- numeric()
for(i in 1:1000){
  
  CLS2w_2k_int_nomain5 <- rbind(CLS2w_2k_int_nomain5, getCLS(cbind(Szen2w_2k_int_nomain5[[i]]$x,
                                                                   Szen2w_2k_int_nomain5[[i]]$y)))
  print(i)
}

vv2w_i25 = density(CLS2w_2k_int_nomain5[,1:2])
vv2w_ni_2w5 = density(CLS2w_2k_int_nomain5[, -(1:2)])


dens_2w_2k_int_nomain5 =  ggplot() +
  geom_line(aes(x= vv2w_i25$x, y = vv2w_i25$y, colour = "imp2"), linewidth = 1) +
  geom_line(aes(x = vv2w_ni_2w5$x, vv2w_ni_2w5$y, colour = "unimp"), linewidth = 1) +
  labs(x = "CLS", y = "estimated density", title = "Density of CLS") +
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



plt_dens_no_with = ggarrange(dens_2w_2k_int_nomain5, dens_2w_2k_int_nomain4,
                             common.legend = TRUE, legend="bottom")

ggsave(filename = "figure/plt_dens_no_with.pdf", plt_dens_no_with, device = "pdf",
       width = 10, height = 5)



#### how many ####

##### p = 2k #####
load("selected_2w_2k_not_abs_neu.RData")

q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$whole[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$whole[,j] == "SNP2")) %>% sort()
  whole = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  
  whole_ = rbind(whole_, whole)
  print(j)
}

# RW
RW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$RW[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$RW[,j] == "SNP2")) %>% sort()
  RW = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  
  RW_ = rbind(RW_, RW)
  print(j)
}

# SW
SW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$SW[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$SW[,j] == "SNP2")) %>% sort()
  SW = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  
  SW_ = rbind(SW_, SW)
  print(j)
}

# corr
corr_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$corr[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$corr[,j] == "SNP2")) %>% sort()
  corr = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  
  corr_ = rbind(corr_, corr)
  print(j)
}

# sketch 0.5
e_0.5_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$e_0.5[,j] == "SNP2")) %>% sort()
  e_0.5 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  
  e_0.5_ = rbind(e_0.5_, e_0.5)
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$e_0.2[,j] == "SNP2")) %>% sort()
  e_0.2 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  e_0.2_ = rbind(e_0.2_, e_0.2)
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_2k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_2w_2k_not_abs$e_0.1[,j] == "SNP2")) %>% sort()
  e_0.1 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  e_0.1_ = rbind(e_0.1_, e_0.1)
  print(j)
}

mean_2w_2k_not_abs <- data.frame("q" = 2:1000,
                                 "whole" = colMeans(whole_)[-1],
                                 "RW" = colMeans(RW_)[-1],
                                 "SW" = colMeans(SW_)[-1],
                                 "corr" = colMeans(corr_)[-1],
                                 "e0.5" = colMeans(e_0.5_)[-1],
                                 "e0.2" = colMeans(e_0.2_)[-1],
                                 "e0.1" = colMeans(e_0.1_)[-1])

# 575:
median(whole_[,574])
median(RW_[,574])
median(SW_[,574])
median(e_0.5_[,574])
median(e_0.2_[,574])
median(e_0.1_[,574])
median(corr_[,574])



mean_2w_2k_not_abs = mean_2w_2k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w_2k_not_abs = ggplot(mean_2w_2k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 2000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black") + 
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

ggsave(filename = "figure/plt_mean_2w_2k_not_abs.pdf", plt_mean_2w_2k_not_abs, device = "pdf",
       width = 10, height = 5)



##### p = 20k #####

load("selected_2w_20k_not_abs_neu.RData")

q = 1000
# whole:
whole_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$whole[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$whole[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ whole = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ whole = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ whole = rep(0,q)
  print("null")}}
  
  whole_ = rbind(whole_, whole)
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$RW[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$RW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ RW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ RW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ RW = rep(0,q)
  print("null")}}
  
  RW_ = rbind(RW_, RW)
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$SW[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$SW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ SW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ SW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ SW = rep(0,q)
  print("null")}}
  
  SW_ = rbind(SW_, SW)
  print(j)
}

# e_0.5
e_0.5_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$e_0.5[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.5 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.5 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.5 = rep(0,q)
  print("null")}}
  
  e_0.5_ = rbind(e_0.5_, e_0.5)
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$e_0.2[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.2 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.2 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.2 = rep(0,q)
  print("null")}}
  
  e_0.2_ = rbind(e_0.2_, e_0.2)
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$e_0.1[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.1 = rep(0,q)
  print("null")}}
  
  e_0.1_ = rbind(e_0.1_, e_0.1)
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w_20k_not_abs$corr[,j] == "SNP1"), 
         which(selected_2w_20k_not_abs$corr[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ corr = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ corr = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ corr = rep(0,q)
  print("null")}}
  
  corr_ = rbind(corr_, corr)
  print(j)
}


mean_2w_20k_not_abs <- data.frame("q" = 2:1000,
                                   "whole" = colMeans(whole_)[-1],
                                   "RW" = colMeans(RW_)[-1],
                                   "SW" = colMeans(SW_)[-1],
                                   "corr" = colMeans(corr_)[-1],
                                   "e0.5" = colMeans(e_0.5_)[-1],
                                   "e0.2" = colMeans(e_0.2_)[-1],
                                   "e0.1" = colMeans(e_0.1_)[-1])

# 575:
median(whole_[,574])
median(RW_[,574])
median(SW_[,574])
median(e_0.5_[,574])
median(e_0.2_[,574])
median(e_0.1_[,574])
median(corr_[,574])

mean_2w_20k_not_abs = mean_2w_20k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w_20k_not_abs = ggplot(mean_2w_20k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 20000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black") + 
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

ggsave(filename = "figure/plt_mean_2w_20k_not_abs.pdf", plt_mean_2w_20k_not_abs, device = "pdf",
       width = 10, height = 5)


plt2k_20k_not_abs = ggarrange(plt_mean_2w_2k_not_abs, plt_mean_2w_20k_not_abs,
                             common.legend = TRUE, legend="bottom")

ggsave(filename = "figure/plt2k_20k_not_abs.pdf", plt2k_20k_not_abs, device = "pdf",
       width = 10, height = 5)




##### p = 200k #####
load("selected_2w_200k_not_abs_neu.RData")

q = 10000
# whole:
whole_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$whole[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$whole[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ whole = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ whole = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ whole = rep(0,q)
  print("null")}}
  
  whole_ = rbind(whole_, whole)
  print(j)
}

# RW:
RW_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$RW[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$RW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ RW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ RW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ RW = rep(0,q)
  print("null")}}
  
  RW_ = rbind(RW_, RW)
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$SW[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$SW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ SW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ SW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ SW = rep(0,q)
  print("null")}}
  
  SW_ = rbind(SW_, SW)
  print(j)
}

# e_0.5
e_0.5_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$e_0.5[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.5 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.5 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.5 = rep(0,q)
  print("null")}}
  
  e_0.5_ = rbind(e_0.5_, e_0.5)
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$e_0.2[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.2 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.2 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.2 = rep(0,q)
  print("null")}}
  
  e_0.2_ = rbind(e_0.2_, e_0.2)
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$e_0.1[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.1 = rep(0,q)
  print("null")}}
  
  e_0.1_ = rbind(e_0.1_, e_0.1)
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_200k_not_abs$corr[,j] == "SNP1"), 
         which(selected_2w_200k_not_abs$corr[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ corr = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ corr = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ corr = rep(0,q)
  print("null")}}
  
  corr_ = rbind(corr_, corr)
  print(j)
}


mean_2w_200k_not_abs <- data.frame("q" = 2:10000,
                                 "whole" = colMeans(whole_)[-1],
                                 "RW" = colMeans(RW_)[-1],
                                 "SW" = colMeans(SW_)[-1],
                                 "corr" = colMeans(corr_)[-1],
                                 "e0.5" = colMeans(e_0.5_)[-1],
                                 "e0.2" = colMeans(e_0.2_)[-1],
                                 "e0.1" = colMeans(e_0.1_)[-1])

# 575:
median(whole_[,574])
median(RW_[,574])
median(SW_[,574])
median(e_0.5_[,574])
median(e_0.2_[,574])
median(e_0.1_[,574])
median(corr_[,574])

mean_2w_200k_not_abs = mean_2w_200k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w_200k_not_abs = ggplot(mean_2w_200k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black") + 
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

ggsave(filename = "figure/plt_mean_2w_200k_not_abs.pdf", plt_mean_2w_200k_not_abs, device = "pdf",
       width = 10, height = 5)



##### p = 2000k ####
load("selected_2w_2000k_10_not_abs_neu.RData")
load("selected_2w_2000k_20_not_abs_neu.RData")
load("selected_2w_2000k_30_not_abs_neu.RData")
load("selected_2w_2000k_40_not_abs_neu.RData")
load("selected_2w_2000k_50_not_abs_neu.RData")
load("selected_2w_2000k_60_not_abs_neu.RData")
load("selected_2w_2000k_70_not_abs_neu.RData")
load("selected_2w_2000k_80_not_abs_neu.RData")
load("selected_2w_2000k_90_not_abs_neu.RData")
load("selected_2w_2000k_100_not_abs_neu.RData")
selected_2w_2000k_not_abs <- list()
for(i in 1:length(selected_2w_2000k_10_not_abs)){
  selected_2w_2000k_not_abs[[i]] <- cbind(selected_2w_2000k_10_not_abs[[i]],
                                          selected_2w_2000k_20_not_abs[[i]],
                                          selected_2w_2000k_30_not_abs[[i]],
                                          selected_2w_2000k_40_not_abs[[i]],
                                          selected_2w_2000k_50_not_abs[[i]],
                                          selected_2w_2000k_60_not_abs[[i]],
                                          selected_2w_2000k_70_not_abs[[i]],
                                          selected_2w_2000k_80_not_abs[[i]],
                                          selected_2w_2000k_90_not_abs[[i]],
                                          selected_2w_2000k_100_not_abs[[i]])
}
names(selected_2w_2000k_not_abs) = names(selected_2w_2000k_10_not_abs)
save(selected_2w_2000k_not_abs, file = "selected_2w_2000k_not_abs.RData")
load("selected_2w_2000k_not_abs.RData")

q = 10000
# whole:
whole_ = NA

# RW:
RW_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$RW[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$RW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ RW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ RW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ RW = rep(0,q)
  print("null")}}
  
  RW_ = rbind(RW_, RW)
  print(j)
}

# SW:
SW_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$SW[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$SW[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ SW = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ SW = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ SW = rep(0,q)
  print("null")}}
  
  SW_ = rbind(SW_, SW)
  print(j)
}

# e_0.5
e_0.5_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$e_0.5[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$e_0.5[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.5 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.5 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.5 = rep(0,q)
  print("null")}}
  
  e_0.5_ = rbind(e_0.5_, e_0.5)
  print(j)
}

# e_0.2:
e_0.2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$e_0.2[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$e_0.2[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.2 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.2 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.2 = rep(0,q)
  print("null")}}
  
  e_0.2_ = rbind(e_0.2_, e_0.2)
  print(j)
}

# e_0.1:
e_0.1_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$e_0.1[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$e_0.1[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ e_0.1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ e_0.1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ e_0.1 = rep(0,q)
  print("null")}}
  
  e_0.1_ = rbind(e_0.1_, e_0.1)
  print(j)
}

# corr:
corr_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w_2000k_not_abs$corr[,j] == "SNP1"), 
         which(selected_2w_2000k_not_abs$corr[,j] == "SNP2")) %>% sort()
  if(length(w1) == 2){ corr = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print("zwei") }else{ if(length(w1) == 1){ corr = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print("eins") }else{ corr = rep(0,q)
  print("null")}}
  
  corr_ = rbind(corr_, corr)
  print(j)
}


mean_2w_2000k_not_abs <- data.frame("q" = 2:10000,
                                   "whole" = NA,
                                   "RW" = colMeans(RW_)[-1],
                                   "SW" = colMeans(SW_)[-1],
                                   "corr" = colMeans(corr_)[-1],
                                   "e0.5" = colMeans(e_0.5_)[-1],
                                   "e0.2" = colMeans(e_0.2_)[-1],
                                   "e0.1" = colMeans(e_0.1_)[-1])

# 575:
# median(whole_[,574])
median(RW_[,574])
median(SW_[,574])
median(e_0.5_[,574])
median(e_0.2_[,574])
median(e_0.1_[,574])
median(corr_[,574])



mean_2w_2000k_not_abs = mean_2w_2000k_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w_2000k_not_abs = ggplot(mean_2w_2000k_not_abs, aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 2000000, colour = "expected"), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black") + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = "black") +
  labs(x = "q", y = "important variables found", title = "p=2000000") +
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

ggsave(filename = "figure/plt_mean_2w_2000k_not_abs.pdf", plt_mean_2w_2000k_not_abs, device = "pdf",
       width = 10, height = 5)


plt200k_2000k_not_abs = ggarrange(plt_mean_2w_200k_not_abs, plt_mean_2w_2000k_not_abs,
                                  common.legend = TRUE, legend="bottom")

ggsave(filename = "figure/plt200k_2000k_not_abs.pdf", plt200k_2000k_not_abs, device = "pdf",
       width = 10, height = 5)




plt2w_not_abs = ggarrange(plt_mean_2w_2k_not_abs,
                          plt_mean_2w_20k_not_abs,
                          plt_mean_2w_200k_not_abs, 
                          plt_mean_2w_2000k_not_abs,nrow = 1,
                                  common.legend = TRUE, legend="bottom")

ggsave(filename = "figure/plt2w_not_abs.pdf", plt2w_not_abs, device = "pdf",
       width = 20, height = 5)
