library('tidyverse')
library('ggplot2')
library('ggpubr') ## for ggrange

# Read the functions we need:
source('all_functions.R')

if (!dir.exists("results/2w2w")) {
  dir.create("results/2w2w", recursive = TRUE)
}

#### 2k: ####
load('results/2w2w/selected_2w2w_2k_not_abs_neu.RData')

# count how many in total out of 4:
# for Table 4
q = 575
how_whole = c()
for(i in seq(1,1999,2)){
  how_whole = c(how_whole, sum(any(selected_2w2w_2k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$whole[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$whole[,i][1:q] == 'SNP4')))
  print(i)
}

how_RW = c()
for(i in seq(1,1999,2)){
  how_RW = c(how_RW, sum(any(selected_2w2w_2k_not_abs$RW[,i][1:q] == 'SNP1'), 
                        any(selected_2w2w_2k_not_abs$RW[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$RW[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$RW[,i][1:q] == 'SNP4')))
  print(i)
}


how_SW = c()
for(i in seq(1,1999,2)){
  how_SW = c(how_SW, sum(any(selected_2w2w_2k_not_abs$SW[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$SW[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$SW[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$SW[,i][1:q] == 'SNP4')))
  print(i)
}


how_corr = c()
for(i in seq(1,1999,2)){
  how_corr = c(how_corr, sum(any(selected_2w2w_2k_not_abs$corr[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$corr[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$corr[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$corr[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.5 = c()
for(i in seq(1,1999,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_2w2w_2k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$e_0.5[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$e_0.5[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.2 = c()
for(i in seq(1,1999,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_2w2w_2k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$e_0.2[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$e_0.2[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.1 = c()
for(i in seq(1,1999,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_2w2w_2k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2k_not_abs$e_0.1[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2k_not_abs$e_0.1[,i][1:q] == 'SNP4')))
  print(i)
}




table4a <- c(how_whole %>% median(), how_RW %>% median(), how_SW %>% median(), how_e_0.5 %>% median(),
             how_e_0.2 %>% median(), how_e_0.1 %>% median(), how_corr %>% median())

           

##### Figure 5: ####
# count how many out of 2 we found for each interaction:
q = 1000
# whole:
whole1_ = c()
whole2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$whole[,j] == 'SNP2')) %>% sort()
  whole1 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$whole[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$whole[,j] == 'SNP4')) %>% sort()
  whole2 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  whole1_ = rbind(whole1_, whole1)
  whole2_ = rbind(whole2_, whole2)
  print(j)
}

# RW
RW1_ = c()
RW2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$RW[,j] == 'SNP2')) %>% sort()
  RW1 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$RW[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  RW2 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  RW1_ = rbind(RW1_, RW1)
  RW2_ = rbind(RW2_, RW2)
  print(j)
}


# SW
SW1_ = c()
SW2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$SW[,j] == 'SNP2')) %>% sort()
  SW1 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$SW[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  SW2 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  SW1_ = rbind(SW1_, SW1)
  SW2_ = rbind(SW2_, SW2)
  print(j)
}



# corr:
corr1_ = c()
corr2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$corr[,j] == 'SNP2')) %>% sort()
  corr1 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$corr[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  corr2 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  corr1_ = rbind(corr1_, corr1)
  corr2_ = rbind(corr2_, corr2)
  print(j)
}


# sketch
# e_0.5:
e_0.51_ = c()
e_0.52_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$e_0.5[,j] == 'SNP2')) %>% sort()
  e_0.51 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$e_0.5[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  e_0.52 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  e_0.51_ = rbind(e_0.51_, e_0.51)
  e_0.52_ = rbind(e_0.52_, e_0.52)
  print(j)
}


# e_0.2:
e_0.21_ = c()
e_0.22_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$e_0.2[,j] == 'SNP2')) %>% sort()
  e_0.21 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$e_0.2[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  e_0.22 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  e_0.21_ = rbind(e_0.21_, e_0.21)
  e_0.22_ = rbind(e_0.22_, e_0.22)
  print(j)
}

# e_0.1:
e_0.11_ = c()
e_0.12_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_2k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_2w2w_2k_not_abs$e_0.1[,j] == 'SNP2')) %>% sort()
  e_0.11 = c(rep(0,w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  
  w2 = c(which(selected_2w2w_2k_not_abs$e_0.1[,j] == 'SNP3'), 
         which(selected_2w2w_2k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  e_0.12 = c(rep(0,w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  
  e_0.11_ = rbind(e_0.11_, e_0.11)
  e_0.12_ = rbind(e_0.12_, e_0.12)
  print(j)
}


mean_2w2w_2k_1_not_abs <- data.frame('q' = 2:1000,
                                 'whole' = colMeans(whole1_)[-1],
                                 'RW' = colMeans(RW1_)[-1],
                                 'SW' = colMeans(SW1_)[-1],
                                 'corr' = colMeans(corr1_)[-1],
                                 'e0.5' = colMeans(e_0.51_)[-1],
                                 'e0.2' = colMeans(e_0.21_)[-1],
                                 'e0.1' = colMeans(e_0.11_)[-1])


# Figure 5:
mean_2w2w_2k_1_not_abs = mean_2w2w_2k_1_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_2k_1_not_abs = ggplot(mean_2w2w_2k_1_not_abs, 
                                    aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 2000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'first interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

mean_2w2w_2k_2_not_abs <- data.frame('q' = 2:1000,
                                     'whole' = colMeans(whole2_)[-1],
                                     'RW' = colMeans(RW2_)[-1],
                                     'SW' = colMeans(SW2_)[-1],
                                     'corr' = colMeans(corr2_)[-1],
                                     'e0.5' = colMeans(e_0.52_)[-1],
                                     'e0.2' = colMeans(e_0.22_)[-1],
                                     'e0.1' = colMeans(e_0.12_)[-1])


mean_2w2w_2k_2_not_abs = mean_2w2w_2k_2_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_2k_2_not_abs = ggplot(mean_2w2w_2k_2_not_abs, 
                                    aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 2000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'second interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

plt_2w2w_2k_not_abs = ggarrange(plt_mean_2w2w_2k_1_not_abs,plt_mean_2w2w_2k_2_not_abs,
                              common.legend = TRUE, legend='bottom')


ggsave(filename = 'plots/Fig05_2w2w_2k_not_abs.eps', plt_2w2w_2k_not_abs, 
       device = cairo_ps,
       width = 10, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)




#### 20k: ####
load('results/2w2w/selected_2w2w_20k_not_abs_neu.RData')

# for Table 4:
# how many out of 4:
q = 575
how_whole = c()
for(i in seq(1,1999,2)){
  how_whole = c(how_whole, sum(any(selected_2w2w_20k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_20k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_20k_not_abs$whole[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_20k_not_abs$whole[,i][1:q] == 'SNP4')))
  print(i)
}


how_RW = c()
for(i in seq(1,1999,2)){
  how_RW = c(how_RW, sum(any(selected_2w2w_20k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_20k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_20k_not_abs$RW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_20k_not_abs$RW[,i][1:q] == 'SNP4')))
  print(i)
}


how_SW = c()
for(i in seq(1,1999,2)){
  how_SW = c(how_SW, sum(any(selected_2w2w_20k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_20k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_20k_not_abs$SW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_20k_not_abs$SW[,i][1:q] == 'SNP4')))
  print(i)
}


how_corr = c()
for(i in seq(1,1999,2)){
  how_corr = c(how_corr, sum(any(selected_2w2w_20k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_2w2w_20k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_2w2w_20k_not_abs$corr[,i][1:q] == 'SNP3'), 
                             any(selected_2w2w_20k_not_abs$corr[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.5 = c()
for(i in seq(1,1999,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_2w2w_20k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_20k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_20k_not_abs$e_0.5[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_20k_not_abs$e_0.5[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.2 = c()
for(i in seq(1,1999,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_2w2w_20k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_20k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_20k_not_abs$e_0.2[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_20k_not_abs$e_0.2[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.1 = c()
for(i in seq(1,1999,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_2w2w_20k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_20k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_20k_not_abs$e_0.1[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_20k_not_abs$e_0.1[,i][1:q] == 'SNP4')))
  print(i)
}



table4b <- c(how_whole %>% median(), how_RW %>% median(), how_SW %>% median(), how_e_0.5 %>% median(),
             how_e_0.2 %>% median(), how_e_0.1 %>% median(), how_corr %>% median())


# each interaction:
##### Figure 10 ####
q = 1000
# whole:
whole1_ = c()
whole2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$whole[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){whole1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){whole1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{whole1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$whole[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$whole[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){whole2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){whole2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{whole2 = rep(0,q)
  print('null')}}
  
  whole1_ = rbind(whole1_, whole1)
  whole2_ = rbind(whole2_, whole2)
  print(j)
}

# RW:
RW1_ = c()
RW2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$RW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){RW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){RW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{RW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$RW[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){RW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){RW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{RW2 = rep(0,q)
  print('null')}}
  
  RW1_ = rbind(RW1_, RW1)
  RW2_ = rbind(RW2_, RW2)
  print(j)
}


# SW
SW1_ = c()
SW2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$SW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){SW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){SW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{SW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$SW[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){SW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){SW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{SW2 = rep(0,q)
  print('null')}}
  
  SW1_ = rbind(SW1_, SW1)
  SW2_ = rbind(SW2_, SW2)
  print(j)
}



# corr:
corr1_ = c()
corr2_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$corr[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){corr1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){corr1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{corr1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$corr[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){corr2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){corr2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{corr2 = rep(0,q)
  print('null')}}
  
  corr1_ = rbind(corr1_, corr1)
  corr2_ = rbind(corr2_, corr2)
  print(j)
}


# sketch e_0.5:
e_0.51_ = c()
e_0.52_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$e_0.5[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.51 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.51 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.51 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$e_0.5[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.52 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.52 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.52 = rep(0,q)
  print('null')}}
  
  e_0.51_ = rbind(e_0.51_, e_0.51)
  e_0.52_ = rbind(e_0.52_, e_0.52)
  print(j)
}


# e_0.2:
e_0.21_ = c()
e_0.22_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$e_0.2[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.21 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.21 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.21 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$e_0.2[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.22 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.22 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.22 = rep(0,q)
  print('null')}}
  
  e_0.21_ = rbind(e_0.21_, e_0.21)
  e_0.22_ = rbind(e_0.22_, e_0.22)
  print(j)
}

# e_0.1:
e_0.11_ = c()
e_0.12_ = c()
for(j in seq(1,1999,2)){
  
  w1 = c(which(selected_2w2w_20k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_2w2w_20k_not_abs$e_0.1[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.11 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.11 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.11 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_20k_not_abs$e_0.1[,j] == 'SNP3'),
         which(selected_2w2w_20k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.12 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.12 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.12 = rep(0,q)
  print('null')}}
  
  e_0.11_ = rbind(e_0.11_, e_0.11)
  e_0.12_ = rbind(e_0.12_, e_0.12)
  print(j)
}


mean_2w2w_20k_1_not_abs <- data.frame('q' = 2:1000,
                                       'whole' = colMeans(whole1_)[-1],
                                       'RW' = colMeans(RW1_)[-1],
                                       'SW' = colMeans(SW1_)[-1],
                                       'corr' = colMeans(corr1_)[-1],
                                       'e0.5' = colMeans(e_0.51_)[-1],
                                       'e0.2' = colMeans(e_0.21_)[-1],
                                       'e0.1' = colMeans(e_0.11_)[-1])


mean_2w2w_20k_1_not_abs = mean_2w2w_20k_1_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_20k_1_not_abs = ggplot(mean_2w2w_20k_1_not_abs,
                                     aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 20000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'first interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

mean_2w2w_20k_2_not_abs <- data.frame('q' = 2:1000,
                                       'whole' = colMeans(whole2_)[-1],
                                       'RW' = colMeans(RW2_)[-1],
                                       'SW' = colMeans(SW2_)[-1],
                                       'corr' = colMeans(corr2_)[-1],
                                       'e0.5' = colMeans(e_0.52_)[-1],
                                       'e0.2' = colMeans(e_0.22_)[-1],
                                       'e0.1' = colMeans(e_0.12_)[-1])


mean_2w2w_20k_2_not_abs = mean_2w2w_20k_2_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_20k_2_not_abs = ggplot(mean_2w2w_20k_2_not_abs,
                                     aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'second interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

plt_2w2w_20k_not_abs = ggarrange(plt_mean_2w2w_20k_1_not_abs,plt_mean_2w2w_20k_2_not_abs,
                                  common.legend = TRUE, legend='bottom')


ggsave(filename = 'plots/Fig10_2w2w_20k_not_abs.eps', plt_2w2w_20k_not_abs, 
       device = cairo_ps,
       width = 10, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)



#### 200k: ####
load('results/2w2w/selected_2w2w_200k_not_abs.RData')

# for Table 4: 
# how many out of 4:
q = 575
how_whole = c()
for(i in seq(1,199,2)){
  how_whole = c(how_whole, sum(any(selected_2w2w_200k_not_abs$whole[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_200k_not_abs$whole[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_200k_not_abs$whole[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_200k_not_abs$whole[,i][1:q] == 'SNP4')))
  print(i)
}


how_RW = c()
for(i in seq(1,199,2)){
  how_RW = c(how_RW, sum(any(selected_2w2w_200k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_200k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_200k_not_abs$RW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_200k_not_abs$RW[,i][1:q] == 'SNP4')))
  print(i)
}


how_SW = c()
for(i in seq(1,199,2)){
  how_SW = c(how_SW, sum(any(selected_2w2w_200k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_200k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_200k_not_abs$SW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_200k_not_abs$SW[,i][1:q] == 'SNP4')))
  print(i)
}


how_corr = c()
for(i in seq(1,199,2)){
  how_corr = c(how_corr, sum(any(selected_2w2w_200k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_2w2w_200k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_2w2w_200k_not_abs$corr[,i][1:q] == 'SNP3'), 
                             any(selected_2w2w_200k_not_abs$corr[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.5 = c()
for(i in seq(1,199,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_2w2w_200k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_200k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_200k_not_abs$e_0.5[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_200k_not_abs$e_0.5[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.2 = c()
for(i in seq(1,199,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_2w2w_200k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_200k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_200k_not_abs$e_0.2[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_200k_not_abs$e_0.2[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.1 = c()
for(i in seq(1,199,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_2w2w_200k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_200k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_200k_not_abs$e_0.1[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_200k_not_abs$e_0.1[,i][1:q] == 'SNP4')))
  print(i)
}


table4c <- c(how_whole %>% median(), how_RW %>% median(), how_SW %>% median(), how_e_0.5 %>% median(),
             how_e_0.2 %>% median(), how_e_0.1 %>% median(), how_corr %>% median())

##### Figure 11: ####
# how many out of 2 for each interaction:
q = 10000
# whole:
whole1_ = c()
whole2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$whole[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$whole[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){whole1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){whole1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{whole1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$whole[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$whole[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){whole2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){whole2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{whole2 = rep(0,q)
  print('null')}}
  
  whole1_ = rbind(whole1_, whole1)
  whole2_ = rbind(whole2_, whole2)
  print(j)
}

# RW:
RW1_ = c()
RW2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$RW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){RW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){RW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{RW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$RW[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){RW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){RW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{RW2 = rep(0,q)
  print('null')}}
  
  RW1_ = rbind(RW1_, RW1)
  RW2_ = rbind(RW2_, RW2)
  print(j)
}


# SW
SW1_ = c()
SW2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$SW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){SW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){SW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{SW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$SW[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){SW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){SW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{SW2 = rep(0,q)
  print('null')}}
  
  SW1_ = rbind(SW1_, SW1)
  SW2_ = rbind(SW2_, SW2)
  print(j)
}


# corr:
corr1_ = c()
corr2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$corr[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){corr1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){corr1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{corr1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$corr[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){corr2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){corr2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{corr2 = rep(0,q)
  print('null')}}
  
  corr1_ = rbind(corr1_, corr1)
  corr2_ = rbind(corr2_, corr2)
  print(j)
}


# sketch 0.5
e_0.51_ = c()
e_0.52_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$e_0.5[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.51 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.51 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.51 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$e_0.5[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.52 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.52 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.52 = rep(0,q)
  print('null')}}
  
  e_0.51_ = rbind(e_0.51_, e_0.51)
  e_0.52_ = rbind(e_0.52_, e_0.52)
  print(j)
}


# e_0.2:
e_0.21_ = c()
e_0.22_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$e_0.2[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.21 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.21 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.21 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$e_0.2[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.22 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.22 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.22 = rep(0,q)
  print('null')}}
  
  e_0.21_ = rbind(e_0.21_, e_0.21)
  e_0.22_ = rbind(e_0.22_, e_0.22)
  print(j)
}

# e_0.1:
e_0.11_ = c()
e_0.12_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_200k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_2w2w_200k_not_abs$e_0.1[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.11 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.11 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.11 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_200k_not_abs$e_0.1[,j] == 'SNP3'),
         which(selected_2w2w_200k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.12 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.12 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.12 = rep(0,q)
  print('null')}}
  
  e_0.11_ = rbind(e_0.11_, e_0.11)
  e_0.12_ = rbind(e_0.12_, e_0.12)
  print(j)
}



mean_2w2w_200k_1_not_abs <- data.frame('q' = 2:10000,
                                     'whole' = colMeans(whole1_)[-1],
                                     'RW' = colMeans(RW1_)[-1],
                                     'SW' = colMeans(SW1_)[-1],
                                     'corr' = colMeans(corr1_)[-1],
                                     'e0.5' = colMeans(e_0.51_)[-1],
                                     'e0.2' = colMeans(e_0.21_)[-1],
                                     'e0.1' = colMeans(e_0.11_)[-1])


mean_2w2w_200k_1_not_abs = mean_2w2w_200k_1_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_200k_1_not_abs = ggplot(mean_2w2w_200k_1_not_abs, 
                                      aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'first interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

mean_2w2w_200k_2_not_abs <- data.frame('q' = 2:10000,
                                     'whole' = colMeans(whole2_)[-1],
                                     'RW' = colMeans(RW2_)[-1],
                                     'SW' = colMeans(SW2_)[-1],
                                     'corr' = colMeans(corr2_)[-1],
                                     'e0.5' = colMeans(e_0.52_)[-1],
                                     'e0.2' = colMeans(e_0.22_)[-1],
                                     'e0.1' = colMeans(e_0.12_)[-1])


mean_2w2w_200k_2_not_abs = mean_2w2w_200k_2_not_abs %>% 
  pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_200k_2_not_abs = ggplot(mean_2w2w_200k_2_not_abs, 
                                      aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'second interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

plt_2w2w_200k_not_abs = ggarrange(plt_mean_2w2w_200k_1_not_abs,plt_mean_2w2w_200k_2_not_abs,
                                common.legend = TRUE, legend='bottom')


ggsave(filename = 'plots/Fig11_2w2w_200k_not_abs.eps', plt_2w2w_200k_not_abs, 
       device = cairo_ps,
       width = 10, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)




#### 2000k: ####
load('results/2w2w/selected_2w2w_2000k_not_abs_10.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_20.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_30.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_40.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_50.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_60.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_70.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_80.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_90.RData')
load('results/2w2w/selected_2w2w_2000k_not_abs_100.RData')


selected_2w2w_2000k_not_abs = list()
for(i in 1:7){
selected_2w2w_2000k_not_abs[[i]] = cbind(selected_2w2w_2000k_not_abs_10[[i]],
                                       selected_2w2w_2000k_not_abs_20[[i]],
                                       selected_2w2w_2000k_not_abs_30[[i]],
                                       selected_2w2w_2000k_not_abs_40[[i]],
                                       selected_2w2w_2000k_not_abs_50[[i]],
                                       selected_2w2w_2000k_not_abs_60[[i]],
                                       selected_2w2w_2000k_not_abs_70[[i]],
                                       selected_2w2w_2000k_not_abs_80[[i]],
                                       selected_2w2w_2000k_not_abs_90[[i]],
                                       selected_2w2w_2000k_not_abs_100[[i]])}
names(selected_2w2w_2000k_not_abs) = names(selected_2w2w_2000k_not_abs_10)
save(selected_2w2w_2000k_not_abs,
      file = 'reslults/2w2w/selected_2w2w_2000k_not_abs.RData')


load('results/2w2w/selected_2w2w_2000k_not_abs.RData')


# for Table 4
# how many out of 4:
q = 575


how_RW = c()
for(i in seq(1,199,2)){
  how_RW = c(how_RW, sum(any(selected_2w2w_2000k_not_abs$RW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_2000k_not_abs$RW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_2000k_not_abs$RW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_2000k_not_abs$RW[,i][1:q] == 'SNP4')))
  print(i)
}


how_SW = c()
for(i in seq(1,199,2)){
  how_SW = c(how_SW, sum(any(selected_2w2w_2000k_not_abs$SW[,i][1:q] == 'SNP1'), 
                         any(selected_2w2w_2000k_not_abs$SW[,i][1:q] == 'SNP2'),
                         any(selected_2w2w_2000k_not_abs$SW[,i][1:q] == 'SNP3'), 
                         any(selected_2w2w_2000k_not_abs$SW[,i][1:q] == 'SNP4')))
  print(i)
}


how_corr = c()
for(i in seq(1,199,2)){
  how_corr = c(how_corr, sum(any(selected_2w2w_2000k_not_abs$corr[,i][1:q] == 'SNP1'), 
                             any(selected_2w2w_2000k_not_abs$corr[,i][1:q] == 'SNP2'),
                             any(selected_2w2w_2000k_not_abs$corr[,i][1:q] == 'SNP3'), 
                             any(selected_2w2w_2000k_not_abs$corr[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.5 = c()
for(i in seq(1,199,2)){
  how_e_0.5 = c(how_e_0.5, sum(any(selected_2w2w_2000k_not_abs$e_0.5[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2000k_not_abs$e_0.5[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2000k_not_abs$e_0.5[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2000k_not_abs$e_0.5[,i][1:q] == 'SNP4')))
  print(i)
}

how_e_0.2 = c()
for(i in seq(1,199,2)){
  how_e_0.2 = c(how_e_0.2, sum(any(selected_2w2w_2000k_not_abs$e_0.2[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2000k_not_abs$e_0.2[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2000k_not_abs$e_0.2[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2000k_not_abs$e_0.2[,i][1:q] == 'SNP4')))
  print(i)
}


how_e_0.1 = c()
for(i in seq(1,199,2)){
  how_e_0.1 = c(how_e_0.1, sum(any(selected_2w2w_2000k_not_abs$e_0.1[,i][1:q] == 'SNP1'), 
                               any(selected_2w2w_2000k_not_abs$e_0.1[,i][1:q] == 'SNP2'),
                               any(selected_2w2w_2000k_not_abs$e_0.1[,i][1:q] == 'SNP3'), 
                               any(selected_2w2w_2000k_not_abs$e_0.1[,i][1:q] == 'SNP4')))
  print(i)
}

table4d <- c(how_whole %>% median(), how_RW %>% median(), how_SW %>% median(), how_e_0.5 %>% median(),
             how_e_0.2 %>% median(), how_e_0.1 %>% median(), how_corr %>% median())


##### Figure 12: ####

# how many out of two for each interaction:
q = 10000
# whole:

# NA


# RW:
RW1_ = c()
RW2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$RW[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$RW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){RW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){RW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{RW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$RW[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$RW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){RW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){RW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{RW2 = rep(0,q)
  print('null')}}
  
  RW1_ = rbind(RW1_, RW1)
  RW2_ = rbind(RW2_, RW2)
  print(j)
}


# SW
SW1_ = c()
SW2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$SW[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$SW[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){SW1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){SW1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{SW1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$SW[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$SW[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){SW2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){SW2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{SW2 = rep(0,q)
  print('null')}}

  SW1_ = rbind(SW1_, SW1)
  SW2_ = rbind(SW2_, SW2)
  print(j)
}



# corr:
corr1_ = c()
corr2_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$corr[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$corr[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){corr1 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){corr1 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{corr1 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$corr[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$corr[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){corr2 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){corr2 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{corr2 = rep(0,q)
  print('null')}}
  
  corr1_ = rbind(corr1_, corr1)
  corr2_ = rbind(corr2_, corr2)
  print(j)
}



# sketch 0.5
e_0.51_ = c()
e_0.52_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$e_0.5[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$e_0.5[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.51 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.51 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.51 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$e_0.5[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$e_0.5[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.52 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.52 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.52 = rep(0,q)
  print('null')}}
  
  e_0.51_ = rbind(e_0.51_, e_0.51)
  e_0.52_ = rbind(e_0.52_, e_0.52)
  print(j)
}


# e_0.2:
e_0.21_ = c()
e_0.22_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$e_0.2[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$e_0.2[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.21 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.21 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.21 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$e_0.2[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$e_0.2[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.22 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.22 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.22 = rep(0,q)
  print('null')}}
  
  e_0.21_ = rbind(e_0.21_, e_0.21)
  e_0.22_ = rbind(e_0.22_, e_0.22)
  print(j)
}

# e_0.1:
e_0.11_ = c()
e_0.12_ = c()
for(j in seq(1,199,2)){
  
  w1 = c(which(selected_2w2w_2000k_not_abs$e_0.1[,j] == 'SNP1'), 
         which(selected_2w2w_2000k_not_abs$e_0.1[,j] == 'SNP2')) %>% sort()
  if(length(w1) == 2){e_0.11 = c(rep(0, w1[1]-1), rep(1, w1[2]-w1[1]), rep(2, max(0,(q-w1[2]+1))))[1:q]
  print('zwei')}else{if(length(w1) == 1){e_0.11 = c(rep(0, w1[1]-1), rep(1, max(0,(q-w1[1]+1))))[1:q]
  print('eins')}else{e_0.11 = rep(0,q)
  print('null')}}
  
  w2 = c(which(selected_2w2w_2000k_not_abs$e_0.1[,j] == 'SNP3'),
         which(selected_2w2w_2000k_not_abs$e_0.1[,j] == 'SNP4')) %>% sort()
  if(length(w2) == 2){e_0.12 = c(rep(0, w2[1]-1), rep(1, w2[2]-w2[1]), rep(2, max(0,(q-w2[2]+1))))[1:q]
  print('zwei')}else{if(length(w2) == 1){e_0.12 = c(rep(0, w2[1]-1), rep(1, max(0,(q-w2[1]+1))))[1:q]
  print('eins')}else{e_0.12 = rep(0,q)
  print('null')}}
  
  e_0.11_ = rbind(e_0.11_, e_0.11)
  e_0.12_ = rbind(e_0.12_, e_0.12)
  print(j)
}


mean_2w2w_2000k_1_not_abs <- data.frame('q' = 2:10000,
                                       'whole' = NA,
                                       'RW' = colMeans(RW1_)[-1],
                                       'SW' = colMeans(SW1_)[-1],
                                       'corr' = colMeans(corr1_)[-1],
                                       'e0.5' = colMeans(e_0.51_)[-1],
                                       'e0.2' = colMeans(e_0.21_)[-1],
                                       'e0.1' = colMeans(e_0.11_)[-1])


mean_2w2w_2000k_1_not_abs = mean_2w2w_2000k_1_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_2000k_1_not_abs = ggplot(mean_2w2w_2000k_1_not_abs, 
                                       aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'first interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole')) +
  theme(plot.margin = margin(10,10,10,10))

mean_2w2w_2000k_2_not_abs <- data.frame('q' = 2:10000,
                                       'whole' = NA,
                                       'RW' = colMeans(RW2_)[-1],
                                       'SW' = colMeans(SW2_)[-1],
                                       'corr' = colMeans(corr2_)[-1],
                                       'e0.5' = colMeans(e_0.52_)[-1],
                                       'e0.2' = colMeans(e_0.22_)[-1],
                                       'e0.1' = colMeans(e_0.12_)[-1])


mean_2w2w_2000k_2_not_abs = mean_2w2w_2000k_2_not_abs %>% pivot_longer(cols = c(whole, RW, SW, e0.5, e0.2, e0.1, corr))

plt_mean_2w2w_2000k_2_not_abs = ggplot(mean_2w2w_2000k_2_not_abs, 
                                       aes(x = q, y = value, color = name)) + 
  geom_line(linewidth = 1) + 
  geom_line(aes(x = q, y = 2*q / 200000, colour = 'expected'), linewidth = 0.75) +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  geom_vline(xintercept = 575, linewidth = 0.5, lty =3, colour = 'black') +
  labs(x = 'q', y = 'important variables found', title = 'second interaction') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               e0.1 = 'violetred2',
                               e0.2 = 'violetred',
                               e0.5 = 'violet',  
                               expected = 'grey',
                               RW = 'firebrick',
                               SW = 'navy',
                               whole = 'darkgreen'),
                      labels = c('corr', 
                                 expression('Sketching'~(epsilon~'='~0.1)), 
                                 expression('Sketching'~(epsilon~'='~0.2)),
                                 expression('Sketching'~(epsilon~'='~0.5)), 'expected',
                                 'RW', 'SW', 'whole'))

plt_2w2w_2000k_not_abs = ggarrange(plt_mean_2w2w_2000k_1_not_abs,plt_mean_2w2w_2000k_2_not_abs,
                                  common.legend = TRUE, legend='bottom') +
  theme(plot.margin = margin(10,10,10,10))

# Figure 12:
ggsave(filename = 'plots/Fig12_2w2w_2000k_not_abs.eps', plt_2w2w_2000k_not_abs, 
       device = cairo_ps,
       width = 10, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)



#### Table 4: ####
 
Table04 <- rbind(table4a, table4b, table4c, table4d)
Table04 <- as.data.frame(cbind(c("p=2000", "p=20000", "p=200000", "p=2000000"), Table04))
colnames(Table04) <- c("p", "CLS", "RW", "SW", "0.5", "0.2", "0.1",
                       "correlation")

write.table(Table04, file = "results/2w2w/Table04.txt", sep = ";", row.names = F, quote = F)
