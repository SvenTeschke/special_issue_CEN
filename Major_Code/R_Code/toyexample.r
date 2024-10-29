# toyexample:
library('tidyverse')
library('ggplot2')
library('ggpubr') ## for ggrange


# load all functions:
source('all_functions.R')



# Here we calculate the toyexample decsribed in the paper in section 2

# Create the example data matrix and calculate the scores (1000 times)

set.seed(1641)
cc <- c()
sc <- c()
for(i in 1:1000){
  X = matrix(NA, ncol=61, nrow= 16) # construct the data matrix
  n = nrow(X)
  draw = ceiling(n*log(n))
  p = ncol(X)-1
  X[,1] = c(rep(1,2), rep(1,2), rep(0,2), rep(0,2), rep(1,2), rep(1,2), rep(0,2), rep(0,2))
  X[,2] = c(rep(1,2), rep(1,2), rep(0,2), rep(0,2), rep(0,2), rep(0,2), rep(1,2), rep(1,2))
  X[,3] = c(rep(0,2), rep(1,2), rep(1,2), rep(1,2), rep(0,2), rep(1,2), rep(0,2), rep(1,2))
  X[,4] = c(rep(1,2), rep(0,2), rep(1,2), rep(1,2), rep(1,2), rep(0,2),rep(1,2), rep(0,2))
  X[,p+1] = X[,1] & X[,2] | X[,3] & X[,4]  # response variable
  for(u in 5:p){
    X[,u] = sample(c(0, 1), 16, replace = TRUE)
  }
  cors = abs(cor(X)[p+1,-(p+1)]) # calculate correlation in comparison
  scores = getCLS(X) # calculate the CLS
  
  cc_ <- c()
  sc_ <- c()
  for(l in 2:draw){
  cc_ <- c(cc_, sum((rank(-cors) <= l)[1:2])) # just consider the first 2 variables:
  sc_ <- c(sc_, sum((rank(-scores) <= l)[1:2]))
  }
  
  cc = rbind(cc, cc_)
  sc = rbind(sc, sc_)
  
  print(i)
}


toy = data.frame('q' = 2:45,
                 'corr' = colMeans(cc),
                 'cls' = colMeans(sc))
toy <- toy %>% pivot_longer(cols = c(corr, cls))


##### Figure 08: #####
toy_plot = ggplot(toy, aes(x= q, y = value, color = name)) +
  geom_line(linewidth = 1) + 
  labs(x = expression(q), y = 'important variables found (mean)', 
       title = 'How many important variables are found?') +
  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = 'black') + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = 'bottom') +
  scale_colour_manual(name='Method:',
                      values=c(corr = 'yellow', 
                               cls =  'darkgreen'),
                      labels = c('CLS', 'corr'))



ggsave(filename = 'plots/Fig08_toyplot.eps', toy_plot, device = cairo_ps,
       width = 8.2, height = 5, units = 'in',  dpi = 300,  limitsize  = FALSE)




