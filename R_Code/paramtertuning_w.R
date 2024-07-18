### influence or the parameter w in the RW and SW approach: ####
library(tidyverse)

# read in the functions we need:
source('all_functions.R')




#### 20k ####
# read in the data
load("../Data/Szen2w_20k.RData")


set.seed(161)

h <- c()
r <- c()
for(i in 1:1000){
  h_ <- c()
  r_ <- c()
  for(j in c(200,500,1000,1250,2000,2500,4000,5000,10000, 20000)){ # different sizes for w
    xyz <- how_many2(Data = cbind(Szen2w_20k[[i]]$x, Szen2w_20k[[i]]$y), 
                     res_number = 20001,abs = F,
                     whole = F, SW=T, RW = T, R = 1000, w_RW = j, corr_ = F, sketch = F,
                     w_SW = j)
    q = 575
    h_ <- c(h_, sum(any(xyz$p_SW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_SW$SNP[1:q] == "SNP2")))
    r_ <- c(r_, sum(any(xyz$p_RW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_RW$SNP[1:q] == "SNP2")))}
  
  h <- rbind(h, h_)
  r <- rbind(r, r_)
  print(i)
  save(h,r, file = "results/parametertuning/w_2w_20k.RData")
  
}

# Figure 14a)
load("results/parametertuning/w_2w_20k.RData")

w <- c(200,500,1000,1250,2000,2500,4000,5000,10000, 20000)
sliding_20k <- colMeans(h)
random_20k <- colMeans(r)

w_20k <- data.frame(w, sliding_20k, random_20k)
names(w_20k) <- c("w", "SW", "RW")

# plot:
w_20k_l <- pivot_longer(w_20k, cols = c(SW, RW))

w_20k_plot <- ggplot(w_20k_l, aes(x = w, y = value, color = name)) +
  geom_point() +
  geom_line() +
  # scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(name="Method:",values = c("SW" = "navy", "RW" = "firebrick")) +
  #  geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black")
  labs(x = expression(italic(w)), y = "important variables found", 
       title = expression(paste(italic(p) == 20000))) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size =14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  theme(plot.margin = margin(10,10,10,10))



#### 200k ####
# read in the data
load("../Data/Szen2w_200k.RData")

set.seed(132)

h <- c()
r <- c()
for(i in 1:100){
  h_ <- c()
  r_ <- c()
  for(j in c(200,500,1000,1250,2000,2500,4000,5000,10000, 20000)){ # different sizes for w
    xyz <- how_many2(Data = cbind(Szen2w_200k[[i]]$x, Szen2w_200k[[i]]$y), 
                     res_number = 200001,abs = F,
                     whole = F, SW=T, RW = T, R = 1000, w_RW = j, corr_ = F, sketch = F,
                     w_SW = j)
    q = 575
    h_ <- c(h_, sum(any(xyz$p_SW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_SW$SNP[1:q] == "SNP2")))
    r_ <- c(r_, sum(any(xyz$p_RW$SNP[1:q] == "SNP1"), 
                    any(xyz$p_RW$SNP[1:q] == "SNP2")))}
  
  h <- rbind(h, h_)
  r <- rbind(r, r_)
  print(i)
  save(h,r, file = "results/parametertuning/w_2w_200k.RData")
  
}

# Figure 14 b)
load("results/parametertuning/w_2w_200k.RData")

w <- c(200,500,1000,1250,2000,2500,4000,5000,10000, 20000)
sliding_200k <- colMeans(h)
random_200k <- colMeans(r)

w_200k <- data.frame(w, sliding_200k, random_200k)
names(w_200k) <- c("w", "SW", "RW")

w_200k_l <- pivot_longer(w_200k, cols = c(SW, RW))

w_200k_plot <- ggplot(w_200k_l, aes(x = w, y = value, color = name)) +
  geom_point() +
  geom_line() +
  # scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c(name="Method:", "SW" = "navy", "RW" = "firebrick")) +
  # geom_hline(yintercept = 2, linewidth = 0.5, lty =3, colour = "black")
  labs(x = "w", y = "important variables found", title = "p=200000") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 19),
        legend.position = "bottom") +
  theme(plot.margin = margin(10,10,10,10))


##### Figure 14 ####
w_plot <- ggarrange(
  w_20k_plot, w_200k_plot,
  ncol = 2, 
  nrow = 1,  
  common.legend = TRUE,  
  legend = "bottom", 
  widths = c(1, 1)
)


ggsave("plots/Fig14_w_tuning.eps", plot = w_plot, device = cairo_ps,
       width = 8.2, height = 5, units = "in",
       dpi = 300,
       limitsize = FALSE)
