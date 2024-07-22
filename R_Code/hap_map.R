## here i test the methods on the hapmap data
library('ranger')
library('logicFS')
library('SNPassoc')
library('tidyverse')
library('GLDEX') # for wich.na
library('randomForest')


# read the funtions we need
source('all_functions.R')

#### Hap Map ####
data('HapMap')


#### pre ####
HapMapSNP_original <- HapMap[,-c(1:2)]
responseHapMap <- as.numeric(HapMap$group == 'CEU')
# make numeric: 
HapMapSNP_add <- apply(HapMapSNP_original, MARGIN = 2, FUN = additive) 
# missing values:
length(which(is.na(HapMapSNP_add) == TRUE)) # 49002
dim(HapMapSNP_add) # dims multiplied is 1116600
49002/1116600  # 0.04388

# interpolate:
mod_interpol <- function(x) {
  ind <- which(is.na(x) == TRUE)
  ## make sure that levels 0, 1, and 2 are included (SNP data):
  probs = table(factor(x, levels = 0:2))  
  imputedata = sample(0:2, length(ind), prob = probs, replace = TRUE)
  x[ind] <- imputedata
  return(x)
}

HapMapSNP_all <- apply(HapMapSNP_add, MARGIN = 2, FUN = mod_interpol) # imputation
colnames(HapMapSNP_all) = names(HapMapSNP_original)
ind <- which(colSums(HapMapSNP_all) == 0) # 1657 entries
HapMapSNP <- HapMapSNP_all[, -ind]  # 7648 vars left



#### Table 05: ####

# calculate the prediction error in a random Forest model

# train and test data set:
set.seed(162)
train_ <- sample(1:120, 80, F)

train.hapmap <- H[train_, ]
test.hapmap <- H[-train_, ]

# without variable selection
rr.hapmap <- ranger(y~., train.hapmap)
pred.hapmap <- predict(rr.hapmap, test.hapmap)
table(test.hapmap$y, pred.hapmap$predictions)

# arbitary selection
set.seed(1909)
index.arbitrary = sample(1:7648, 10)

rr.arbitrary = ranger(x = train.hapmap[, index.arbitrary], y = train.hapmap[,7649])

pred.arbitrary <- predict(rr.arbitrary, test.hapmap[, index.arbitrary])
table(test.hapmap$y, pred.arbitrary$predictions)

# variable selection with CLS
ccc = getCLS(train.hapmap)

index.cls = which(rank(-ccc) < 10)

rr.cls = ranger(x = train.hapmap[, index.cls], y = train.hapmap[,7649])

pred.cls <- predict(rr.cls, test.hapmap[, index.cls])
table(test.hapmap$y, pred.cls$predictions)

# variable selection with correlation
y_numeric = as.numeric(train.hapmap[,7649]) - 1
corr_ = apply(train.hapmap[,-7649], 2, function(x) cor(x, y_numeric))

index.corr = which(rank(-corr_) < 10)

rr.corr = ranger(x = train.hapmap[, index.corr], y = train.hapmap[,7649])

pred.corr <- predict(rr.corr, test.hapmap[, index.corr])
table(test.hapmap$y, pred.corr$predictions)
