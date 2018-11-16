# Correlation analysis of MB types and a clustering based on the correlation matrix.

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/mba/") # edit to suit your environment
source("./src/mb-utils.r")

#### Do Analysis ####

MB <- loadMB()

#### Correlation analysis of MB ####
MB.numeric <- normColumns(select(MB, AP, APM, APF, APGD), c("AP", "APM", "APF", "APGD"))

dat <- as.matrix(MB.numeric)
# edit the following line to try different methods
method <- "COR" # "COR" "FRECHET" "DTWARP" "INT.PER" "DTW" "DWT" "AR.PIC"
# note: FRECHET is very computationally intensive (it can crash RStudio even with moderate data size)

# across observations (types)
rDist <- diss(dat, method)
hc <- hclust(rDist)
ggdendrogram(hc)
heatmap(as.matrix(rDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)


