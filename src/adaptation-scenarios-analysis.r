# Analysis of the adaptation scenarios

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/mba/") # edit to suit your environment
source("./src/mb-utils.r")

#### Do Analysis ####

MB <- loadMB()

# generate stats for all possible adaptation scenarios
MBS <- getScenarios(MB)

# N_ is a count of the undefined parameters, with '_' symbol in the mask
# execute one line only from below
# MBSg <- select(MBS, GroupAP, Ratio, AP0, AP1, Diff, N_, DP, RP, TP)
# MBSg <- select(MBS, GroupAP, Ratio, Diff, N_, DP, RP)
# MBSg <- select(MBS, GroupAP, Diff, N_)
# MBSg <- select(MBS, GroupAP, Ratio, Diff)
# MBSg <- select(MBS, GroupAP, Ratio, N_)
# MBSg <- select(MBS, Ratio, Diff, N_)
# MBSg <- select(MBS, Ratio, Diff)
# MBSg <- select(MBS, DP, RP)
# MBSg <- select(MBS, DP)
# MBSg <- select(MBS, RP)
# MBSg <- select(MBS, TP)
# MBSg <- select(MBS, DP, RP, TP)
# MBSg <- select(MBS, DP, TP)
# MBSg <- select(MBS, DP, TP, N_)
cols <- c("DP", "TP", "N_")

gDist <- daisy(MBS[, cols], metric = "gower", stand = TRUE)
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)
plotClusters(gDist,MBSg[, cols], 9)

MBSg <- MBS[order(MBS$Diff),]
sum(MBSg$GroupAP)

MBSg0 <- splitByN_(MBSg, 0)
MBSg1 <- splitByN_(MBSg, 1)
MBSg2 <- splitByN_(MBSg, 2)
MBSg3 <- splitByN_(MBSg, 3)

nrow(MBSg0)
sum(MBSg0$GroupAP)
MBSg0
gDist <- daisy(MBSg0, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)

nrow(MBSg1)
sum(MBSg1$GroupAP)
MBSg1

gDist <- daisy(MBSg1, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)

nrow(MBSg2)
sum(MBSg2$GroupAP)
MBSg2[getMasks("","N","",""),]

gDist <- daisy(MBSg2, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)

nrow(MBSg3)
sum(MBSg3$GroupAP)
MBSg3

gDist <- daisy(MBSg3, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

