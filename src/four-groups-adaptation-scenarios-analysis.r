# Analysis of MB types into four groups based on parameters and percentage representation

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/analysis/") # edit to suit your environment
source("./src/mb-utils.r")

#### Cluster Analysis ####

MB <- loadMB()

(MB.sorted <- MB[order(MB$AP), ])

(g_N_J <- filter(MB, SD == "N" & OI == "J"))
sum(g_N_J$AP)
(g__TP <- filter(MB, SI == "T" & OI == "P"))
sum(g__TP$AP)
(g__FP <- filter(MB, SI == "F" & OI == "P"))
sum(g__FP$AP)
(g_S_J <- filter(MB, SD == "S" & OI == "J"))
sum(g_S_J$AP)

(g___P <- filter(MB, OI == "P"))
sum(g___P$AP)
(g___J <- filter(MB, OI == "J"))
sum(g___J$AP)

gDist <- daisy(MB[,c("OD","SD","SI","OI","APN")], metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

#### Collect Cluster Data ####
# only for Masks associated with the above clustering
# see diagram in notes

clusterMasks <- c("___X", "__XP", "X_FP", "IXFP", "EXFP", "_XTP", "XNTP", "XSTP", "_X_J", "_NXJ", "XNFJ", "XNTJ", "XS_J", "ISXJ", "ESXJ")
MBS <- getScenarios(MB, clusterMasks)

# N_ is a count of the undefined parameters, with '_' symbol in the mask
MBSg <- select(MBS, GroupAP,Ratio,Diff,N_, DP, TP, RowMean)

gDist <- daisy(MBSg, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)

MBSg <- MBSg[order(MBSg$Diff),]
sum(MBSg$GroupAP)
MBSg.split <- split(MBSg, MBSg$N_)

MBSg0 <- select(MBSg.split[[1]], -N_)
MBSg1 <- select(MBSg.split[[2]], -N_)
MBSg2 <- select(MBSg.split[[3]], -N_)
MBSg3 <- select(MBSg.split[[4]], -N_)

nrow(MBSg0)
sum(MBSg0$GroupAP)
MBSg0
gDist <- daisy(MBSg0, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

nrow(MBSg1)
sum(MBSg1$GroupAP)
MBSg1

gDist <- daisy(MBSg1, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

nrow(MBSg2)
sum(MBSg2$GroupAP)
MBSg2

nrow(MBSg3)
sum(MBSg3$GroupAP)
MBSg3

