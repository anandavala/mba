

# Analysis of MB types into four groups based on parameters and percentage representation

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/analysis/") # edit to suit your environment
source("./src/mb-utils.r")

#### Cluster Analysis ####

MB <- loadMB()

MBS <- getScenarios(MB)

path <- getPath(MBS, c("E","F","J","N"))
path

allPaths <- getAllPaths(MBS)

allPaths.TGroupAP <- allPaths[order(allPaths$TGroupAP),]
head(allPaths.TGroupAP)
tail(allPaths.TGroupAP)

allPaths.TChDiff <- allPaths[order(allPaths$TChDiff),]
head(allPaths.TChDiff)
tail(allPaths.TChDiff)

allPaths.TChDP <- allPaths[order(allPaths$TChDP),]
head(allPaths.TChDP)
tail(allPaths.TChDP)

allPaths.TChTP <- allPaths[order(allPaths$TChTP),]
head(allPaths.TChTP)
tail(allPaths.TChTP)

allPaths.RowMean <- allPaths[order(allPaths$RowMean),]
head(allPaths.RowMean)
tail(allPaths.RowMean)

# we now have all possible adaptation paths (of four steps)
# we can examine particular paths of interest using getPath(), e.g.

allPaths["NJIT",]
getPath(MBS, allPaths["NJIT",])

# cluster analysis of all paths

gDist <- daisy(allPaths, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)
plotClusters(gDist,allPaths, 2)
plotClusters(gDist,allPaths, 6)
plotClusters(gDist,allPaths, 11)

nrow(allPaths)

allPaths.lrgTGroupAP <- allPaths[allPaths$TGroupAP >= mean(allPaths$TGroupAP), ]
allPaths.smlTGroupAP <- allPaths[allPaths$TGroupAP < mean(allPaths$TGroupAP), ]

allPaths.posTChDiff <- allPaths[allPaths$TChDiff >= 0, ]
allPaths.negTChDiff <- allPaths[allPaths$TChDiff < 0, ]

allPaths.posTChDP <- allPaths[allPaths$TChDP >= 0, ]
allPaths.negTChDP <- allPaths[allPaths$TChDP < 0, ]

allPaths.posTChTP <- allPaths[allPaths$TChTP >= 0, ]
allPaths.negTChTP <- allPaths[allPaths$TChTP < 0, ]

# cluster analysis of all paths with RowMean

# execute one from below
paths <- allPaths
paths <- allPaths.smlTGroupAP
paths <- allPaths.lrgTGroupAP
paths <- allPaths.negTChDiff
paths <- allPaths.posTChDiff
paths <- allPaths.negTChDP
paths <- allPaths.posTChDP
paths <- allPaths.negTChTP
paths <- allPaths.posTChTP

nrow(paths)

gDist <- daisy(paths, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
heatmap(as.matrix(gDist), Rowv=as.dendrogram(hc), Colv="Rowv", symm = TRUE)

plotSilWidth(gDist)
plotClusters(gDist,paths, 2)
plotClusters(gDist,paths, 6)


