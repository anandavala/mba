# Analysis of adaptation scenarios related to the four groups

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/mba/") # edit to suit your environment
source("./src/mb-utils.r")

MB <- loadMB()

clusterMasks <- c("___X", "__XP", "X_FP", "IXFP", "EXFP", "_XTP", "XNTP", "XSTP", "_X_J", "_NXJ", "XNFJ", "XNTJ", "XS_J", "ISXJ", "ESXJ")
MBS4g <- getScenarios(MB, clusterMasks)


MBS4g$Grp <- rep(NA, nrow(MBS4g))
MBS4g[c("_NXJ", "XNFJ", "XNTJ"), ]$Grp <- 1
MBS4g[c("_XTP", "XNTP", "XSTP"), ]$Grp <- 2
MBS4g[c("X_FP", "IXFP", "EXFP"), ]$Grp <- 3
MBS4g[c("XS_J", "ISXJ", "ESXJ"), ]$Grp <- 4
MBS4g[c("__XP", "_X_J"), ]$Grp <- 5
MBS4g["___X", ]$Grp <- 6
MBS4g$Grp <- as.factor(MBS4g$Grp)




MBS <- getScenarios(MB)

sortedPlot(MBS, "GroupAP")
sortedPlot(MBS, "Ratio")
sortedPlot(MBS, "Diff")
sortedPlot(MBS, "DP")
sortedPlot(MBS, "TP")

allPaths <- getAllPaths(MBS)

MBS4g.norm <- normColumns(MBS4g, 7:14)

# Plot of the sorted spectrum of normalised ratio values for all adaptation scenarios related to the four groups. Ratio = yin% / yang%
sortedPlot(MBS4g.norm, "GroupAP")
sortedPlot(MBS4g.norm, "Ratio")
sortedPlot(MBS4g.norm, "Diff")
sortedPlot(MBS4g.norm, "DP")
sortedPlot(MBS4g.norm, "TP")


# Now break the MBS into groups, each containing the adaptation scenarios faced by one of the four groups.
MBS._N_J <- filter(MBS4g.norm, SD == "N" & OI == "J")
rownames(MBS._N_J) <- MBS._N_J$Mask
sortedPlot(MBS._N_J, "GroupAP")
sortedPlot(MBS._N_J, "Ratio")
sortedPlot(MBS._N_J, "Diff")
sortedPlot(MBS._N_J, "DP")
sortedPlot(MBS._N_J, "TP")


MBS.__TP <- filter(MBS4g.norm, SI == "T" & OI == "P")
rownames(MBS.__TP) <- MBS.__TP$Mask
sortedPlot(MBS.__TP, "GroupAP")
sortedPlot(MBS.__TP, "Ratio")
sortedPlot(MBS.__TP, "Diff")
sortedPlot(MBS.__TP, "DP")
sortedPlot(MBS.__TP, "TP")


MBS.__FP <- filter(MBS4g.norm, SI == "F" & OI == "P")
rownames(MBS.__FP) <- MBS.__FP$Mask
sortedPlot(MBS.__FP, "GroupAP")
sortedPlot(MBS.__FP, "Ratio")
sortedPlot(MBS.__FP, "Diff")
sortedPlot(MBS.__FP, "DP")
sortedPlot(MBS.__FP, "TP")


MBS._S_J <- filter(MBS4g.norm, SD == "S" & OI == "J")
rownames(MBS._S_J) <- MBS._S_J$Mask
sortedPlot(MBS._S_J, "GroupAP")
sortedPlot(MBS._S_J, "Ratio")
sortedPlot(MBS._S_J, "Diff")
sortedPlot(MBS._S_J, "DP")
sortedPlot(MBS._S_J, "TP")
