# Using total, male, female and gender difference data.
# Script following the format of the data tables and diagram shown in MBA01: What can data science reveal about the structure of society?
# Article url: https://medium.com/@Anandavala/what-can-data-science-reveal-about-the-structure-of-society-b26ff16f9ca1

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/mba/") # edit to suit your environment
source("./src/mb-utils.r")

#### Analysis ####

MB <- loadMBGender()

# MB types sorted by prevalence
# rownames(MB[order(MB$AP), ])
# rownames(MB[order(MB$APF), ])
# rownames(MB[order(MB$APM), ])
# rownames(MB[order(MB$APGD), ])

# Plot of the sorted spectrum of normalised prevalence values for all Myers-Briggs types %ofPop.
sortedGenderPlot(MB, "AP", withlines = TRUE, datatype = "Myers-Briggs Types", ylabel = "% of pop.", ptsize = 3)
sortedGenderPlot(MB, "APF", withlines = TRUE, datatype = "Myers-Briggs Types", ylabel = "% of pop.", ptsize = 3)
sortedGenderPlot(MB, "APM", withlines = TRUE, datatype = "Myers-Briggs Types", ylabel = "% of pop.", ptsize = 3)
sortedGenderPlot(MB, "APGD", withlines = TRUE, datatype = "Myers-Briggs Types", ylabel = "% of pop.", ptsize = 3)

# Cluster Diagram
gDist <- daisy(MB[,c("OD","SD","SI","OI","AP")], metric = "gower")
ggdendrogram(hclust(gDist))
gDist <- daisy(MB[,c("OD","SD","SI","OI","APF")], metric = "gower")
ggdendrogram(hclust(gDist))
gDist <- daisy(MB[,c("OD","SD","SI","OI","APM")], metric = "gower")
ggdendrogram(hclust(gDist))
gDist <- daisy(MB[,c("OD","SD","SI","OI","APGD")], metric = "gower")
ggdendrogram(hclust(gDist))
# extra information in the diagram was added manually in a graphics editor

# evolutionary path “E”,”F”,”J”,”N”
MBS <- getScenarios(MB)
MBSF <- getScenarios(MB, cname = "APF")
MBSM <- getScenarios(MB, cname = "APM")
MBSGD <- MBS
MBSGD[7:13] <- MBSF[7:13] - MBSM[7:13]

plotPath(MBS, MBSF, MBSM, MBSGD, c("E","F","J","N"), cname = "ChDiff")
getPath(MBS, c("E","F","J","N"))
getPath(MBSF, c("E","F","J","N"))
getPath(MBSM, c("E","F","J","N"))
getPath(MBSGD, c("E","F","J","N"))

# Plot of the sorted spectrum of normalised group prevalence values for all adaptation scenarios. GroupAP = %ofPop
MBSGender <- getGendered(MBS, MBSF, MBSM, MBSGD, "GroupAP")
sortedGenderPlot(MBSGender, "AP", datatype = "Adaptation Scenarios", ylabel = "% of pop.")
# sortedGenderPlot(MBSGender, "APF", datatype = "Adaptation Scenarios", ylabel = "% of pop.")
# sortedGenderPlot(MBSGender, "APM", datatype = "Adaptation Scenarios", ylabel = "% of pop.")
sortedGenderPlot(MBSGender, "APGD", datatype = "Adaptation Scenarios", ylabel = "% of pop.")

# Plot of the sorted spectrum of normalised choice difference values for all adaptation scenarios. Diff = yin% - yang%
MBSGender <- getGendered(MBS, MBSF, MBSM, MBSGD, "Diff")
sortedGenderPlot(MBSGender, "AP", datatype = "Adaptation Scenarios", ylabel = "Difference")
# sortedGenderPlot(MBSGender, "APF", datatype = "Adaptation Scenarios", ylabel = "Difference")
# sortedGenderPlot(MBSGender, "APM", datatype = "Adaptation Scenarios", ylabel = "Difference")
sortedGenderPlot(MBSGender, "APGD", datatype = "Adaptation Scenarios", ylabel = "Difference")

# Plot of the sorted spectrum of normalised demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
MBSGender <- getGendered(MBS, MBSF, MBSM, MBSGD, "DP")
sortedGenderPlot(MBSGender, "AP", datatype = "Adaptation Scenarios", ylabel = "Demographic Pressure")
# sortedGenderPlot(MBSGender, "APF", datatype = "Adaptation Scenarios", ylabel = "Demographic Pressure")
# sortedGenderPlot(MBSGender, "APM", datatype = "Adaptation Scenarios", ylabel = "Demographic Pressure")
sortedGenderPlot(MBSGender, "APGD", datatype = "Adaptation Scenarios", ylabel = "Demographic Pressure")

# Plot of the sorted spectrum of normalised TP values for all adaptation scenarios. TP = Diff / GroupAP
MBSGender <- getGendered(MBS, MBSF, MBSM, MBSGD, "TP")
sortedGenderPlot(MBSGender, "AP", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure")
# sortedGenderPlot(MBSGender, "APF", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure")
# sortedGenderPlot(MBSGender, "APM", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure")
sortedGenderPlot(MBSGender, "APGD", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure")

# What are the most discouraged and encouraged adaptations, with the highest choice difference magnitude?
# What are the adaptations with the least choice difference?
sortedPlot(rbind(head(MBS[order(MBS$Diff), ]), head(MBS[order(abs(MBS$Diff)), ]), tail(MBS[order(MBS$Diff), ])), "Diff", datatype = "Adaptation Scenarios", ylabel = "% of pop.", ptsize = 3, suffix = "Total Population")
sortedPlot(rbind(head(MBSF[order(MBSF$Diff), ]), head(MBSF[order(abs(MBSF$Diff)), ]), tail(MBSF[order(MBSF$Diff), ])), "Diff", datatype = "Adaptation Scenarios", ylabel = "% of pop.", ptsize = 3, suffix = "Females")
sortedPlot(rbind(head(MBSM[order(MBSM$Diff), ]), head(MBSM[order(abs(MBSM$Diff)), ]), tail(MBSM[order(MBSM$Diff), ])), "Diff", datatype = "Adaptation Scenarios", ylabel = "% of pop.", ptsize = 3, suffix = "Males")
sortedPlot(rbind(head(MBSGD[order(MBSGD$Diff), ]), head(MBSGD[order(abs(MBSGD$Diff)), ]), tail(MBSGD[order(MBSGD$Diff), ])), "Diff", datatype = "Adaptation Scenarios", ylabel = "% of pop.", ptsize = 3, suffix = "Gender Differences")

# What are the adaptations with the most targeted pressure?
# What are the adaptations with the least targeted pressure?
sortedPlot(rbind(head(MBS[order(MBS$TP), ]), head(MBS[order(abs(MBS$TP)), ]), tail(MBS[order(MBS$TP), ])), "TP", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Total Population")
sortedPlot(rbind(head(MBSF[order(MBSF$TP), ]), head(MBSF[order(abs(MBSF$TP)), ]), tail(MBSF[order(MBSF$TP), ])), "TP", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Females")
sortedPlot(rbind(head(MBSM[order(MBSM$TP), ]), head(MBSM[order(abs(MBSM$TP)), ]), tail(MBSM[order(MBSM$TP), ])), "TP", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Males")
sortedPlot(rbind(head(MBSGD[order(MBSGD$TP), ]), head(MBSGD[order(abs(MBSGD$TP)), ]), tail(MBSGD[order(MBSGD$TP), ])), "TP", datatype = "Adaptation Scenarios", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Gender Differences")

# Plot of the sorted spectrum of normalise total group prevalence values for all 4 step evolutionary paths.
allPaths <- getAllPaths(MBS)
allPathsF <- getAllPaths(MBSF)
allPathsM <- getAllPaths(MBSM)
allPathsGD <- allPaths
allPathsGD[6:9] <- allPathsF[6:9] - allPathsM[6:9]

AllPathsGender <- getGendered(allPaths, allPathsF, allPathsM, allPathsGD, "TGroupAP")
sortedGenderPlot(AllPathsGender, "AP", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "% of pop.")
# sortedGenderPlot(AllPathsGender, "APF", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "% of pop.")
# sortedGenderPlot(AllPathsGender, "APM", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "% of pop.")
sortedGenderPlot(AllPathsGender, "APGD", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "% of pop.")

# Plot of the sorted spectrum of normalised total choice difference values for all 4 step evolutionary paths.
AllPathsGender <- getGendered(allPaths, allPathsF, allPathsM, allPathsGD, "TChDiff")
sortedGenderPlot(AllPathsGender, "AP", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Difference")
# sortedGenderPlot(AllPathsGender, "APF", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Difference")
# sortedGenderPlot(AllPathsGender, "APM", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Difference")
sortedGenderPlot(AllPathsGender, "APGD", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Difference")

# Plot of the sorted spectrum of normalised demographic pressure values for all 4 step evolutionary paths.
AllPathsGender <- getGendered(allPaths, allPathsF, allPathsM, allPathsGD, "TChDP")
sortedGenderPlot(AllPathsGender, "AP", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Demographic Pressure")
# sortedGenderPlot(AllPathsGender, "APF", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Demographic Pressure")
# sortedGenderPlot(AllPathsGender, "APM", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Demographic Pressure")
sortedGenderPlot(AllPathsGender, "APGD", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Demographic Pressure")

# Plot of the sorted spectrum of normalised total targeted pressure values for all 4 step evolutionary paths.
AllPathsGender <- getGendered(allPaths, allPathsF, allPathsM, allPathsGD, "TChTP")
sortedGenderPlot(AllPathsGender, "AP", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Targeted Pressure")
# sortedGenderPlot(AllPathsGender, "APF", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Targeted Pressure")
# sortedGenderPlot(AllPathsGender, "APM", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Targeted Pressure")
sortedGenderPlot(AllPathsGender, "APGD", lblsize = 4, datatype = "Evolutionary Paths", ylabel = "Targeted Pressure")

# The most discouraged paths (in terms of choice difference)
# The most encouraged paths (in terms of choice difference)
# The least pressure paths (in terms of choice difference)
sortedPlot(rbind(head(allPaths[order(allPaths$TChDiff), ]), head(allPaths[order(abs(allPaths$TChDiff)), ]), tail(allPaths[order(allPaths$TChDiff), ])), "TChDiff", datatype = "Evolutionary Paths", ylabel = "% of pop.", ptsize = 3, suffix = "Total Population")
sortedPlot(rbind(head(allPathsF[order(allPathsF$TChDiff), ]), head(allPathsF[order(abs(allPathsF$TChDiff)), ]), tail(allPathsF[order(allPathsF$TChDiff), ])), "TChDiff", datatype = "Evolutionary Paths", ylabel = "% of pop.", ptsize = 3, suffix = "Females")
sortedPlot(rbind(head(allPathsM[order(allPathsM$TChDiff), ]), head(allPathsM[order(abs(allPathsM$TChDiff)), ]), tail(allPathsM[order(allPathsM$TChDiff), ])), "TChDiff", datatype = "Evolutionary Paths", ylabel = "% of pop.", ptsize = 3, suffix = "Males")
sortedPlot(rbind(head(allPathsGD[order(allPathsGD$TChDiff), ]), head(allPathsGD[order(abs(allPathsGD$TChDiff)), ]), tail(allPathsGD[order(allPathsGD$TChDiff), ])), "TChDiff", datatype = "Evolutionary Paths", ylabel = "% of pop.", ptsize = 3, suffix = "Gender Differences")

# # The most discouraged paths (in terms of demographic pressure)
# # The most encouraged paths (in terms of demographic pressure)
# # The least pressure paths (in terms of demographic pressure)
# sortedPlot(rbind(head(allPaths[order(allPaths$TChDP), ]), head(allPaths[order(abs(allPaths$TChDP)), ]), tail(allPaths[order(allPaths$TChDP), ])), "TChDP", datatype = "Evolutionary Paths", ylabel = "Demographic Pressure", ptsize = 3, suffix = "Total Population")
# sortedPlot(rbind(head(allPathsF[order(allPathsF$TChDP), ]), head(allPathsF[order(abs(allPathsF$TChDP)), ]), tail(allPathsF[order(allPathsF$TChDP), ])), "TChDP", datatype = "Evolutionary Paths", ylabel = "Demographic Pressure", ptsize = 3, suffix = "Females")
# sortedPlot(rbind(head(allPathsM[order(allPathsM$TChDP), ]), head(allPathsM[order(abs(allPathsM$TChDP)), ]), tail(allPathsM[order(allPathsM$TChDP), ])), "TChDP", datatype = "Evolutionary Paths", ylabel = "Demographic Pressure", ptsize = 3, suffix = "Males")
# sortedPlot(rbind(head(allPathsGD[order(allPathsGD$TChDP), ]), head(allPathsGD[order(abs(allPathsGD$TChDP)), ]), tail(allPathsGD[order(allPathsGD$TChDP), ])), datatype = "Evolutionary Paths", ylabel = "Demographic Pressure", "TChDP", ptsize = 3, suffix = "Gender Differences")

# The most discouraged paths (in terms of targeted pressure)
# The most encouraged paths (in terms of targeted pressure)
# The least pressure paths (in terms of targeted pressure)
sortedPlot(rbind(head(allPaths[order(allPaths$TChTP), ]), head(allPaths[order(abs(allPaths$TChTP)), ]), tail(allPaths[order(allPaths$TChTP), ])), "TChTP", datatype = "Evolutionary Paths", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Total Population")
sortedPlot(rbind(head(allPathsF[order(allPathsF$TChTP), ]), head(allPathsF[order(abs(allPathsF$TChTP)), ]), tail(allPathsF[order(allPathsF$TChTP), ])), "TChTP", datatype = "Evolutionary Paths", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Females")
sortedPlot(rbind(head(allPathsM[order(allPathsM$TChTP), ]), head(allPathsM[order(abs(allPathsM$TChTP)), ]), tail(allPathsM[order(allPathsM$TChTP), ])), "TChTP", datatype = "Evolutionary Paths", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Males")
sortedPlot(rbind(head(allPathsGD[order(allPathsGD$TChTP), ]), head(allPathsGD[order(abs(allPathsGD$TChTP)), ]), tail(allPathsGD[order(allPathsGD$TChTP), ])), "TChTP", datatype = "Evolutionary Paths", ylabel = "Targeted Pressure", ptsize = 3, suffix = "Gender Differences")


# Discouraged: Logistician ISTJ becomes a Protagonist ENFJ via 12 adaptation steps
plotPath(MBS, MBSF, MBSM, MBSGD, c("N","P","E","F","T","I","S","P","F","E","N","J"), chosen = c("I","S","T","J"), "ChDiff")

# Encouraged: Advocate INFJ becomes an Executive ESTJ via 12 adaptation steps
plotPath(MBS, MBSF, MBSM, MBSGD, c("E","S","T","I","F","N","T","P","S","E","S","J"), chosen = c("I","N","F","J"), "ChDiff")

# The most discouraged: Advocate INFJ remains an Advocate throughout 12 adaptation steps
# for total and males
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","N","F","J","I","N","F","J","I","N","F","J"), chosen = c("I","N","F","J"), "ChDiff")
# for females
plotPath(MBS, MBSF, MBSM, MBSGD, c("E","N","T","J","E","N","T","J","E","N","T","J"), chosen = c("E","N","T","J"), "ChDiff")

# The most encouraged: Defender ISFJ remains a Defender throughout 12 adaptation steps
# for total and females
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","S","F","J","I","S","F","J","I","S","F","J"), chosen = c("I","S","F","J"), "ChDiff")
# for males
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","S","T","J","I","S","T","J","I","S","T","J"), chosen = c("I","S","T","J"), "ChDiff")


# Cycling within the _N_J group
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","N","T","J","E","N","T","J","E","N","F","J","I","N","F","J"), chosen = c("I","N","F","J"), "ChDiff")

# Cycling within the __TP group
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","N","T","P","I","S","T","P","E","S","T","P","E","N","T","P"), chosen = c("E","N","T","P"), "ChDiff")

# Cycling within the __FP group
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","N","F","P","I","S","F","P","E","S","F","P","E","N","F","P"), chosen = c("E","N","F","P"), "ChDiff")

# Cycling within the _S_J group
plotPath(MBS, MBSF, MBSM, MBSGD, c("I","S","T","J","E","S","T","J","E","S","F","J","I","S","F","J"), chosen = c("I","S","F","J"), "ChDiff")

