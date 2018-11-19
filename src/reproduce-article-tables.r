# script for repreducing all of the data tables the diagram shown in MBA01: What can data science reveal about the structure of society?
# Article url: https://medium.com/@Anandavala/what-can-data-science-reveal-about-the-structure-of-society-b26ff16f9ca1

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/mba/") # edit to suit your environment
source("./src/mb-utils.r")

#### Analysis ####

MB <- loadMBForArticle() # loads only the data sources used at the time of the article

# MB types sorted by prevalence
MB[order(MB$AP), ]

# Plot of the sorted spectrum of prevalence values for all Myers-Briggs types. AP = %ofPop.
sortedPlot(MB, "APN", ptsize = 3, datatype = "Myers-Briggs Types")

# Cluster Diagram
gDist <- daisy(MB[,c("OD","SD","SI","OI","APN")], metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
# extra information in the diagram was added manually in a graphics editor

# evolutionary path “E”,”F”,”J”,”N”
MBS <- getScenarios(MB)
getPath(MBS, c("E","F","J","N"))

# Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
sortedPlot(MBS, "GroupAP", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
sortedPlot(MBS, "Diff", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
sortedPlot(MBS, "DP", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
sortedPlot(MBS, "TP", datatype = "Adaptation Scenarios")

# What are the most discouraged and encouraged adaptations, with the highest absolute choice difference?
head(MBS[order(-abs(MBS$Diff)), ])

# What are the adaptations with the least choice difference?
head(MBS[order(abs(MBS$Diff)), ])

# What are the adaptations with the most targeted pressure?
head(MBS[order(-abs(MBS$TP)), ])

# What are the adaptations with the least targeted pressure?
head(MBS[order(abs(MBS$TP)), ])

# Plot of the sorted spectrum of normalise total group prevalence values for all 4 step evolutionary paths.
allPaths <- getAllPaths(MBS)
sortedPlot(allPaths, "TGroupAP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total choice difference values for all 4 step evolutionary paths.
sortedPlot(allPaths, "TChDiff", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of demographic pressure values for all 4 step evolutionary paths.
sortedPlot(allPaths, "TChDP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total targeted pressure values for all 4 step evolutionary paths.
sortedPlot(allPaths, "TChTP", lblsize = 4, datatype = "Evolutionary Paths")

# The most discouraged paths (in terms of choice difference)
head(allPaths[order(allPaths$TChDiff), ], n = 16)

# The most discouraged paths (in terms of targeted pressure)
head(allPaths[order(allPaths$TChTP), ], n = 16)

# The most encouraged paths (in terms of choice difference)
head(allPaths[order(-allPaths$TChDiff), ], n = 16)

# The most encouraged paths (in terms of targeted pressure)
head(allPaths[order(-allPaths$TChTP), ], n = 16)

# The least pressure paths (in terms of choice difference)
head(allPaths[order(abs(allPaths$TChDiff)), ], n = 16)

# The least pressure paths (in terms of targeted pressure)
head(allPaths[order(abs(allPaths$TChTP)), ], n = 16)

# Discouraged: Logistician ISTJ becomes a Protagonist ENFJ via 12 adaptation steps
getPath(MBS, c("N","P","E","F","T","I","S","P","F","E","N","J"), chosen = c("I","S","T","J"))

# Encouraged: Advocate INFJ becomes an Executive ESTJ via 12 adaptation steps
getPath(MBS, c("E","S","T","I","F","N","T","P","S","E","S","J"), chosen = c("I","N","F","J"))

# The most discouraged: Advocate INFJ remains an Advocate throughout 12 adaptation steps
getPath(MBS, c("I","N","F","J","I","N","F","J","I","N","F","J"), chosen = c("I","N","F","J"))

# The most encouraged: Defender ISFJ remains a Defender throughout 12 adaptation steps
getPath(MBS, c("I","S","F","J","I","S","F","J","I","S","F","J"), chosen = c("I","S","F","J"))

# Cycling within the _N_J group
getPath(MBS, c("I","N","T","J","E","N","T","J","E","N","F","J"), chosen = c("I","N","F","J"))

# Cycling within the __TP group
getPath(MBS, c("I","N","T","P","I","S","T","P","E","S","T","P"), chosen = c("E","N","T","P"))

# Cycling within the __FP group
getPath(MBS, c("I","N","F","P","I","S","F","P","E","S","F","P"), chosen = c("E","N","F","P"))

# Cycling within the _S_J group
getPath(MBS, c("I","S","T","J","E","S","T","J","E","S","F","J"), chosen = c("I","S","F","J"))

