# script for repreducing all of the data tables the diagram shown in MBA01: What can data science reveal about the structure of society?
# Article url: https://medium.com/@Anandavala/what-can-data-science-reveal-about-the-structure-of-society-b26ff16f9ca1

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/16PersonalityTypes/all/analysis/") # edit to suit your environment
source("./src/mb-utils.r")

#### Cluster Analysis ####

MB <- loadMB()

# Data table 1
(MB.sorted <- MB[order(MB$AP), ])

# Cluster Diagram
gDist <- daisy(MB[,c("OD","SD","SI","OI","APN")], metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)
# extra information in the diagram was added manually in a graphics editor

# Data table 2
MBS <- getScenarios(MB)
getPath(MBS, c("E","F","J","N"))

# Data table 3
head(MBS[order(-abs(MBS$Diff)), ])

# Data table 4
head(MBS[order(abs(MBS$Diff)), ])

# Data table 5
head(MBS[order(-abs(MBS$TP)), ])

# Data table 6
head(MBS[order(abs(MBS$TP)), ])

# Data table 7
allPaths <- getAllPaths(MBS)
head(allPaths[order(allPaths$TChDiff), ], n = 16)

# Data table 8
head(allPaths[order(allPaths$TChTP), ], n = 16)

# Data table 9
head(allPaths[order(-allPaths$TChDiff), ], n = 16)

# Data table 10
head(allPaths[order(-allPaths$TChTP), ], n = 16)

# Data table 11
head(allPaths[order(abs(allPaths$TChDiff)), ], n = 16)

# Data table 12
head(allPaths[order(abs(allPaths$TChTP)), ], n = 16)

# Data table 13
(path <-getPath(MBS, c("N","P","E","F","T","I","S","P","F","E","N","J"), chosen = c("I","S","T","J")))

# Data table 14
(path <-getPath(MBS, c("E","S","T","I","F","N","T","P","S","E","S","J"), chosen = c("I","N","F","J")))

# Data table 15
(path <-getPath(MBS, c("I","N","F","J","I","N","F","J","I","N","F","J"), chosen = c("I","N","F","J")))

# Data table 16
(path <-getPath(MBS, c("I","S","F","J","I","S","F","J","I","S","F","J"), chosen = c("I","S","F","J")))

# Data table 17
getPath(MBS, c("I","N","T","J","E","N","T","J","E","N","F","J"), chosen = c("I","N","F","J"))

# Data table 18
getPath(MBS, c("I","N","T","P","I","S","T","P","E","S","T","P"), chosen = c("E","N","T","P"))

# Data table 19
getPath(MBS, c("I","N","F","P","I","S","F","P","E","S","F","P"), chosen = c("E","N","F","P"))

# Data table 20
getPath(MBS, c("I","S","T","J","E","S","T","J","E","S","F","J"), chosen = c("I","S","F","J"))

