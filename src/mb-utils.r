#### Utils ####
library(dplyr)
library(Rtsne)
library(cluster)
library(ggplot2)
library(ggdendro)
library(TSclust)

# get RowMeans for a data frame
getRowMeans <- function(mb, rnames = rownames(mb)) {
  dat <- as.matrix(mb)
  rownames(dat) <- rnames
  # edit the following line to try different methods
  method <- "COR" # "COR" "FRECHET" "DTWARP" "INT.PER" "DTW" "DWT" "AR.PIC"
  # note: FRECHET is very computationally intensive (it can crash RStudio even with moderate data size)
  
  # across observations (types)
  rDist <- diss(dat, method)
  rmeans <- as.data.frame(rowMeans(as.matrix(rDist)))
  colnames(rmeans) <- "RowMean"
  return(rmeans)
}


# PR from https://www.careerplanner.com/MB2/TypeInPopulation.cfm
# LP, UP from https://www.myersbriggs.org/my-mbti-personality-type/my-mbti-results/how-frequent-is-my-type.htm?bhjs=0
# SBT, SBM, SBF from https://www.statisticbrain.com/myers-briggs-statistics/
# PMT, PMM, PMF from https://personalitymax.com/personality-types/population-gender/ (Note that these are estimates and are not necessarily correct.)

#### load MB data ###
loadMB <- function() {
  out <- read.csv(file = "./data/myers-briggs-dataset-01.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  out$OD <- as.factor(out$OD)
  out$SD <- as.factor(out$SD)
  out$SI <- factor(out$SI, labels = c("F", "T"))
  out$OI <- as.factor(out$OI)
  # compute average percentages
  out$LP <- as.double(out$LP)
  out$UP <- as.double(out$UP)
  out$PR <- as.double(out$PR)
  out$Pair <- as.integer(out$Pair)
  out$SBT <- as.double(out$SBT - 0.26 / 16)
  out$SBM <- as.double(out$SBM)
  out$SBF <- as.double(out$SBF - 2.6 / 16)
  out$PMT <- as.double(out$PMT) # treat with caution - only claims to be an estimate
  out$PMM <- as.double(out$PMM) # treat with caution - only claims to be an estimate
  out$PMF <- as.double(out$PMF) # treat with caution - only claims to be an estimate
  # Assemble results
  out <- out %>%
    mutate(LUP = (LP + UP) / 2 + 1/16) %>%
    mutate(PR = PR - 0.3 / 16) %>%
    mutate(AP = (LUP + PR + SBT) / 3) %>% # don't use PMT, its only an estimate
    mutate(APN = (AP - min(AP)) / (max(AP) - min(AP))) %>%
    mutate(Num = rownames(out)) %>%
    mutate(APM = (SBM + PMM) / 2) %>%
    mutate(APF = (SBF + PMF) / 2) %>%
    select(Num, 1:7, AP, APN, SBM, PMM, APM, SBF, PMF, APF)
  # # compute RowMean for all rows
  # if (paramsAsFactors) {
  #   out.numeric <- select(loadMB(paramsAsFactors = FALSE), OD, SD, SI, OI, APN)
  # }
  # else {
  #   out.numeric <- select(out, OD, SD, SI, OI, APN)
  # }
  # rowMeans <- getRowMeans(out.numeric, out$Type)
  # out <- mutate(out, RowMean = rowMeans$RowMean)
  rownames(out) <- out$Type
  # Add group id column
  out$Grp <- rep(NA, nrow(out))
  out[out$SD == "N" & out$OI == "J", ]$Grp <- 1
  out[out$SI == "T" & out$OI == "P", ]$Grp <- 2
  out[out$SI == "F" & out$OI == "P", ]$Grp <- 3
  out[out$SD == "S" & out$OI == "J", ]$Grp <- 4
  out$Grp <- as.factor(out$Grp)
  return(out)
}


roundFun <- function(x) sprintf("%.3f", x)

# adapted from (source unknown)
# Plot silhouette width for many k using PAM
plotSilWidth <- function(distMatrix) {
  silWidth <- c(NA)
  for(i in 2:15){
    pamFit <- pam(distMatrix,
                  diss = TRUE,
                  k = i)
    silWidth[i] <- pamFit$silinfo$avg.width
  }
  ggplot(mapping = aes(x = 1:15, y = silWidth)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(2,15)) +
    scale_y_continuous(labels = roundFun)
}

# adapted from (source unknown)
plotClusters <- function(distMatrix, Data, K) {
  pamFit <- pam(distMatrix, diss = TRUE, k = K)
  pam_results <- Data %>%
    mutate(cluster = pamFit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  print(t(Data[pamFit$medoids, ]))
  tsneObj <- Rtsne(distMatrix, is_distance = TRUE)
  tsneData <- tsneObj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pamFit$clustering),
           name = 1:nrow(as.matrix(distMatrix)))
  ggplot(aes(x = X, y = Y), data = tsneData) +
    geom_point(aes(color = cluster))
}


#### Collect Cluster Data ####
# for all possible patterns or Masks

numX <- function(str) { # str is vector of characters
  count <- 0
  for (i in 1:length(str)) {
    if (str[i] == "X") count <- count + 1
  }
  return(count)
}

num_ <- function(strs) { # strs is a vector of strings
  res <- c()
  for (str in strs) {
    str <- strsplit(str, "")[[1]]
    count <- 0
    for (i in 1:length(str)) {
      if (str[i] == "_") count <- count + 1
    }
    res <- append(res, count)
  }
  return(res)
}


symbolSet <- data.frame(c1 = c("I", "E", "X", "_"),
                        c2 = c("N", "S", "X", "_"),
                        c3 = c("F", "T", "X", "_"),
                        c4 = c("P", "J", "X", "_") )

getMasks <- function(m1 = "", m2 = "", m3 = "", m4 = "") {
  str <- c("", "", "", "")
  Masks <- c()
  if (m1 != "") {
    r1 <- c(1)
    sym1 <- c(m1)
  }
  else {
    r1 <- 1:4
    sym1 <- symbolSet[, 1]
  }
  for (s1 in r1) {
    str[1] <- sprintf("%s", sym1[s1])
    # print(str)
    if (m2 != "") {
      r2 <- c(1)
      sym2 <- c(m2)
    }
    else {
      r2 <- 1:4
      sym2 <- symbolSet[, 2]
    }
    for (s2 in r2) {
      str[2] <- sprintf("%s", sym2[s2])
      # print(str)
      if (m3 != "") {
        r3 <- c(1)
        sym3 <- c(m3)
      }
      else {
        r3 <- 1:4
        sym3 <- symbolSet[, 3]
      }
      for (s3 in r3) {
        str[3] <- sprintf("%s", sym3[s3])
        # print(str)
        if (m4 != "") {
          r4 <- c(1)
          sym4 <- c(m4)
        }
        else {
          r4 <- 1:4
          sym4 <- symbolSet[, 4]
        }
        for (s4 in r4) {
          str[4] <- sprintf("%s", sym4[s4])
          # print(str)
          if (numX(str) == 1) Masks <- append(Masks, sprintf("%s%s%s%s", str[1], str[2], str[3], str[4]))
        }
      }
    }
  }
  return(Masks)
}


# function, given a mask compute some statistics for the associated group of types
getScenarios <- function(mb, masks = getMasks()) {
  groupAPs <- c()
  ratios <- c()
  ap0s <- c()
  ap1s <- c()
  diffs <- c()
  params <- list(c(),c(),c(),c())
  origMB <- mb
  for (mask in masks) {
    mb <- origMB
    pp <- 1:4 # possible positions
    xp <- 0 # X position
    vec <- strsplit(mask, split = "")[[1]]
    for (p in 1:4) {
      params[[p]] <- append(params[[p]], vec[p])
      if (vec[p] == "X") {
        pp <- pp[pp!=p]
        xp <- p
      }
      else if (vec[p] == "_") {
        pp <- pp[pp!=p]
      }
    }
    if (length(pp) > 0) {
      for (i in 1:length(pp)) {
        tmp <- split(mb, mb[, 3 + pp[i]])
        mb <- tmp[[ifelse(vec[pp[i]] == symbolSet[2, pp[i]], 1, 2)]]
      }
    }
    tmp <- split(mb, mb[, 3 + xp])
    mb0 <- tmp[[1]]
    mb1 <- tmp[[2]]
    p0 <- sum(mb0$AP)
    p1 <- sum(mb1$AP)
    ratio <-  p0 / p1
    ap0 <- p0 / (p0 + p1) * 100
    ap1 <- 100 - ap0
    diff <- p0 - p1
    groupAPs <- append(groupAPs, p0 + p1)
    ratios <- append(ratios, ratio)
    ap0s <- append(ap0s, ap0)
    ap1s <- append(ap1s, ap1)
    diffs <- append(diffs, diff)
  }
  df <- data.frame(row.names = masks, 
                   Mask = masks,
                   OD = params[[1]],
                   SD = params[[2]],
                   SI = params[[3]],
                   OI = params[[4]],
                   N_ = num_(masks),
                   GroupAP = groupAPs,
                   Ratio = ratios,
                   AP0 = ap0s,
                   AP1 = ap1s,
                   Diff = diffs,
                   DP = groupAPs * diffs,
                   TP = diffs / groupAPs )
  df$Mask <- as.character(df$Mask)
  df$N_ <- as.integer(df$N_)
  # # compute RowMean for all rows
  # df.numeric <- select(df, N_, GroupAP, Ratio, AP0, AP1, Diff, DP, TP)
  # df$RowMean <- getRowMeans(df.numeric)$RowMean
  # add group ids 
  df$Grp <- rep(NA, nrow(df))
  df[is.na(df$Grp) & df$SD == "N" & df$OI == "J", ]$Grp <- 1
  df[is.na(df$Grp) & df$SI == "T" & df$OI == "P", ]$Grp <- 2
  df[is.na(df$Grp) & df$SI == "F" & df$OI == "P", ]$Grp <- 3
  df[is.na(df$Grp) & df$SD == "S" & df$OI == "J", ]$Grp <- 4
  # df[is.na(df$Grp) & df$SI == "X" & (df$OI == "P" | df$OI == "J"), ]$Grp <- 5
  # df[is.na(df$Grp) & df$OI == "X", ]$Grp <- 6
  df[is.na(df$Grp), ]$Grp <- 5
  df$Grp <- as.factor(df$Grp)
  return(df)
}


splitByN_ <- function(mbs, num) {
  return(select(split(mbs, mbs$N_)[[num + 1]], -N_))
}

# return the sybol number (1:2) and the parameter number (1:4)
getParam <- function(s) {
  for (c in 1:4) {
    if (symbolSet[1,c] == sprintf("%s", s)) return(c(1, c))
    if (symbolSet[2,c] == sprintf("%s", s)) return(c(2, c))
  }
  return(c(0,0))
}

# each path is defined by a set of four choices, e.g. c("N","J","I","T")
# traverse this sequence of choices and print details of each step
getPath <- function(mbs, choices, undef = -1, chosen = c("","","",""), withTotals = TRUE) {
  # print("mbs")
  # print(mbs)
  # print("choices")
  # print(choices)
  # print("undef")
  # print(undef)
  if (class(choices) == "data.frame") choices <- c(sprintf("%s", choices[1,1]), 
                                                   sprintf("%s", choices[1,2]), 
                                                   sprintf("%s", choices[1,3]), 
                                                   sprintf("%s", choices[1,4]))
  if (all.equal(chosen, c("","","","")) == "TRUE") undef = 3
  if (length(choices) > 0) {
    choice = choices[1]
    if (length(choices) > 1) choices <- choices[2:length(choices)]
    else choices <- c()
    rc <- getParam(choice)
    r <- rc[1]
    c <- rc[2]
    if (undef < 0) {
      undef <- 0
      chosen[c] <- "X"
      mbs <- MBS[getMasks(chosen[1], chosen[2], chosen[3], chosen[4]),]
    }
    chosen[c] <- choice
    mbs$Choice <- choice
    mbs$ChDiff <- mbs$Diff * ifelse(r == 1, 1, -1) 
    mbs$ChDP <- mbs$DP * ifelse(r == 1, 1, -1) 
    mbs$ChTP <- mbs$TP * ifelse(r == 1, 1, -1) 
    df2 <- NULL
    if (c == 1){
      df1 <- mbs[mbs$OD == "X" & mbs$N_ == undef, ]
      if (r == 1) {
        df2 <- getPath(mbs[mbs$OD == "I", ], choices, undef - 1, chosen, FALSE)
      }
      else {
        df2 <- getPath(mbs[mbs$OD == "I", ], choices, undef - 1, chosen, FALSE)
      }
    }
    else if (c == 2){
      df1 <- mbs[mbs$SD == "X" & mbs$N_ == undef, ]
      if (r == 1) {
        df2 <- getPath(mbs[mbs$SD == "N", ], choices, undef - 1, chosen, FALSE)
      }
      else {
        df2 <- getPath(mbs[mbs$SD == "S", ], choices, undef - 1, chosen, FALSE)
      }
    }
    else if (c == 3){
      df1 <- mbs[mbs$SI == "X" & mbs$N_ == undef, ]
      if (r == 1) {
        df2 <- getPath(mbs[mbs$SI == "F", ], choices, undef - 1, chosen, FALSE)
      }
      else {
        df2 <- getPath(mbs[mbs$SI == "T", ], choices, undef - 1, chosen, FALSE)
      }
    }
    else if (c == 4){
      df1 <- mbs[mbs$OI == "X" & mbs$N_ == undef, ]
      if (r == 1) {
        df2 <- getPath(mbs[mbs$OI == "P", ], choices, undef - 1, chosen, FALSE)
      }
      else {
        df2 <- getPath(mbs[mbs$OI == "J", ], choices, undef - 1, chosen, FALSE)
      }
    }
    if (withTotals) {
      df12 <- rbind(df1, df2)
      df3 <- data.frame(row.names = "Total", 
                        Mask = "",
                        OD = "",
                        SD = "",
                        SI = "",
                        OI = "",
                        N_ = "",
                        GroupAP = sum(df12$GroupAP),
                        Ratio = sum(df12$Ratio),
                        AP0 = sum(df12$AP0),
                        AP1 = sum(df12$AP1),
                        Diff = sum(df12$Diff),
                        DP = sum(df12$DP),
                        TP = sum(df12$TP),
                        Grp = "",
                        # RowMean = sum(df12$RowMean),
                        Choice = "",
                        ChDiff = sum(df12$ChDiff),
                        ChDP = sum(df12$ChDP),
                        ChTP = sum(df12$ChTP) )
      return(rbind(df12, df3))
    }
    else {
      return(rbind(df1, df2))
    }
  }
}


getAllPaths <- function(mbs) {
  syms <- symbolSet
  outRow <- c()
  out <- data.frame(Ch1 = c(NA), Ch2 = c(NA), Ch3 = c(NA), Ch4 = c(NA))
  for (s1 in 1:4) {
    for (r1 in 1:2) {
      # print(syms[r1, s1])
      n1 <- colnames(syms)[s1]
      syms2 <- select(syms, -n1)
      for (s2 in 1:ncol(syms2)) {
        for (r2 in 1:2) {
          n2 <- colnames(syms2)[s2]
          syms3 <- select(syms2, -n2)
          for (s3 in 1:ncol(syms3)) {
            for (r3 in 1:2) {
              n3 <- colnames(syms3)[s3]
              syms4 <- select(syms3, -n3)
              for (s4 in 1:ncol(syms4)) {
                for (r4 in 1:2) {
                  outRow <- c(sprintf("%s", syms[r1, s1]))
                  outRow <- append(outRow, sprintf("%s", syms2[r2, s2]))
                  outRow <- append(outRow, sprintf("%s", syms3[r3, s3]))
                  outRow <- append(outRow, sprintf("%s", syms4[r4, s4]))
                  out <- rbind(out, outRow)
                }
              }
            }
          }
        }
      }
    }
  }
  out <- na.omit(out)
  rownames(out) <- sprintf("%s%s%s%s", out[,"Ch1"], out[,"Ch2"], out[,"Ch3"], out[,"Ch4"])
  Mask <- rownames(out)
  TGroupAP <- c()
  TChDiff <- c()
  TChDP <- c()
  TChTP <- c()
  for (i in 1:nrow(out)) {
    pathDf <- getPath(mbs, c(out[i,1], out[i,2], out[i,3], out[i,4]), withTotals = FALSE)
    TGroupAP <- append(TGroupAP, sum(pathDf$GroupAP))
    TChDiff <- append(TChDiff, sum(pathDf$ChDiff))
    TChDP <- append(TChDP, sum(pathDf$ChDP))
    TChTP <- append(TChTP, sum(pathDf$ChTP))
  }
  out$Ch1 <- factor(out$Ch1)
  out$Ch2 <- factor(out$Ch2)
  out$Ch3 <- factor(out$Ch3)
  out$Ch4 <- factor(out$Ch4)
  # # compute RowMean for all rows
  # tmp <- out
  # tmp$Ch1 <- as.numeric(tmp$Ch1)
  # tmp$Ch2 <- as.numeric(tmp$Ch2)
  # tmp$Ch3 <- as.numeric(tmp$Ch3)
  # tmp$Ch4 <- as.numeric(tmp$Ch4)
  # RowMean <- getRowMeans(tmp)
  # out <- cbind(Mask, out, TGroupAP, TChDiff, TChDP, TChTP, RowMean)
  out <- cbind(Mask, out, TGroupAP, TChDiff, TChDP, TChTP)
  out$Grp <- as.integer(rep(NA, nrow(out)))
  out[grepl("N", out$Mask, fixed=TRUE) & grepl("J", out$Mask, fixed=TRUE), ]$Grp <- 1
  out[grepl("T", out$Mask, fixed=TRUE) & grepl("P", out$Mask, fixed=TRUE), ]$Grp <- 2
  out[grepl("F", out$Mask, fixed=TRUE) & grepl("P", out$Mask, fixed=TRUE), ]$Grp <- 3
  out[grepl("S", out$Mask, fixed=TRUE) & grepl("J", out$Mask, fixed=TRUE), ]$Grp <- 4
  out$Grp <- as.factor(out$Grp)
  return(out)
}

# normalise the columns of a data frame
normColumns <- function(df, cinds) {
  for (c in cinds) {
    v <- df[, c]
    if (min(v) < 0) { # v has negative values
      av <- abs(v)
      nv <- sign(v) * (av - min(av)) / (max(av) - min(av))
    }
    else { # v has all positive values
      nv <- (v - min(v)) / (max(v) - min(v))
    }
    df[, c] <- nv
  }
  return(df)
}


# plot a sorted data frame
# first sort the values, then plot them to see how they vary over the spectrum
sortedPlot <- function(mb, cname, lsize = NULL) {
  mb <- mb[order(eval(parse(text = paste("mb$", cname, sep = "")))), ]
  rank <- 1:nrow(mb)
  plt <- ggplot(mb, aes(x = rank, y = eval(parse(text = cname)), color = Grp)) +
    geom_point() +
    scale_x_continuous(breaks = rank, labels = rownames(mb)) +
    theme(axis.text.x = element_text(face="bold", angle=90, size = lsize)) +
    ylab(cname)
  maxGrp <- max(as.integer(mb$Grp))
  if (maxGrp == 5) plt <- plt + scale_colour_manual(values = c("red", "green3", "blue", "gold2", "snow3"))
  else plt <- plt + scale_colour_manual(values = c("red", "green3", "blue", "gold2", "cyan", "violet", "snow3")) 
  plt
}

