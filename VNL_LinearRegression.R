# VNL_LinearRegression
# Written by Cristina Moody
# 21 July 2015
# Note: you need all the functions from getResultDate.R, alignThreshold.R, VNL_Classifier.R

# Installing Libraries
source(file = "librariesVNL.R");# if starting R for 1st time today

# Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- sprintf("%s%s", getwd(), "/");
# Data directory: E:/VNL Data from Joe
dDir <- "E:/VNL\ Data\ from\ Joe/";
# Data filenames

# Loading all files - it won't work as a function :(
listfiles <- list.files(path = dDir, pattern = "\\.rda");
for (i in 1:length(listfiles)) {
    load(file = sprintf("%s%s", dDir, listfiles[i]));
}

# I want to 'train' or create my model on only the patients that have labs
# that evolve in time to cross the threshold. I want to consider this the 'meta' patient.
# I want:
#           A) Given 'meta' patient lab x(ti), output t0-ti - time to threshold.
# Then:
#           B) Given any patient lab x(ti), output x(t0) (when/if the threshold is crossed)
# TimeMod1 <- lm(PROPER_TIME ~ ORDERING_DATE + ORD_NUM_VALUE + COMPONENT_ID) on subset
# of patients that start within the normal range and cross the upper threshold of normal
# (defined by INT_FLAG ==  1)
# Difficulty is that there are several different definitions of time that need to be
# address:
#       i) There is PROPER_TIME defined as the time until/since initial threshold event
#       ii) There is calendar time defined in ORDERING_DATE - this is not a useful time
#       iii) There is time from first measurement
# I need to adjust ORDERING_DATE to ORDERING_DATE2 defined as iii) for the 'meta' patient.
# To answer A), I need time i) for PROPER_TIME and time iii) for ORDERING_DATE.
# To answer B)

# Getting matrix for 'meta' patient for regression from lists
getTrainMatrix <- function(originalListOfDataFrames){
    # Function returnProperTime() from alignThreshold.R - returns PROPER_TIME and INT_FLAG
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    # Creating giant dataframe of all the dataframes
    TrainDF <- getTrainDF(ListOfDataFrames);
    # Subset TrainDF into only interesting cases (INT_FLAG==1)
    TrainDF <- TrainDF[TrainDF$INT_FLAG==1, ];
}

# Creating giant dataframe of all the dataframes
getTrainDF <- function(ListOfDataFrames){
    TrainDF <- data.frame();
    for (j in 1:length(ListOfDataFrames)) {
        TrainDF <- rbind(ListOfDataFrames[[j]], TrainDF);
    }
    return(TrainDF);
}

testf <- function(testList){
    m <- c();
    for (j in 1:length(testList)) {
        m[j] <- sprintf("List %s has %s rows.", j, nrow(testList[[j]]));
    }
    return(m);
}
