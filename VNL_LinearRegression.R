# VNL_LinearRegression
# Written by Cristina Moody
# 21 July 2015
# Note: you need all the functions from getResultDate.R, alignThreshold.R, VNL_Classifier.R

# Installing Libraries
# source(file = "librariesVNL.R");# if starting R for 1st time today
# Libraries
library(lmtest);
library(sandwich);
library(car);
library(zoo);

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
    # Getting matrix for 'meta' patient for regression from lists
    # Function returnProperTime() from alignThreshold.R - returns PROPER_TIME and INT_FLAG
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    # Removing labs that are repeated on the same day and replacing with mean and sd
    ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames);
    # Creating ORDERING_DATE2
    ListOfDataFrames <- addORDDATE2(ListOfDataFrames);
    # Starting patients that do not cross threshold at random
    # negative days between 6 months and 2 years before threshold
    ListOfDataFrames <- startPTIME(ListOfDataFrames);
    # Creating giant dataframe of all the dataframes
    TrainDF <- getTrainDF(ListOfDataFrames);
#     # Subset TrainDF into only interesting cases (INT_FLAG==1) # Oleg said to remove
#     TrainDF <- TrainDF[TrainDF$INT_FLAG==1, ];
    # Linear regression package cannot use days, so changing to numeric
    TrainDF$PROPER_TIME <- as.numeric(TrainDF$PROPER_TIME);
    TrainDF$ORDERING_DATE2 <- as.numeric(TrainDF$ORDERING_DATE2);
    # Adding row names for regression
    rownames(TrainDF) <- paste(TrainDF$ORD_NUM_VALUE, "+/-", TrainDF$SD_ORD_VAL,
                               TrainDF$REFERENCE_UNIT, TrainDF$CPT_CODE,
                               TrainDF$COMPONENT_ID, TrainDF$ORDERING_DATE2,
                               TrainDF$PROPER_TIME, TrainDF$ORDERING_DATE,
                               TrainDF$PAT_ID, sep = "_");
    # return final dataframe
    return(TrainDF);
}

# Creating giant dataframe of all the dataframes
getTrainDF <- function(ListOfDataFrames){
    # Creating giant dataframe of all the dataframes
    TrainDF <- data.frame();
    for (j in 1:length(ListOfDataFrames)) {
        TrainDF <- rbind(ListOfDataFrames[[j]], TrainDF);
    }
    return(TrainDF);
}

# function to get total number of rows in the data frames in a list
testf <- function(testList){
    # function to get total number of rows in the data frames in a list
    m <- c();
    for (j in 1:length(testList)) {
        m[j] <- sprintf("List %s has %s rows for PAT_ID %s",
                        j, nrow(testList[[j]]), testList[[j]]$PAT_ID[1]);
    }
    return(m);
}

# Function to create ORDERING_DATE2
addORDDATE2 <- function(ListOfDataFrames) {
    # Function to create ORDERING_DATE2
    for (j in 1:length(ListOfDataFrames)){
        ListOfDataFrames[[j]]$ORDERING_DATE2 <- ListOfDataFrames[[j]]$ORDERING_DATE - ListOfDataFrames[[j]]$ORDERING_DATE[1];
    }
    return(ListOfDataFrames);
}

# Function to start PROPER_TIME for INT_FLAG==0 at some random negative time before 100 days
startPTIME <- function(ListOfDataFrames){
    # Function to start PROPER_TIME for INT_FLAG==0 at some random negative time before 100 days
    x <- sample(183:731, length(ListOfDataFrames), replace = F);
    for (j in 1:length(ListOfDataFrames)){
        if (ListOfDataFrames[[j]]$PROPER_TIME[1] == 0 & ListOfDataFrames[[j]]$INT_FLAG[1] == 0)
            ListOfDataFrames[[j]]$PROPER_TIME <- as.numeric(
                ListOfDataFrames[[j]]$PROPER_TIME) - x[j]
    }
    return(ListOfDataFrames);
}

# First linear regression
K_80048_1520_reg <- getTrainMatrix(K_80048_1520_gt20);
# Don't care for regression about what happens
# after PROPER_TIME==1 (day after threshold event)
K_80048_1520_reg <- K_80048_1520_reg[K_80048_1520_reg$PROPER_TIME < 1,]
varsK_plot <- c("PROPER_TIME", "ORD_NUM_VALUE", "ORDERING_DATE2", "INT_FLAG");
K_plot <- K_80048_1520_reg[varsK_plot];
svg("Potassium_variables.svg", width = 12, height = 8);
plot(K_plot, pch = (K_plot$INT_FLAG+2));
dev.off();
hist(K_plot$PROPER_TIME);
hist(K_plot$ORDERING_DATE2);
hist(K_plot$ORD_NUM_VALUE);
# Multiple linear regression
reg1 <- lm(PROPER_TIME ~ ORD_NUM_VALUE + ORDERING_DATE2, #+ COMPONENT_ID,
           data = K_80048_1520_reg);
summary(reg1);

reg1$robse <- vcovHC(reg1, type = "HC1");
coeftest(reg1, reg1$robse);
qqPlot(reg1, id.n = 3);
threshold1_hat <- fitted(reg1); # predicted values
as.data.frame(threshold1_hat);
threshold1_resid <- residuals(reg1); # residuals
as.data.frame(threshold1_resid);
residualPlots(reg1);
avPlots(reg1, id.n = 2, id.cex = 0.7);
# Multiple generalized linear models
reg2 <- glm(PROPER_TIME ~ ORD_NUM_VALUE + ORDERING_DATE2, #+ COMPONENT_ID,
           data = K_80048_1520_reg);
summary(reg2);

reg2$robse <- vcovHC(reg2, type = "HC1");
coeftest(reg2, reg2$robse);
threshold2_hat <- fitted(reg2); # predicted values
as.data.frame(threshold2_hat);
threshold2_resid <- residuals(reg2); # residuals
as.data.frame(threshold2_resid);
residualPlots(reg2);
avPlots(reg2, id.n = 2, id.cex = 0.6, col = "blue");

# Testing regressions
K_Test <- K_80048_1520_reg[sample(nrow(K_80048_1520_reg), 300),];
x1 <-predict(reg1, K_Test, interval="prediction");
plot(K_Test$PROPER_TIME, K_Test$ORD_NUM_VALUE)
x1 <- as.data.frame(x1)
varx2 <- c("PROPER_TIME", "ORD_NUM_VALUE", "ORDERING_DATE2", "INT_FLAG");
x2 <- K_Test[varx2];
x2 <- cbind(x2, x1);
t1 <- x2$INT_FLAG + 4;
plot(x2$PROPER_TIME, x2$fit, pch = t1, col = alpha("gray", 1));
with(data = x2, expr = errbar(x2$PROPER_TIME, x2$fit, fit + upr, fit - lwr,
                              bg = alpha("gray", 0.1), col = alpha("gray", 1),
                              pch = 21, add = T, cap = 0.01));

# New ideas
# Case 1 - when I do not have 10 days before the threshold
{
# Case 1 - when I do not have 10 days before the threshold
#
# Sample code from David for rearranging data.frame:
# Populate thisDf with some sample data; keep PT as a negative-value index with some missing elements
# And VT1 and VT2 be values or whatever
thisDf = data.frame(PT=c(0,-1,-2,-5,-6), VT1=c(0.1,-1.1,-2.1,-5.1,-6.1), VT2=c(0.2,-1.2,-2.2,-5.2,-6.2))
# Define newDf to have the completed index in PT and just fixed values for the VT1 and VT2
newDf = data.frame( PT =(-(0:10)), VT1=NA, VT2=NA )
# What this does is reference the row space of newDf that corresponds to a row in thisDf
# by looking at the value of PT in both of them.
newDf[ newDf$PT %in% thisDf$PT, ] = thisDf[ thisDf$PT %in% newDf$PT, ]
# This will unroll newDf as a vector;
# note that R is column-major storage so the transpose is necessary
# since R vectorizes matrices by columns first.
# as.vector(as.matrix(t(newDf)));
# # Change rownames to stringerized version of $PT
rownames(newDf) = sprintf("%s", newDf$PT)
Filled <- rownames(newDf)[ !is.na(newDf$VT1) ];
# What this does is append a new element to the end of newDf to make sure
# that the last value is always a good value
# We will get rid of this value at the very end, since it is superfluous.
newDf = rbind( newDf, newDf[ rownames(newDf) %in% Filled[ length(Filled) ], ] );
# Select what columns are to be replaced
namesSub <- c("VT1", "VT2");
testDf <- newDf;
testDf[namesSub] <- na.locf(testDf[namesSub], fromLast = T)
testDf <- head(testDf, -1)
newDf <- testDf;
as.vector(as.matrix(t(newDf))); # to convert to a row!
finalDf <- as.vector(as.matrix(t(newDf)));
}
# Case 2 - when I have more than 10 days before the threshold
{
# Case 2 - when I have more than 10 days before the threshold
thisDf = data.frame(PT=c(0,-1,-2,-5,-13), VT1=c(0.1,-1.1,-2.1,-5.1,-13.1),
                    VT2=c(0.2,-1.2,-2.2,-5.2,-13.2));
thisDf;
# Define newDf to have the completed index in PT and just fixed values for the VT1 and VT2
newDf = data.frame( PT =(-(0:10)), VT1=NA, VT2=NA );
newDf;
# Earlier rows
earlierDf <- thisDf[ !thisDf$PT %in% newDf$PT, ];
earlierDf;
# What this does is reference the row space of newDf that corresponds to a row in thisDf
# by looking at the value of PT in both of them.
newDf[ newDf$PT %in% thisDf$PT, ] = thisDf[ thisDf$PT %in% newDf$PT, ];
newDf;
# What this does is append a new element to the end of newDf to make sure
# that the last value is always a good value
# We will get rid of this value at the very end, since it is superfluous.
newDf = rbind( newDf, head(earlierDf, 1) );
newDf;
# Change rownames to stringerized version of $PT
rownames(newDf) = sprintf("%s", newDf$PT)
newDf;
# Select what columns are to be replaced
namesSub <- c("VT1", "VT2");
testDf <- newDf;
testDf[namesSub] <- na.locf(testDf[namesSub], fromLast = T)
testDf <- head(testDf, -1)
newDf <- testDf;
finalDf <- as.data.frame(t(as.vector(as.matrix(t(newDf))))); # to convert to a row!
finalDf;
as.data.frame(t(as.vector(as.matrix(t(newDf)))))
}

# nms creates a character vector with "_day" to rename the columns in a dataframe
nms <- function(n) {
    # nms creates a character vector with "_day" to rename the columns in a dataframe
    ns <- c();
    for (i in -10:0) {
        ns <- cbind( t(paste(n, i, sep = "_")), ns);
    }
    return(t(ns));
}

# Case 1 function
case1 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA );
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    LastBeforeTen <- as.numeric(thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ]$PROPER_TIME[1]);
    # LastBeforeTen is NA when patient returns.
    if(is.na(LastBeforeTen)){print("NO LastBeforeTen in case1()"); next;}
    newRow <- newDf[ newDf$PROPER_TIME == LastBeforeTen, ];
    newDf <- rbind(newRow, newDf); # Add last measured value before 10 days before threshold
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F)
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    newDf <- head(newDf, -1); # Removing fill row
    # rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric and all dates to Dates!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- lapply(finalDf[datcols], as.Date, format = "%Y-%m-%d"); # Change columns to Dates
    finalDf <- cbind(finalDf$ORDERING_DATE2_0, finalDf); # Adding Threshold Time to dataframe
    names(finalDf)[names(finalDf)=="finalDf$ORDERING_DATE2_0"] <- "THRESHOLD_TIME";
    return(finalDf);
}
# Case 2 function
case2 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA );
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    # Getting last filled PROPER_TIME in thisDf in the 10 days before threshold
    LastBeforeTen <- as.numeric(thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ]$PROPER_TIME[1]);
    if (is.na(LastBeforeTen)) {LastBeforeTen <- -10.1;}
    # Getting the measurement before 10 days from threshold
    newRow <- thisDf[ thisDf$PROPER_TIME < LastBeforeTen, ]; # Data frame of values before 10 days before threshold
    newRow <- newRow[nrow(newRow), ]; # first values before 10 days before threshold
    newDf <- rbind(newRow, newDf); # Add last measured value before 10 days before threshold
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F);
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    newDf <- head(newDf, -1); # Removing fill row
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric and all dates to Dates!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- lapply(finalDf[datcols], as.Date, format = "%Y-%m-%d"); # Change columns to Dates
    finalDf <- cbind(newRow$PROPER_TIME, finalDf); # Adding Threshold Time to dataframe
    finalDf$`newRow$PROPER_TIME` <- 0 - finalDf$`newRow$PROPER_TIME`;
    names(finalDf)[names(finalDf)=="newRow$PROPER_TIME"] <- "THRESHOLD_TIME";
    return(finalDf);
}
# Case 3 function
case3 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA );
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F);
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric and all dates to Dates!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- lapply(finalDf[datcols], as.Date, format = "%Y-%m-%d"); # Change columns to Dates
    finalDf <- cbind(finalDf$ORDERING_DATE2_0, finalDf); # Adding Threshold Time to dataframe
    names(finalDf)[names(finalDf)=="finalDf$ORDERING_DATE2_0"] <- "THRESHOLD_TIME";
    return(finalDf);
}

# Creates a training dataframe that will keep track of each point in time
reorderPT <- function(ListOfDataFrames){
    # Creates a training dataframe that will keep track of each point in time
    # Starts after use of
    #     > ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    #     > ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames);
    #     > ListOfDataFrames <- addORDDATE2(ListOfDataFrames);
    #     > ListOfDataFrames <- startPTIME(ListOfDataFrames);
    #       names(ListOfDataFrames[[j]])
    #       [1] "ORDERING_DATE"  "ORD_NUM_VALUE"  "REFERENCE_UNIT" "PAT_ID"         "REFERENCE_HIGH" "REFERENCE_LOW"
    #       [7] "CPT_CODE"       "COMPONENT_ID"   "MIN_RAW_LABS"   "PROPER_TIME"    "INT_FLAG"       "SD_ORD_VAL"
    #       [13] "ORDERING_DATE2"
    newOrder <- data.frame();
    varsNotChanging <- c("ENC_CSN_ID", "PAT_ID", "MIN_RAW_LABS", "INT_FLAG",
                         "CPT_CODE", "COMPONENT_ID",
                         "REFERENCE_UNIT", "REFERENCE_LOW", "REFERENCE_HIGH");
    varsChanging <- c("PROPER_TIME", "ORD_NUM_VALUE", "SD_ORD_VAL",  #### DO NOT CHANGE THIS VARIABLE (YOU WILL BREAK IT)
                      "ORDERING_DATE2", "ORDERING_DATE"); # Plus WEIGHT
    for (j in 1:length(ListOfDataFrames)){
        ChangingDF <- ListOfDataFrames[[j]][varsChanging];
        NotChangingDF <- ListOfDataFrames[[j]][varsNotChanging][1,];
        # Changing days to numeric
        ChangingDF$PROPER_TIME <- as.numeric(ChangingDF$PROPER_TIME);
        ChangingDF$ORDERING_DATE2 <- as.numeric(ChangingDF$ORDERING_DATE2);
        ChangingDF$ORDERING_DATE <- as.character(ChangingDF$ORDERING_DATE);
        # Normalizing ORD_NUM_VALUE to ORD_NUM_VALUE/REFERENCE_HIGH
        ChangingDF$ORD_NUM_VALUE <- ChangingDF$ORD_NUM_VALUE/f2n(NotChangingDF$REFERENCE_HIGH);
        ChangingDF$SD_ORD_VAL <- ChangingDF$SD_ORD_VAL/f2n(NotChangingDF$REFERENCE_HIGH);
        if (as.numeric(min(ChangingDF$PROPER_TIME)) > -10) {
            print(j); print("AAAAAA");
            ChangingDF <- case1(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) < -10) {
            print(j); print("BBBBBB");
            ChangingDF <- case2(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) == -10){
            print(j); print("CCCCCC");
            ChangingDF <- case3(ChangingDF, varsChanging);
        } else {print("BREAK"); break;}
        Order <- cbind(NotChangingDF, ChangingDF);
        rownames(Order) <- sprintf("%s.%s_%s", NotChangingDF$ENC_CSN_ID,
                                   NotChangingDF$PAT_ID, ChangingDF$`ORDERING_DATE_-10`);
        newOrder <- rbind(Order, newOrder);
    }
    return(newOrder);
}

# Getting matrix for 'meta' patient for regression from lists
getTimeTrainMatrix <- function(originalListOfDataFrames){
    # Getting matrix for 'meta' patient for regression from lists
    # Function returnProperTime() from alignThreshold.R - returns PROPER_TIME and INT_FLAG
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    if (class(ListOfDataFrames)=='numeric') {print("NO SERIES PASSED PROPER_TIME CUT"); break;}
    # Removing labs that are repeated on the same day and replacing with mean and sd
    ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames);
    # Creating ORDERING_DATE2
    ListOfDataFrames <- addORDDATE2(ListOfDataFrames);
    # Starting patients that do not cross threshold at random
    # negative days between 6 months and 2 years before threshold
    ListOfDataFrames <- startPTIME(ListOfDataFrames);
    # Organizing into giant dataframe with only 10 days before threshold
    TrainDF <- reorderPT(ListOfDataFrames);
    #     # Subset TrainDF into only interesting cases (INT_FLAG==1) # Oleg said to remove
    #     TrainDF <- TrainDF[TrainDF$INT_FLAG==1, ];
    # return final dataframe
    return(TrainDF);
}

# Making one big dataframe
makeTimeDF = 0;
if (makeTimeDF){
        BUN_80048_1518_reg <- getTimeTrainMatrix(BUN_80048_1518_gt20)
        BUN_80053.01_1518_reg <- getTimeTrainMatrix(BUN_80053.01_1518_gt20)
        BUN_80069_1518_reg <- getTimeTrainMatrix(BUN_80069_1518_gt20)
        Creat_80048_1523_reg <- getTimeTrainMatrix(Creat_80048_1523_gt20)
        Creat_80053.01_1523_reg <- getTimeTrainMatrix(Creat_80053.01_1523_gt20)
        Creat_80069_1523_reg <- getTimeTrainMatrix(Creat_80069_1523_gt20)
        K_80048_1520_reg3 <- getTimeTrainMatrix(K_80048_1520_gt20);
        K_80053.01_1520_reg <- getTimeTrainMatrix(K_80053.01_1520_gt20)
        K_80069_1520_reg <- getTimeTrainMatrix(K_80069_1520_gt20)
        Na_80048_1519_reg <- getTimeTrainMatrix(Na_80048_1519_gt20)
        Na_80053.01_1519_reg <- getTimeTrainMatrix(Na_80053.01_1519_gt20)
        Na_80069_1519_reg <- getTimeTrainMatrix(Na_80069_1519_gt20)
        PLATE_85027_1504_reg <- getTimeTrainMatrix(PLATE_85027_1504_gt20)
        PLATE_CBCD_1504_reg <- getTimeTrainMatrix(PLATE_CBCD_1504_gt20)
        WBC_85027_1496_reg <- getTimeTrainMatrix(WBC_85027_1496_gt20)
        WBC_CBCD_1496_reg <- getTimeTrainMatrix(WBC_CBCD_1496_gt20)

        # Making into one list
        goodDataOrdered10DaysBeforeThreshold <- rbind(BUN_80048_1518_reg, BUN_80053.01_1518_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, Creat_80048_1523_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, Creat_80053.01_1523_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, K_80048_1520_reg3);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, K_80053.01_1520_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, K_80069_1520_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, Na_80048_1519_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, Na_80053.01_1519_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, PLATE_CBCD_1504_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, PLATE_85027_1504_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, WBC_85027_1496_reg);
        goodDataOrdered10DaysBeforeThreshold <- rbind(goodDataOrdered10DaysBeforeThreshold, WBC_CBCD_1496_reg);

        save(goodDataOrdered10DaysBeforeThreshold,
             BUN_80048_1518_reg, BUN_80053.01_1518_gt20,
             Creat_80048_1523_reg, Creat_80053.01_1523_reg,
             K_80048_1520_reg3, K_80053.01_1520_reg, K_80069_1520_reg,
             Na_80048_1519_reg, Na_80053.01_1519_reg,
             PLATE_CBCD_1504_reg, PLATE_85027_1504_reg,
             WBC_85027_1496_reg, WBC_CBCD_1496_reg,
             file = sprintf("%s%s", dDir, "TimeOrdered_lists_gt20.rda"));
}

metaPatient <- goodDataOrdered10DaysBeforeThreshold[
    goodDataOrdered10DaysBeforeThreshold$INT_FLAG>0,];

# Sorting into testing and training sets
n.points <- nrow(metaPatient);
sampling.rate <- 0.8;
num.test.set.labels <- n.points * (1 - sampling.rate);
training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE);
trainset <- subset(metaPatient[training, ]);
testing <- setdiff(1:n.points, training);
testset <- subset(metaPatient[testing, ]);

## 3rd linear regression
reg3 <- lm(`THRESHOLD_TIME` ~ `COMPONENT_ID` + #`INT_FLAG` +
                `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
                `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
                `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
                `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
                `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
                `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
                `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
                `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`, #+ COMPONENT_ID,
            data = trainset);
summary(reg3);

reg3$robse <- vcovHC(reg3, type = "HC1");
coeftest(reg3, reg3$robse);
threshold3_hat <- fitted(reg3); # predicted values
as.data.frame(threshold2_hat);
threshold3_resid <- residuals(reg3); # residuals
as.data.frame(threshold2_resid);
residualPlots(reg3);
avPlots(reg3, id.n = 2, id.cex = 0.6, col = "blue");

# Testing regressions
# reg_Test <- goodDataOrdered10DaysBeforeThreshold[sample(
#     nrow(goodDataOrdered10DaysBeforeThreshold[
#         goodDataOrdered10DaysBeforeThreshold$INT_FLAG>0,]), 100),];
reg_Test <- testset;
x3 <- predict(reg3, reg_Test, interval="prediction");
x3 <- as.data.frame(x3)
varx3 <- c("THRESHOLD_TIME", "ORD_NUM_VALUE_-5", "ORDERING_DATE2_-5", "INT_FLAG");
x3.1 <- reg_Test[varx3];
x3 <- cbind(x3.1, x3);
t3 <- x3$INT_FLAG + 22;
x3 <- as.data.frame(x3)
svg("3rdPrediction_VNL_LinearRegression.svg", width = 7, height = 7);
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
with(data = x3, expr = errbar(x3$THRESHOLD_TIME, x3$fit, fit + upr, fit - lwr,
                              bg = alpha("black", 0.1), errbar.col = alpha("black", 0.4),
                              pch = "", add = T, cap = 0.01));
dev.off();
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, col = alpha("blue", 1), bg = alpha("blue", .5), panel.first = grid());

