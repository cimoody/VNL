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
        m[j] <- sprintf("List %s has %s rows.", j, nrow(testList[[j]]));
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
        if (ListOfDataFrames[[j]]$PROPER_TIME[1] == 0)
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
plot(x2$PROPER_TIME, x2$fit, pch = t1, col = alpha("gray", 0.5));
with(data = x2, expr = errbar(x2$PROPER_TIME, x2$fit, fit + upr, fit - lwr,
                              bg = alpha("gray", 0.1), col = alpha("gray", 0.1),
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
thisDf;
# Define newDf to have the completed index in PT and just fixed values for the VT1 and VT2
newDf = data.frame( PT =(-(0:10)), VT1=NA, VT2=NA )
newDf;
# What this does is reference the row space of newDf that corresponds to a row in thisDf
# by looking at the value of PT in both of them.
newDf[ newDf$PT %in% thisDf$PT, ] = thisDf[ thisDf$PT %in% newDf$PT, ]
newDf;
# This will unroll newDf as a vector;
# note that R is column-major storage so the transpose is necessary
# since R vectorizes matrices by columns first.
as.vector(as.matrix(t(newDf)));
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

nms <- function(n) {
    # nms creates a character vector with "_day" to rename the columns in a dataframe
    ns <- c();
    for (i in -10:0) {
        ns <- cbind( t(paste(n, i, sep = "_")), ns);
    }
    return(t(ns));
}

