## Code for attempt at creating a classifier for Value Next Lab data
##
## Created by Cristina I Moody
## July 2015

# Installing Libraries
# source(file = "librariesVNL.R");# if starting R for 1st time today

# Installing Gaussian Process Library
library(tgp);
# Installing package to compare the two data tables
library(compare);
library(plyr);
library(Hmisc);
# Package for classification
library(rpart);
library(rattle);
library(rpart.plot);
library(RColorBrewer);
library(randomForest);
library(party);
# Additional package for categorical graphics
library(vcd);


# Set random seed
sseed = 42;
set.seed(sseed);

# From VNL_Model_01.R
# Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- getwd();
# Data directory: E:/VNL Data from Joe
dDir <- "E:/VNL\ Data\ from\ Joe/";
# Data filenames
data.popname = sprintf("%s%s", dDir, "di_populations_v1.1.csv");
data.labname = sprintf("%s%s", dDir, "di_lab_data_v1.1.csv");
# Loading already saved rda files:
list.files(path = "E:/VNL\ Data\ from\ Joe/", pattern = "\\.rda");
load("E:/VNL\ Data\ from\ Joe/goodData.rda");


# Create a data frame with only interested variables
# Use as.numeric(levels(f))[f] after removing "<null>" entry
flag <- data.frame("ENC_CSN_ID" = goodlab$ENC_CSN_ID,
                   "CPT_CODE" = goodlab$CPT_CODE,
                   "COMPONENT_ID" = goodlab$COMPONENT_ID,
                   "ORD_NUM_VALUE" = goodlab$ORD_NUM_VALUE,
                   "REF_LOW" = goodlab$REFERENCE_LOW,
                   "REF_HIGH" = goodlab$REFERENCE_HIGH); # 1048575 rows
# CLEANING UP THE TABLE
# Removing invaled results with "<null>" in ORD_NUM_VALUE
rejected <- flag[flag$ORD_NUM_VALUE=="<null>",]; # Rejects 6650 rows
flag <- flag[flag$ORD_NUM_VALUE!="<null>",]; # 1041925 rows
# Remove invalid results with ORD_NUM_VALUE==9999999
rejected <- flag[as.numeric(levels(flag$ORD_NUM_VALUE)
                            [flag$ORD_NUM_VALUE])>=999999,]; # Saving to examine, 228571 rows
flag <- flag[as.numeric(levels(flag$ORD_NUM_VALUE)
                        [flag$ORD_NUM_VALUE])<9999999,]; # good results, 813359 rows
# Converting ORD_NUM_VALUE to a number!
flag$ORD_NUM_VALUE <- as.numeric(levels(flag$ORD_NUM_VALUE)
                                 [flag$ORD_NUM_VALUE]);
# Removing ORD_NUM_VALUE where both REF_LOW & REF_HIGH are "<null>" factors
flagtst <- flag[flag$REF_LOW!="<null>"
                & flag$REF_HIGH!="<null>",]; # 763185 rows, Rejected all rows with a <null> value
rejected <- flag[flag$REF_LOW=="<null>"
                 & flag$REF_HIGH=="<null>",]; # 29411 rows rejected
flag <- flag[!(flag$REF_LOW=="<null>" & flag$REF_HIGH=="<null>"), ]; # 783948
# Use
# f$REF_HIGH <- gsub("<null>", "8888888", as.character(f$REF_HIGH))
# to replace "<null>"
flag$REF_HIGH <- gsub("<null>", "8888888", as.character(flag$REF_HIGH));
# Dummy character to convert to numeric for high limit is "8888888"
# Replacing flag$REF_LOW of "NEG" with "-0"
flag$REF_LOW <- gsub("NEG", "-0.00000000001", as.character(flag$REF_LOW));
flag$REF_LOW <- gsub(">", "", as.character(flag$REF_LOW));
flag$REF_LOW <- gsub("<", "", as.character(flag$REF_LOW));
# Rejecting rows with values of REF_LOW that are not numeric
flag <- flag[!is.na(as.numeric(as.character(flag$REF_LOW))), ]; # 783348 rows
# Converting all values of REF_LOW and REF_HIGH to numeric!
flag$REF_LOW <- as.numeric(flag$REF_LOW);
flag$REF_HIGH <- as.numeric(flag$REF_HIGH);
# DONE CLEANING UP THE TABLE!

# Flagging test as interesting if it crosses threshold of REF_LOW or REF_HIGH
# INTERESTING is defined as 1 !!
flag$INT_FLAG <- as.numeric((flag$ORD_NUM_VALUE < flag$REF_LOW)
                            | (flag$ORD_NUM_VALUE > flag$REF_HIGH)); # Creating Flag!

# 15 July 2015 - Dropping previous inquiry line with flag$INT_FLAG because
# it does not consider overall patient timeline. It MAY have been possible to
# continue it, but it would be more work and code already does what I need in
# alignThreshold.R
# Instead added INT_FLAG to table from alignThreshold.R code. Continuing
# classifier work from getMeanSDListDataFrames() and from returnProperTime()
# functions from code in alignThreshold.R .

# Loading all files - it won't work as a function :(
# loadfiles <- function() {
listfiles <- list.files(path = dDir, pattern = "\\.rda");
for (i in 1:length(listfiles)) {
    load(file = sprintf("%s%s", dDir, listfiles[i]));
}
# }
# loadfiles();
# Function to get flag value from list of dataframes returned from returnProperTime()
# or form getMeanSDListDataFrames() and add flag to goodpop data. Classifier
# is looking for features from population data that is 'time-independent'.
flagPopData <- function(popData, labData, originalListOfDataFrames) {
    # Function to get flag value from list of dataframes returned from returnProperTime()
    # or form getMeanSDListDataFrames() and add flag to goodpop data. Classifier
    # is looking for features from population data that is 'time-independent'.newPop <- data.frame();
    patFlag <- data.frame();
    # Function returnProperTime() from alignThreshold.R
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    for (j in 1:length(ListOfDataFrames)) {
        patFlag <- rbind(patFlag,
                            data.frame("PAT_ID" = ListOfDataFrames[[j]]$PAT_ID[1],
                                       "INT_FLAG" = ListOfDataFrames[[j]]$INT_FLAG[1],
                                       "ENC_CSN_ID" = labData[
                                           which(labData$PAT_ID
                                                 ==ListOfDataFrames[[j]]$PAT_ID[1]),
                                           "ENC_CSN_ID"][1]) );
        newPop <- rbind(newPop, popData[which(popData$ENC_CSN_ID==patFlag$ENC_CSN_ID[j]),] );
        # newPop$INT_FLAG <- patFlag$INT_FLAG[j];
    }
    newPop <- cbind(newPop, "INT_FLAG"=patFlag$INT_FLAG);
    return(newPop);
}

# Creating a dataframe with the interesting flag for K tests
# popData_K_80048_1520 <- flagPopData(popData = goodpop, labData = goodlab,
#                                     ListOfDataFrames = mspK_80048_1520_gt20);
# popData_K_80053.01_1520 <- flagPopData(popData = goodpop, labData = goodlab,
#                                        ListOfDataFrames = mspK_80053.01_1520_gt20);
# popData_K_80069_1520 <- flagPopData(popData = goodpop, labData = goodlab,
#                                     ListOfDataFrames = mspK_80069_1520_gt20);
popData_Creat_80048 <- flagPopData(goodpop, goodlab, Creat_80048_1523_gt20);
popData_Creat_80053.01 <- flagPopData(goodpop, goodlab, Creat_80053.01_1523_gt20);
popData_Creat <- rbind(popData_Creat_80048, popData_Creat_80053.01);
popData_K_80048 <- flagPopData(goodpop, goodlab, K_80048_1520_gt20);
popData_K_80053.01 <- flagPopData(goodpop, goodlab, K_80053.01_1520_gt20);
popData_K_80069 <- flagPopData(goodpop, goodlab, K_80069_1520_gt20);

# # Final dataframe for K tests 116 rows or patients to test on
popData_K <- rbind(popData_K_80048, popData_K_80069, popData_K_80053.01);


data <- popData_K;
prop.table(table(data$INT_FLAG));

# Dividing into test (25%) and train (75%) sets - from stackoverflow
smp_size <- floor(0.75 * nrow(data));
## set the seed to make your partition reproductible
set.seed(sseed);


# Exploring data
attach(data);
prop.table(table(data$INT_FLAG));

# Separating AGE into decade bins for classifier
data$AGE2[data$AGE < 30 & data$AGE >= 20] <- '20-30';
data$AGE2[data$AGE < 40 & data$AGE >= 30] <- '30-40';
data$AGE2[data$AGE < 50 & data$AGE >= 40] <- '40-50';
data$AGE2[data$AGE < 60 & data$AGE >= 50] <- '50-60';
data$AGE2[data$AGE < 70 & data$AGE >= 60] <- '60-70';
data$AGE2[data$AGE < 80 & data$AGE >= 70] <- '70-80';
data$AGE2[data$AGE < 90 & data$AGE >= 80] <- '80-90';
# Separating into a train and test set
train_ind <- sample(seq_len(nrow(data)), size = smp_size);
traindata <- data[train_ind, ];
testdata <- data[-train_ind, ];

# attach(data);
aggregate(INT_FLAG ~ AGE2, data=traindata, FUN=function(x) {sum(x)/length(x)});
# data$AGE2 data$INT_FLAG
# 1     20-30     0.6666667
# 2     30-40     0.5000000
# 3     40-50     0.2222222
# 4     50-60     0.5000000
# 5     60-70     0.4838710
# 6     70-80     0.2647059
# 7     80-90     0.5500000
aggregate(INT_FLAG ~ SEX_C, data=traindata, FUN=function(x) {sum(x)/length(x)});
# data$SEX_C data$INT_FLAG
# 1          1     0.5000000
# 2          2     0.3636364
fit <- rpart(INT_FLAG ~ SEX_C + AGE2, data=traindata, method="class");
fancyRpartPlot(fit);
summary(fit)
# Call:
#     rpart(formula = data$INT_FLAG ~ data$SEX_C + data$AGE2, data = data,
#           method = "class")
# n= 116
#
# CP nsplit rel error   xerror      xstd
# 1 0.06122449      0 1.0000000 1.000000 0.1085701
# 2 0.02040816      3 0.8163265 1.102041 0.1096397
# 3 0.01000000      5 0.7755102 1.000000 0.1085701
#
# Variable importance
# data$AGE2 data$SEX_C
# 94          6
#
# Node number 1: 116 observations,    complexity param=0.06122449
# predicted class=0  expected loss=0.4224138  P(node) =1
# class counts:    67    49
# probabilities: 0.578 0.422
# left son=2 (43 obs) right son=3 (73 obs)
# Primary splits:
#     data$AGE2  splits as  RRLRRLR, improve=3.586120, (1 missing)
# data$SEX_C < 1.5 to the right, improve=1.057994, (0 missing)
#
# Node number 2: 43 observations
# predicted class=0  expected loss=0.255814  P(node) =0.3706897
# class counts:    32    11
# probabilities: 0.744 0.256
#
# Node number 3: 73 observations,    complexity param=0.06122449
# predicted class=1  expected loss=0.4794521  P(node) =0.6293103
# class counts:    35    38
# probabilities: 0.479 0.521
# left son=6 (40 obs) right son=7 (33 obs)
# Primary splits:
#     data$SEX_C < 1.5 to the right, improve=0.3671440, (0 missing)
# data$AGE2  splits as  RL-LL-R, improve=0.1780785, (1 missing)
# Surrogate splits:
#     data$AGE2 splits as  LR-LL-L, agree=0.575, adj=0.061, (0 split)
#
# Node number 6: 40 observations,    complexity param=0.06122449
# predicted class=0  expected loss=0.475  P(node) =0.3448276
# class counts:    21    19
# probabilities: 0.525 0.475
# left son=12 (30 obs) right son=13 (10 obs)
# Primary splits:
#     data$AGE2 splits as  LL-LL-R, improve=1.35, (0 missing)
#
# Node number 7: 33 observations,    complexity param=0.02040816
# predicted class=1  expected loss=0.4242424  P(node) =0.2844828
# class counts:    14    19
# probabilities: 0.424 0.576
# left son=14 (22 obs) right son=15 (11 obs)
# Primary splits:
#     data$AGE2 splits as  RR-RL-L, improve=0.9101732, (1 missing)
#
# Node number 12: 30 observations
# predicted class=0  expected loss=0.4  P(node) =0.2586207
# class counts:    18    12
# probabilities: 0.600 0.400
#
# Node number 13: 10 observations
# predicted class=1  expected loss=0.3  P(node) =0.0862069
# class counts:     3     7
# probabilities: 0.300 0.700
#
# Node number 14: 22 observations,    complexity param=0.02040816
# predicted class=0  expected loss=0.5  P(node) =0.1896552
# class counts:    11    11
# probabilities: 0.500 0.500
# left son=28 (10 obs) right son=29 (12 obs)
# Primary splits:
#     data$AGE2 splits as  ----R-L, improve=0.221645, (1 missing)
#
# Node number 15: 11 observations
# predicted class=1  expected loss=0.2727273  P(node) =0.09482759
# class counts:     3     8
# probabilities: 0.273 0.727
#
# Node number 28: 10 observations
# predicted class=0  expected loss=0.4  P(node) =0.0862069
# class counts:     6     4
# probabilities: 0.600 0.400
#
# Node number 29: 12 observations
# predicted class=1  expected loss=0.4166667  P(node) =0.1034483
# class counts:     5     7
# probabilities: 0.417 0.583
pred <- predict(fit, testdata);
mean(pred == testdata[ , "INT_FLAG"]);

