## Code for attempt at creating a classifier for Value Next Lab data
##
## Created by Cristina I Moody
## July 2015

# Set random seed
sseed = 42;

# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);

# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("plyr");
library(plyr);

# Package for classification
library(rpart);
install.packages('rattle');
library(rattle);
install.packages('rpart.plot');
library(rpart.plot);
install.packages('RColorBrewer');
library(RColorBrewer);
install.packages('randomForest');
library(randomForest);
set.seed(sseed);

# Additional package for categorical graphics
install.packages("vcd");
library(vcd);

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
