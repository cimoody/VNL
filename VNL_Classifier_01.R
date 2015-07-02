## Code for attempt at creating a classifier for Value Next Lab data
##
## Created by Cristina I Moody
## July 2015

# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);

# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("plyr");
library(plyr);

# Package for classification
install.packages("rpart");
library(rpart);

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

# Create column for whether a result is interesting
INT_FLAG <- 0;

# Create a data frame with only interested variables
# Use as.numeric(levels(f))[f] after removing "<null>" entry
flag <- data.frame("ENC_CSN_ID" = goodlab$ENC_CSN_ID,
                   "CPT_CODE" = goodlab$CPT_CODE,
                   "COMPONENT_ID" = goodlab$COMPONENT_ID,
                   "ORD_NUM_VALUE" = goodlab$ORD_NUM_VALUE,
                   "REF_LOW" = goodlab$REFERENCE_LOW,
                   "REF_HIGH" = goodlab$REFERENCE_HIGH,
                   INT_FLAG); # 1048575 rows
# Removing invaled results with "<null>" in ORD_NUM_VALUE
rejected <- flag[as.character(flag$ORD_NUM_VALUE)=="<null>",]; # Rejects 6650 rows
flag <- flag[as.character(flag$ORD_NUM_VALUE)!="<null>",]; # 1041925 rows
# Remove invalid results with ORD_NUM_VALUE==9999999
rejected <- flag[as.numeric(levels(flag$ORD_NUM_VALUE)
                            [flag$ORD_NUM_VALUE])>=999999,]; # Saving to examine, 228571 rows
flag <- flag[as.numeric(levels(flag$ORD_NUM_VALUE)
                        [flag$ORD_NUM_VALUE])<9999999,]; # good results, 813359 rows
# Converting ORD_NUM_VALUE to a number!
flag$ORD_NUM_VALUE <- as.numeric(levels(flag$ORD_NUM_VALUE)
                                [flag$ORD_NUM_VALUE]);
