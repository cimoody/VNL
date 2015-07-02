## Code for Value Next Lab
##
## Created by Cristina I Moody
## June 2015

# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);

# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("plyr");
library(plyr);

## Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- getwd();
## Data directory: E:/VNL Data from Joe
dDir <- "E:/VNL\ Data\ from\ Joe/";
## Data filenames
data.popname = sprintf("%s%s", dDir, "di_populations_v1.1.csv");
data.labname = sprintf("%s%s", dDir, "di_lab_data_v1.1.csv");
# ## sprintf() suggested for more control than paste() by D. Sanchez 6/25/15
# data.popname <- paste(dDir, "di_populations_v1.1.csv", sep ="");
# data.labname <- paste(dDir, "di_lab_data_v1.1.csv", sep ="");

### Don't need to repeat these steps every time
# ## Reading the data into memory
# ## D. Sanchez suggested always using stringsAsFactors = FALSE, because it gives
# ## more flexibility in combining fields (as you will probably need to do).
# data.pop <- read.csv(file = data.popname, header = TRUE, stringsAsFactors = FALSE);
# data.lab <- read.csv(file = data.labname, header = TRUE, stringsAsFactors = FALSE);
# ## Saving the data in R for manipulation
# save(data.pop, file = sprintf("%s%s", dDir, "dataPop.rda"));
# save(data.lab, file = sprintf("%s%s", dDir, "dataLab.rda"));
# ## Listing all R processing data in directory
# list.files(path = dDir, pattern = "\\.rda");
# ## Loading R processing files to work wtih
# load("E:/VNL\ Data\ from\ Joe/dataPop.rda");
# load("E:/VNL\ Data\ from\ Joe/dataLab.rda");
# str(data.lab); # Returns the structure of data.lab
# # Dropping column "X" - appears to be all NA - and saving to a new data.frame named goodlab
# # Checked by isolating the column by
# #  chck <- data.lab[,"X"]; # Creates vector of just the column to check
# #  dc <- chck[!is.na(chck)]; # Creates a vector of any values in chck that are not NA
# ## if dc returns 'logical(0)' to screen, then confirmed that column checked is all NA
# ## Easier way to check for all-NA from D. Sanchez 6/25/15
# # This exploits the fact that is.na returns a vector of booleans, and the typing system in R
# # coerces bools to numerics upon arithmetic operations (TRUE casts to 1, FALSE to 0).  So even
# # though the + 0 part is arithmetically pointless, it ensures that the bools are cast to ints so
# # that sum collects them.  If everything is NA, then the sum is going to be equal to the length.
# if( sum( is.na(data.lab$X) + 0 ) >= length( data.lab$X ) ) {
#   print(" X is all NA\n");
# } # from D. Sanchez
#
# drops <- c("X");
# goodlab <- data.lab[,!(names(data.lab) %in% drops)];
# # Checking that goodlab is the same as data.lab without the "X" column
# str(goodlab);
# str(data.pop); # Returns the structure of data.lab
# # Dropping column "X", "X.1" - appear to be all NA - and saving to a new data.frame
# # named goodpop
# drops <- c("X", "X.1");
# goodpop <- data.pop[,!(names(data.pop) %in% drops)];
# # Checking that goodlab is the same as data.lab without the "X" column
# str(goodpop);
#
# ## Saving the good data in R for manipulation
# save(goodpop, goodlab, file = "E:/VNL\ Data\ from\ Joe/goodData.rda");
# # Verifying saved files

list.files(path = "E:/VNL\ Data\ from\ Joe/", pattern = "\\.rda");
load("E:/VNL\ Data\ from\ Joe/goodData.rda");

# Figure out the top labs per patient
# Make a variable that is a combination of ENC_CSN_ID, CPT_CODE, COMPONENT_ID
# CPT_CODE & COMPONENT_ID are the lab
labnames <- paste(goodlab$ENC_CSN_ID, goodlab$CPT_CODE, goodlab$COMPONENT_ID, sep = "_");
# Use table to get the frequency of each combined patient & lab
topLabs <- table(labnames);
# Order these by frequency
df <- topLabs;
df <- df[order(df)];
# Check that they are ordered by frequency
plot(df);
# Decide on a cut-off for rare labs
#   >=          Number Available
#    1          309808
#    2          161320
#    3          110570
#    4           81226
#    5           60487
#   10           19639
#   15            9189
#   20            5272
#   25            3256
#   30            2080
#   35            1446
#   40            1117
#   45             757
#   50             568
cutoff <- 50;
dfNotRare <- df[df>=cutoff];
plot(dfNotRare);

# Notes from Dave in R snippet on how to find the top Labs:
# # Figure out the top labs
# df$labNames = paste( df$lab_id, df$comp_id );
# topLabs = sort( table( df$labNames) ); # might need to play around with arguments to sort
#                     # because table() is going to return a wide vector and not a data frame
# topLabs = topLabs[,1:50];
# topLabs = topLabs[1,];   # should extract just the names of the labs you want
# # This should subset df according to whether a row contains a top-50 element
# df = df[ df$labNames %in% topLabs, ];


# Want to find an interesting place for a cut-off. Plotting
coVal <- seq(1:100); # for x axis
# Creating function for y(x). There is probably an easier way to do this.
fun <- function(c){
    xv <- length(df[df>=c])
    return(xv);
}
patWlabs <- c(rep(0, 100));
patWlabs <- lapply(coVal, fun); # for y axis
# Creating plot and saving plot
png("Patients_w_Repeated_Labs_20150626.png", width = 600, height = 500, units = "px");
plot(coVal, patWlabs, xlab = "Minimum Repeated Labs per Patient",
     ylab = "Log(No. of Patients)", xlim = c(1,100),
     panel.first = grid(), log = "y", col = "blue");
dev.off();
graphics.off();

# Determining which labs pass cutoff <= 50;
# Accessing the row names in the table (since they are ID_lab)
rn <- row.names(dfNotRare);
rn <- strsplit(rn, "_"); # Splitting up the names into the components
# Transposing the list rn
lis <- rn;
m <- do.call(rbind, lis);
split(m, col(m));
rn2 <- m;
gt50tests <- data.frame(rn2); # Creating a data frame for ease of use
# Combining the number of tests with the patient and lab information
gt50tests <- cbind(gt50tests, dfNotRare);
colnames(gt50tests) <- c("ENC_CSN_ID", "CPT_CODE", "COMPONENT_ID", "TIMES_TEST_REPEATED");
gt50tests$CPT_ID_CODE = paste(gt50tests$CPT_CODE, gt50tests$COMPONENT_ID, sep = "_");
# Finding which tests patients (determined by ENC_CSN_ID) had more than 50 times
whTestsgt50 <- table(gt50tests$CPT_ID_CODE);
whTestsgt50 <- whTestsgt50[order(whTestsgt50)];
View(whTestsgt50);

# ## Notes to self on next steps at end of day 6/26/2015:
# Figure out top ten tests which a patient had more than 50 times that return a real value (not
# a binary). Plot the results of the top test (or 5) for each patient in time (adjust so that
# either admission time or threshold is aligned in timeframe). Save the plot.
# Possible problems so far - what do I do with $PAT_ID and $ENC_CSN_ID - they do not have a
# one-to-one correspondence.

topTestgt50 <- whTestsgt50[whTestsgt50>=10];
View(topTestgt50);
head(topTestgt50, n = 10);
# 80048_1514    80048_1520 24700500_5585 82947.19_1661
# 10            11            15            24
#
# 52000611_5585     CBCD_1511   82962.05_3442
# 34                     110           177
head(whTestsgt50[whTestsgt50>=8], n = 25);
# 24701204_5585 52000335_5585    80048_1518    80048_1519
# 8             8             9             9
#
# 80048_1521    80048_1522    80048_1523    80048_1554
# 9             9             9             9
#
# 80048_3216    80048_5339    80048_1514    80048_1520
# 9             9            10            11
#
# 24700500_5585 82947.19_1661 52000611_5585     CBCD_1511
# 15            24            34           110
#
# 82962.05_3442
# 177
##
## Choosing 80048_1518 as first test to evaluate
tID <- c("CPT_CODE"="80048", "COMPONENT_ID"= as.numeric(1519));
# ENC_CSN_ID from
firstENC <- gt50tests[gt50tests$COMPONENT_ID==as.numeric(tID[2]) & gt50tests$CPT_CODE==tID[1],];
#
#                         ENC_CSN_ID CPT_CODE COMPONENT_ID TIMES_TEST_REPEATED CPT_ID_CODE
# 73475137_80048_1519   73475137    80048         1519                  51  80048_1519
# 75612334_80048_1519   75612334    80048         1519                  52  80048_1519
# 72864593_80048_1519   72864593    80048         1519                  54  80048_1519
# 71690301_80048_1519   71690301    80048         1519                  59  80048_1519
# 74997079_80048_1519   74997079    80048         1519                  60  80048_1519
# 76288700_80048_1519   76288700    80048         1519                  62  80048_1519
# 71829000_80048_1519   71829000    80048         1519                  76  80048_1519
# 76686116_80048_1519   76686116    80048         1519                  77  80048_1519
# 74048411_80048_1519   74048411    80048         1519                  91  80048_1519
##
v <- firstENC$ENC_CSN_ID; # Vector of ENC_CSN_ID want to plot
## Tables for each ENC_CSN_ID full lab data from list v
###### SHOULD FIND A WAY TO AUTOMATE THIS ######
labID <- tID;
# To see all the entries
View(goodlab[goodlab$CPT_CODE==labID[1] & goodlab$COMPONENT_ID==as.numeric(labID[2]),]);
# Function to get specific ENC_CSN_ID lab results for specific lab
getPat <- function(vec, labID = labID) {
    pat <- goodlab[goodlab$CPT_CODE==labID[1] & goodlab$COMPONENT_ID==as.numeric(labID[2]) &
                       goodlab$ENC_CSN_ID==vec,];
    return(pat);
}
pat1 <- getPat(v[1], labID); ###### Will making this a list help to automate it for the length of v?
pat2 <- getPat(v[2], labID);
pat3 <- getPat(v[3], labID);
pat4 <- getPat(v[4], labID);
pat5 <- getPat(v[5], labID);
pat6 <- getPat(v[6], labID);
pat7 <- getPat(v[7], labID);
pat8 <- getPat(v[8], labID);
pat9 <- getPat(v[9], labID);

## Working with largest table for pat9
# str(pat9)
# 'data.frame':	91 obs. of  17 variables
# trTimepat9 <- as.Date(as.character(pat9$RESULT_DATE[]), format ="%m/%d/%Y %H:%M" ); # Time
# pat9BUN <- as.numeric(pat9$ORD_NUM_VALUE[]); # BUN Value
# plot(trTimepat9, pat9BUN, panel.first = grid());
trTimeBUN1 <- as.Date(as.character(pat1$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN2 <- as.Date(as.character(pat2$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN3 <- as.Date(as.character(pat3$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN4 <- as.Date(as.character(pat4$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN5 <- as.Date(as.character(pat5$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN6 <- as.Date(as.character(pat6$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN7 <- as.Date(as.character(pat7$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN8 <- as.Date(as.character(pat8$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time
trTimeBUN9 <- as.Date(as.character(pat9$RESULT_DATE[]), format = "%m/%d/%Y %H:%M"); # Time

pat9BUN <- as.numeric(as.character(pat9$ORD_NUM_VALUE)); # BUN Value
pat8BUN <- as.numeric(as.character(pat8$ORD_NUM_VALUE)); # BUN Value
pat7BUN <- as.numeric(as.character(pat7$ORD_NUM_VALUE)); # BUN Value
pat6BUN <- as.numeric(as.character(pat6$ORD_NUM_VALUE)); # BUN Value
pat5BUN <- as.numeric(as.character(pat5$ORD_NUM_VALUE)); # BUN Value
pat4BUN <- as.numeric(as.character(pat4$ORD_NUM_VALUE)); # BUN Value
pat3BUN <- as.numeric(as.character(pat3$ORD_NUM_VALUE)); # BUN Value
pat2BUN <- as.numeric(as.character(pat2$ORD_NUM_VALUE)); # BUN Value
pat1BUN <- as.numeric(as.character(pat1$ORD_NUM_VALUE)); # BUN Value
# Creating tables for each ENC_CSN_ID
pat1TBUN <- data.frame(trTimeBUN1, pat1BUN);
pat2TBUN <- data.frame(trTimeBUN2, pat2BUN);
pat3TBUN <- data.frame(trTimeBUN3, pat3BUN);
pat4TBUN <- data.frame(trTimeBUN4, pat4BUN);
pat5TBUN <- data.frame(trTimeBUN5, pat5BUN);
pat6TBUN <- data.frame(trTimeBUN6, pat6BUN);
pat7TBUN <- data.frame(trTimeBUN7, pat7BUN);
pat8TBUN <- data.frame(trTimeBUN8, pat8BUN);
pat9TBUN <- data.frame(trTimeBUN9, pat9BUN);
# Removing invalid results of 99999
pat1TBUN <- pat1TBUN[pat1TBUN$pat1BUN<500,];
pat2TBUN <- pat2TBUN[pat2TBUN$pat2BUN<500,];
pat3TBUN <- pat3TBUN[pat3TBUN$pat3BUN<500,];
pat4TBUN <- pat4TBUN[pat4TBUN$pat4BUN<500,];
pat5TBUN <- pat5TBUN[pat5TBUN$pat5BUN<500,];
pat6TBUN <- pat6TBUN[pat6TBUN$pat6BUN<500,];
pat7TBUN <- pat7TBUN[pat7TBUN$pat7BUN<500,];
pat8TBUN <- pat8TBUN[pat8TBUN$pat8BUN<500,];
pat9TBUN <- pat9TBUN[pat9TBUN$pat9BUN<500,];
# Ordering chronologically
pat1TBUN <- pat1TBUN[order(pat1TBUN$trTimeBUN1),];
pat2TBUN <- pat2TBUN[order(pat2TBUN$trTimeBUN2),];
pat3TBUN <- pat3TBUN[order(pat3TBUN$trTimeBUN3),];
pat4TBUN <- pat4TBUN[order(pat4TBUN$trTimeBUN4),];
pat5TBUN <- pat5TBUN[order(pat5TBUN$trTimeBUN5),];
pat6TBUN <- pat6TBUN[order(pat6TBUN$trTimeBUN6),];
pat7TBUN <- pat7TBUN[order(pat7TBUN$trTimeBUN7),];
pat8TBUN <- pat8TBUN[order(pat8TBUN$trTimeBUN8),];
pat9TBUN <- pat9TBUN[order(pat9TBUN$trTimeBUN9),];

## Lunch time 6/29/2015 notes for after lunch: Now automate or do for all 9 patient ENC_CSN_IDS
# Automation is failing. Just doing.
png("BUN_Time_for_ENC_CSN_ID_20150630.png", width = 600, height = 500, units = "px");
plot(pat9TBUN$trTimeBUN9, pat9TBUN$pat9BUN, col = "gray", panel.first = grid(),
     xlim = range(as.Date("2008-02-01"),as.Date("2008-11-30")), ylim = range(100, 170),
     xlab = "Time (days)", ylab = "Blood Urea Nitrogen level (mmol/L)");
points(pat8TBUN$trTimeBUN8, pat8TBUN$pat8BUN, col = "black");
points(pat7TBUN$trTimeBUN7, pat7TBUN$pat7BUN, col = "purple");
points(pat6TBUN$trTimeBUN6, pat6TBUN$pat6BUN, col = "blue");
points(pat5TBUN$trTimeBUN5, pat5TBUN$pat5BUN, col = "green");
points(pat4TBUN$trTimeBUN4, pat4TBUN$pat4BUN, col = "dark green");
points(pat3TBUN$trTimeBUN3, pat3TBUN$pat3BUN, col = "orange");
points(pat2TBUN$trTimeBUN2, pat2TBUN$pat2BUN, col = "red");
points(pat1TBUN$trTimeBUN1, pat1TBUN$pat1BUN, col = "pink");
abline(h = 135, col = "red"); # Reference low
abline(h = 146, col = "red"); # Reference high
lines(pat9TBUN$trTimeBUN9, pat9TBUN$pat9BUN, col = "gray");
lines(pat8TBUN$trTimeBUN8, pat8TBUN$pat8BUN, col = "black");
lines(pat7TBUN$trTimeBUN7, pat7TBUN$pat7BUN, col = "purple");
lines(pat6TBUN$trTimeBUN6, pat6TBUN$pat6BUN, col = "blue");
lines(pat5TBUN$trTimeBUN5, pat5TBUN$pat5BUN, col = "green");
lines(pat4TBUN$trTimeBUN4, pat4TBUN$pat4BUN, col = "dark green");
lines(pat3TBUN$trTimeBUN3, pat3TBUN$pat3BUN, col = "orange");
lines(pat2TBUN$trTimeBUN2, pat2TBUN$pat2BUN, col = "red");
lines(pat1TBUN$trTimeBUN1, pat1TBUN$pat1BUN, col = "pink");
dev.off();
svg("BUN_Time_for_ENC_CSN_ID_20150630.svg", width = 7, height = 5);
graphics.off();
