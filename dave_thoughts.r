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
data.popname <- paste(dDir, "di_populations_v1.1.csv", sep ="");
data.labname <- paste(dDir, "di_lab_data_v1.1.csv", sep ="");

#### Suggestion #####
# Complete stylistic suggestion; I like to use sprintf in R instead of paste
# because it gives you more control over the format of the result string.
# It also makes it very easy to properly format numerics (e.g., leading zeros), etc
data.popname = sprintf("%s%s", dDir, "di_populations_v1.1.csv");
data.labname = sprintf("%s%s", dDir, "di_lab_data_v1.1.csv");

### Don't need to repeat these steps every time
# ## Reading the data into memory
# data.pop <- read.csv(file = data.popname, header = TRUE);
# data.lab <- read.csv(file = data.labname, header = TRUE);
# ## Saving the data in R for manipulation
# save(data.pop, file = paste(dDir, "dataPop.rda", sep=""));
# save(data.lab, file = paste(dDir,"dataLab.rda", sep=""));
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
# drops <- c("X");
# goodlab <- data.lab[,!(names(data.lab) %in% drops)];
# # Checking that goodlab is the same as data.lab without the "X" column
# str(goodlab);
# str(data.pop); # Returns the structure of data.lab
# # Dropping column "X", "X.1" - appear to be all NA - and saving to a new data.frame named goodpop
# drops <- c("X", "X.1");
# goodpop <- data.pop[,!(names(data.pop) %in% drops)];
# # Checking that goodlab is the same as data.lab without the "X" column
# str(goodpop);
#
# ## Saving the good data in R for manipulation
# save(goodpop, goodlab, file = "E:/VNL\ Data\ from\ Joe/goodData.rda");
# # Verifying saved files


#### Suggestion ####
# I prefer to import data with stringsAsFactors = FALSE, because that gives you a lot
# of flexibility in combining fields (as you will probably need to do).
data.pop = read.csv(file = data.popname, header = TRUE, stringsAsFactors = FALSE);
data.lab = read.csv(file = data.labname, header = TRUE, stringsAsFactors = FALSE);

#### Suggestion ####
# Easier way to check for all-NA:
# This exploits the fact that is.na returns a vector of booleans, and the typing system in R
# coerces bools to numerics upon arithmetic operations (TRUE casts to 1, FALSE to 0).  So even
# though the + 0 part is arithmetically pointless, it ensures that the bools are cast to ints so
# that sum collects them.  If everything is NA, then the sum is going to be equal to the length.
if( sum( is.na(data.lab$X) + 0 ) >= length( data.lab$X ) ) {
  print(" X is all NA\n");
}


list.files(path = "E:/VNL\ Data\ from\ Joe/", pattern = "\\.rda");
load("E:/VNL\ Data\ from\ Joe/goodData.rda");






