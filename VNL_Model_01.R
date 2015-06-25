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

# Figure out the top labs
df$labNames = paste( df$lab_id, df$comp_id );
topLabs = sort( table( df$labNames) ); #might need to play around with arguments to sort
                        #because table() is going to return a wide vector and not a data frame
topLabs = topLabs[,1:50];
topLabs = topLabs[1,];   # should extract just the names of the labs you want
# This should subset df according to whether a row contains a top-50 element
df = df[ df$labNames %in% topLabs, ];







