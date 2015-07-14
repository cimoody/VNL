## getResultDate.R
## Written by Cristina Moody
## July 2015

# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);
# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("plyr");
library(plyr);
install.packages("Hmisc", dependencies = T);
library(Hmisc);

# Preprosessing files - must be done manually for each data set
# Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- getwd();
# Data directory: E:/VNL Data from Joe
dDir <- "E:/VNL\ Data\ from\ Joe/";
# Data filenames
data.popname = sprintf("%s%s", dDir, "di_populations_v1.1.csv");
data.labname = sprintf("%s%s", dDir, "di_lab_data_v1.1.csv");
data.pop <- read.csv(file = data.popname, header = TRUE, stringsAsFactors = FALSE);
data.lab <- read.csv(file = data.labname, header = TRUE, stringsAsFactors = FALSE);
# Saving the data in R for manipulation
save(data.pop, file = sprintf("%s%s", dDir, "dataPop.rda"));
save(data.lab, file = sprintf("%s%s", dDir, "dataLab.rda"));
# Checking columns for NA
if( sum( is.na(data.lab$X) + 0 ) >= length( data.lab$X ) ) {
    print(" X is all NA\n");
} # from D. Sanchez
if( sum( is.na(data.pop$X) + 0 ) >= length( data.pop$X ) ) {
    print(" X is all NA\n");
} # from D. Sanchez
if( sum( is.na(data.pop$X.1) + 0 ) >= length( data.pop$X.1 ) ) {
    print(" X is all NA\n");
} # from D. Sanchez
drops <- c("X");
goodlab <- data.lab[,!(names(data.lab) %in% drops)];
drops <- c("X", "X.1");
goodpop <- data.pop[,!(names(data.pop) %in% drops)];
# Save modified files for R manipulation
save(goodpop, goodlab, file = sprintf("%s%s", dDir, "goodData.rda"));
list.files(path = dDir, pattern = "\\.rda");
load(sprintf("%s%s", dDir, "goodData.rda"));

# Choosing first test to evaluate
tID <- c("CPT_CODE"="80048", "COMPONENT_ID"= as.numeric(1518));
# Choosing cutoff
cutoff <- 50;

## FUNCTION THAT RETURNS LAB RESULT AND ORDERING DATA FOR ANY GIVEN LAB
getResultDate <- function(cutoff, tID){
    # Make a variable that is a combination of ENC_CSN_ID, CPT_CODE, COMPONENT_ID
    # CPT_CODE & COMPONENT_ID are the lab
    labnames <- paste(goodlab$ENC_CSN_ID, goodlab$CPT_CODE, goodlab$COMPONENT_ID, sep = "_");
    # Use table to get the frequency of each combined patient & lab
    topLabs <- table(labnames);
    # Order these by frequency
    df <- topLabs;
    df <- df[order(df)];
    # cutoff <- 50;
    dfNotRare <- df[df>=cutoff];
    # Determining which labs pass cutoff <= 50;
    # Accessing the row names in the table (since they are ID_lab)
    rn <- row.names(dfNotRare);
    rn <- strsplit(rn, "_"); # Splitting up the names into the components
    # Transposing the list rn
    lis <- rn;
    m <- do.call(rbind, lis);
    split(m, col(m));
    rn2 <- m;
    gtCOtests <- data.frame(rn2); # Creating a data frame for ease of use
    # Combining the number of tests with the patient and lab information
    gtCOtests <- cbind(gtCOtests, dfNotRare);
    colnames(gtCOtests) <- c("ENC_CSN_ID", "CPT_CODE",
                             "COMPONENT_ID", "TIMES_TEST_REPEATED");
    gtCOtests$CPT_ID_CODE = paste(gtCOtests$CPT_CODE, gtCOtests$COMPONENT_ID, sep = "_");
    # Finding which tests patients (determined by ENC_CSN_ID) had more than cutoff # of times
    whTestsgtCO <- table(gtCOtests$CPT_ID_CODE);
    whTestsgtCO <- whTestsgtCO[order(whTestsgtCO)];
    # ENC_CSN_ID from
    firstENC <- gtCOtests[gtCOtests$COMPONENT_ID==as.numeric(tID[2]) &
                              gtCOtests$CPT_CODE==tID[1],];
    # ENC_CSN_ID of pateintw with specific test
    v <- firstENC$ENC_CSN_ID; # Vector of ENC_CSN_ID want to plot
    labID <- tID;
    # Function to get specific ENC_CSN_ID lab results for specific lab
    getPat <- function(vec, labID. = labID) {
        pat <- goodlab[(goodlab$CPT_CODE==labID.[1] &
                            goodlab$COMPONENT_ID==as.numeric(labID.[2]) &
                            goodlab$ENC_CSN_ID==vec),];
        return(pat);
    }
    # Getting lab results for specific lab
    pat <- list(lapply(v, FUN = getPat));
    # Isolatinge the ORDERING_DATE
    trTimeLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        trTimeLAB[[i]] <- as.Date(pat[[1]][i][[1]]$ORDERING_DATE,
                                  format = "%m/%d/%Y %H:%M");
    }
    # Isolating lab ORD_NUM_VALUE
    patLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        patLAB[[i]] <- as.numeric(as.character(pat[[1]][i][[1]]$ORD_NUM_VALUE));
    }
    # Creating tables for each ENC_CSN_ID of ORDERING_DATE & ORD_NUM_VALUE in a list
    patTLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        patTLAB[[i]] <- data.frame(Test_Order_Date = trTimeLAB[i], LAB = patLAB[i],
                                   pat[[1]][i][[1]]$REFERENCE_UNIT,
                                   pat[[1]][i][[1]]$PAT_ID,
                                   pat[[1]][i][[1]]$REFERENCE_HIGH,
                                   pat[[1]][i][[1]]$REFERENCE_LOW,
                                   pat[[1]][i][[1]]$CPT_CODE,
                                   pat[[1]][i][[1]]$COMPONENT_ID,
                                   MIN_RAW_LABS = cutoff);
        names(patTLAB[[i]]) <- c("ORDERING_DATE", "ORD_NUM_VALUE", "REFERENCE_UNIT",
                                 "PAT_ID", "REFERENCE_HIGH", "REFERENCE_LOW",
                                 "CPT_CODE", "COMPONENT_ID", "MIN_RAW_LABS");
    }
    # Removing invalid results of 99999
    for (i in 1:length(v)){
        patTLAB[[i]] <- patTLAB[[i]][patTLAB[[i]]$ORD_NUM_VALUE < 77777, ];
    }
    # Ordering chronologically
    for (i in 1:length(v)){
        patTLAB[[i]] <- patTLAB[[i]][order(patTLAB[[i]]$ORDERING_DATE), ];
    }
    return(patTLAB);
}

## FUNCTION THAT PLOTS ALL ECN_CSN_ID FOR ONE LAB FROM LIST RETURNED FROM getResultDate
makePlot <- function(ListOfDataFrames) {
    svg(sprintf("Lab_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5));
    ymax <- c(); ymin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) ));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) ));
    }
    co <- rainbow(length(ListOfDataFrames));
    plot(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
         col = co[i], type = "o", panel.first = grid(),
         xlim = range(as.Date("2008-02-01"),as.Date("2008-11-30")),
         ylim = range(min(ymin), max(ymax)),
         xlab = "Time (days)",
         ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                        ListOfDataFrames[[1]]$COMPONENT_ID[1],
                        ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
    # Reference low
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
    # Reference high
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
    axis.Date(side = 1, x = as.Date(ListOfDataFrames[[1]]$ORDERING_DATE));
    for (i in 1:length(ListOfDataFrames)){
        lines(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
              col = co[i], type = "b", panel.first = grid(),
              xlim = range(as.Date("2008-02-01"),as.Date("2008-11-30")),
              ylim = range(min(ymin), max(ymax)),
              xlab = "Time (days)",
              ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                             ListOfDataFrames[[1]]$COMPONENT_ID[1],
                             ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
        axis.Date(side = 1, x = as.Date(ListOfDataFrames[[i]]$ORDERING_DATE));}
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return(svg(sprintf("Lab_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5)));
}
