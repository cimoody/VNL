## alignThreshold.R
## Written by Cristina Moody
## July 2015

# Installing Libraries
# source(file = "librariesVNL.R"); # if starting R for 1st time today

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

dDir <- "E:/VNL\ Data\ from\ Joe/";

# FUNCTION TO CHANGE FACTOR TO NUMERIC
f2n <- function(val) {
    # FUNCTION TO CHANGE FACTOR TO NUMERIC
    val <- as.numeric(as.character(val));
}

# Subset into a list that only contains tests that start in a normal range.
normalStart <- function(ListOfDataFrames) {
    # Subset into a list that only contains tests that start in a normal range.
    x <- list();
    for(i in 1:length(ListOfDataFrames)) {
        if ( (ListOfDataFrames[[i]]$ORD_NUM_VALUE[1] <= f2n(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])) &
             (ListOfDataFrames[[i]]$ORD_NUM_VALUE[1] >= f2n(ListOfDataFrames[[i]]$REFERENCE_LOW[1])) )
        {x[i] <- ListOfDataFrames[i];} else {x[i] <- NA}
    }
    x <- x[!is.na(x[])];
    return(x)
}

# Function to get choosen t0 (when test first crosses reference_high)
# from each dataframe in list
t0Finder <- function(ListOfDataFrames){
    # Function to get choosen t0 (when test first crosses reference_high) from each dataframe in list
    t0list <- data.frame();
    for (i in 1:length(ListOfDataFrames)){
        if (is.null(nrow(ListOfDataFrames[[i]]))){print(i); break();}
        for (j in 1:nrow(ListOfDataFrames[[i]])){
            if ( ((ListOfDataFrames[[i]]$ORD_NUM_VALUE[j] >
                   f2n(ListOfDataFrames[[i]]$REFERENCE_HIGH[j])))){
                t0list <- rbind(t0list,
                                data.frame(
                                    "PATID_ORDERDATE" = sprintf("%s_%s", ListOfDataFrames[[i]]$PAT_ID[j],
                                                                ListOfDataFrames[[i]]$ORDERING_DATE[j]),
                                    "PAT_ID" = ListOfDataFrames[[i]]$PAT_ID[j],
                                    "t0" = ListOfDataFrames[[i]]$ORDERING_DATE[j],
                                    "TEST_NUM" = j,
                                    "TOTAL_TESTS" = nrow(ListOfDataFrames[[i]]) ));
                break();
            }
        }
    }
    return((t0list));
}

# FUNCTION TO ADD PROPERTIME TO EACH DATAFRAME IN LIST
addProperTime <- function(ListOfDataFrames, t0DataFrame){
    # ADD PROPERTIME TO EACH DATAFRAME IN LIST
    for (i in 1:nrow(t0DataFrame)) {
        for (j in 1:length(ListOfDataFrames)){
            if ( ListOfDataFrames[[j]]$PAT_ID[1]==t0DataFrame$PAT_ID[i] ){
                ListOfDataFrames[[j]]$PROPER_TIME <-
                    ListOfDataFrames[[j]]$ORDERING_DATE -
                    t0DataFrame$t0[i];
            }
        }
    }
    return(ListOfDataFrames);
}

# Function to return a dataframe with the proper time
returnProperTime <- function(originalListOfDataFrames) {
    # Return a dataframe with the proper time
    ListOfDataFrames <- normalStart(originalListOfDataFrames);
    if (length(ListOfDataFrames)==0) {return(0);}
    properTime <- t0Finder(ListOfDataFrames);
    adjustedList <- addProperTime(ListOfDataFrames, properTime);
    for (j in 1:length(adjustedList)) {
        if (is.null(adjustedList[[j]]$PROPER_TIME)) {
            adjustedList[[j]]$PROPER_TIME <- adjustedList[[j]]$ORDERING_DATE -
                adjustedList[[j]]$ORDERING_DATE[which.min(adjustedList[[j]]$ORDERING_DATE)];
                # adjustedList[[j]]$ORDERING_DATE[1]; # ALigns not interesting patients to start at 0
                # Changing so that pateints that do not hit the threshold have data that ends before 0
            adjustedList[[j]]$INT_FLAG <- 0; # Adding INT_FLAG = 0 (not interesting)
        }
        else {
            adjustedList[[j]]$INT_FLAG <- 1;
        }
    }
    return(adjustedList);
}

# I have multiple test results ordered on the same day.
# Need to create a function to find all the multiple tests per day,
# get the mean, and standard deviation of the results.
# Not sure how to do this yet. Return to it on Monday - ask David for advice.

# Function to plot everything with new proper time
makePlot2 <- function(ListOfDataFrames, shade) {
    # Plot everything with new proper time
    svg(sprintf("Lab_by_day_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5);
    ymax <- c(); ymin <- c();
    xmax <- c(); xmin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) ));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) ));
        xmax[i] <- max(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
        xmin[i] <- min(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
    }
    co <- rainbow(length(ListOfDataFrames));
    if (length(ListOfDataFrames[[i]]$PROPER_TIME)==length(ListOfDataFrames[[i]]$ORD_NUM_VALUE)){
        plot(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
             bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
             type = "o", panel.first = grid(),
             xlim = range(min(xmin), max(xmax)),
             ylim = range(min(ymin), max(ymax)),
             xlab = "Time (days)",
             ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                            ListOfDataFrames[[1]]$COMPONENT_ID[1],
                            ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
        # axis.Date(side = 1, x = as.Date(ListOfDataFrames[[1]]$PROPER_TIME));
        for (i in 1:length(ListOfDataFrames)){
            lines(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
                  bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
                  type = "b", panel.first = grid(),
                  xlim = range(min(xmin), max(xmax)),
                  ylim = range(min(ymin), max(ymax)),
                  xlab = "Time (days)",
                  ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                                 ListOfDataFrames[[1]]$COMPONENT_ID[1],
                                 ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
            # Reference low
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
            # Reference high
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
            # axis.Date(side = 1, x = as.Date(ListOfDataFrames[[i]]$PROPER_TIME));}
        }
    }
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return( svg(sprintf("Lab_by_day_%s_%s_gt_%g.svg",
                       ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                       ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5) );
}

# Function to save each table to a csv file
makeCSV <- function(ListOfDataFrames) {
    # Function to save each table to a csv file
    for (j in 1:length(ListOfDataFrames)) {
        write.csv(ListOfDataFrames[[j]],
                  file = sprintf("E:/data_for_Oleg/Test__%s__patient_%s.csv",
                                 deparse(substitute(ListOfDataFrames)),
                                 ListOfDataFrames[[j]]$PAT_ID[1]),
                  row.names = FALSE, na="");
    }
}

# Function to get the standard deviation, but return 0 if vector is of length 1.
getSD <- function(val) {
    # Function to get the standard deviation, but return 0 if vector is of length 1.
    if (is.na(sd(val))) {
        return(0);
    } else {
        return(sd(val));
    }
}

# Function to go through list of dataframes and
# replace repeated labs with mean and add standard deviation column
getMeanSDListDataFrames <- function(ListOfDataFrames) {
    # Function to go through list of dataframes and replace repeated labs with mean and add standard deviation column
    # Create new list so not to overwrite original
    newListOfDataFrames <- ListOfDataFrames;
    for (j in  1:length(ListOfDataFrames)){
        # Function to get the standard deviations of labs repeated on a given day
        sds <- aggregate(ORD_NUM_VALUE ~ ORDERING_DATE, data = ListOfDataFrames[[j]], FUN = getSD);
        # Function to get the mean value of labs repeated on a given day
        means <- aggregate(ORD_NUM_VALUE ~ ORDERING_DATE, data = ListOfDataFrames[[j]], FUN = mean);
        # Function that merges the days and keeps the maximum ORD_NUM_VALUE - from the internet.
        newListOfDataFrames[[j]] <- do.call(rbind,
                                            lapply(split(newListOfDataFrames[[j]],
                                                         newListOfDataFrames[[j]]$ORDERING_DATE),
                                                   function(chunk) chunk[which.max(chunk$ORD_NUM_VALUE), ]));
        # Changing the maximum ORD_NUM_VALUE to the mean
        newListOfDataFrames[[j]]$ORD_NUM_VALUE <- means$ORD_NUM_VALUE;
        # Adding the standard deviation
        newListOfDataFrames[[j]]$SD_ORD_VAL <- sds$ORD_NUM_VALUE;
    }
    return(newListOfDataFrames);
}

# # Function to get the standard deviations of labs repeated on a given day
# sds <- aggregate(ORD_NUM_VALUE ~ ORDERING_DATE, data = test[[2]], FUN = getSD);
# # Function to get the mean value of labs repeated on a given day
# means <- aggregate(ORD_NUM_VALUE ~ ORDERING_DATE, data = test[[2]], FUN = mean);
# # Function that merges the days and keeps the maximum ORD_NUM_VALUE - from the internet - it is awesome.
# test[[2]] <- do.call(rbind,lapply(split(test[[2]],test[[2]]$ORDERING_DATE),
#                                   function(chunk) chunk[which.max(chunk$ORD_NUM_VALUE),]));
# # Changing the maximum ORD_NUM_VALUE to the mean
# test[[2]]$ORD_NUM_VALUE <- means$ORD_NUM_VALUE;
# # Adding the standard deviation
# test[[2]]$SD_ORD_VAL <- sds$ORD_NUM_VALUE;

# Function to plot everything with
# new mean for repeated labs and error bar of standard deviation
makePlot3 <- function(ListOfDataFrames, shade) {
    # Plot everything with new mean for repeated labs and error bar of standard deviation
    svg(sprintf("Mean_lab_by_day_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5);
    ymax <- c(); ymin <- c();
    xmax <- c(); xmin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]
                             + ListOfDataFrames[[i]][which.max(ListOfDataFrames[[i]][,2]),12], # Adds the error bar
                             as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) )));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]
                             - ListOfDataFrames[[i]][which.min(ListOfDataFrames[[i]][,2]),12], # Subtracts the error bar
                             as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) )));
        xmax[i] <- max(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
        xmin[i] <- min(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
    }
    co <- rainbow(length(ListOfDataFrames));
    if (length(ListOfDataFrames[[i]]$PROPER_TIME)==length(ListOfDataFrames[[i]]$ORD_NUM_VALUE)){
        # Plot
        plot(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
             bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
             type = "o", panel.first = grid(),
             xlim = range(min(xmin), max(xmax)),
             ylim = range(min(ymin), max(ymax)),
             xlab = "Time (days)",
             ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                            ListOfDataFrames[[1]]$COMPONENT_ID[1],
                            ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Add Errorbars with SD_ORD_VAL
        with(data = ListOfDataFrames[[i]],
             expr = errbar(PROPER_TIME, ORD_NUM_VALUE, ORD_NUM_VALUE + SD_ORD_VAL,
                           ORD_NUM_VALUE - SD_ORD_VAL, pch = "", add = T,
                           errbar.col = alpha(co[i], shade+0.2), cap = 0.02));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
        # axis.Date(side = 1, x = as.Date(ListOfDataFrames[[1]]$PROPER_TIME));
        for (i in 1:length(ListOfDataFrames)){
            lines(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
                  bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
                  type = "b", panel.first = grid(),
                  xlim = range(min(xmin), max(xmax)),
                  ylim = range(min(ymin), max(ymax)),
                  xlab = "Time (days)",
                  ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                                 ListOfDataFrames[[1]]$COMPONENT_ID[1],
                                 ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
            # Add Errorbars with SD_ORD_VAL
            with(data = ListOfDataFrames[[i]],
                 expr = errbar(PROPER_TIME, ORD_NUM_VALUE, ORD_NUM_VALUE + SD_ORD_VAL,
                               ORD_NUM_VALUE - SD_ORD_VAL, pch = "", add = T,
                               errbar.col = alpha(co[i], shade+0.2), cap = 0.02));
            # Reference low
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
            # Reference high
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
            # axis.Date(side = 1, x = as.Date(ListOfDataFrames[[i]]$PROPER_TIME));}
        }
    }
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return( svg(sprintf("Mean_lab_by_day_%s_%s_gt_%g.svg",
                        ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                        ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5) );
}
