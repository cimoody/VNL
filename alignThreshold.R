## alignThreshold.R
## Written by Cristina Moody
## July 2015


# FUNCTION TO CHANGE FACTOR TO NUMERIC
f2n <- function(val) {val <- as.numeric(as.character(val));}

# Subset into a list that only contains tests that start in a normal range.
normalStart <- function(ListOfDataFrames) {
    x <- list();
    for(i in 1:length(ListOfDataFrames)) {
        if ( (ListOfDataFrames[[i]]$ORD_NUM_VALUE[1] <= f2n(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])) &
             (ListOfDataFrames[[i]]$ORD_NUM_VALUE[1] >= f2n(ListOfDataFrames[[i]]$REFERENCE_LOW[1])) )
        {x[i] <- ListOfDataFrames[i];} else {x[i]=NA}
    }
    x <- x[!is.na(x[])];
    return(x)
}

# Function to get choosen t0 (when test first crosses reference_high)
# from each dataframe in list
t0Finder <- function(ListOfDataFrames){
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
