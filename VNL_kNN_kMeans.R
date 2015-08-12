# And now for something completely different
#                   . . . well, not really completely different!
# Cristina Moody Aug 2015
# Note: you need all the functions from:
#   getResultDate.R
#   alignThreshold.R
#   VNL_Classifier.R
#   VNL_LinearRegression.R

# Installing Libraries
# source(file = "librariesVNL.R");# if starting R for 1st time today
# Libraries
require(rJava);
require(RWeka);
require(lmtest);
require(sandwich);
require(car);
require(zoo);
require(kknn);
require(lubridate);

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

# Merging population and lab data
mergedDF <-  merge.data.frame(goodDataOrdered10DaysBeforeThreshold, goodpop, stringsAsFactors = FALSE);
# Sorting into testing and training sets
n.points <- nrow(meta);
sampling.rate <- 0.8;
num.test.set.labels <- n.points * (1 - sampling.rate);
training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE);
vnl.train <- subset(meta[training, ]);#, select = c(-INT_FLAG));
testing <- setdiff(1:n.points, training);
vnl.test <- subset(meta[testing, ]);#, select = c(-INT_FLAG));

# Not working because of NA in meta
# Replacing NAs in meta
# Columns with NA are found by
naCol <- as.data.frame(apply(meta,2,function(x) any(is.na(x)) ));
# Bad columns are
badcol <- meta[c("PRINC_PROC", "REFRG_MD", "ETHNIC_GROUP_C", "PCP_PROV_ID")];
# Replacing NAs with
replaceNA <- function(x) {
    # replaces NA in a column with mean + noise to avoid changing statistics of column -abs to aviod neg.
    abs(mean(x, na.rm = T) + rnorm(sum(is.na(x)==T))*sd(x, na.rm = T));
}
badcol[is.na(badcol$PRINC_PROC),]$PRINC_PROC <- replaceNA(badcol$PRINC_PROC);
badcol[is.na(badcol$REFRG_MD),]$REFRG_MD <- replaceNA(badcol$REFRG_MD);
badcol[is.na(badcol$ETHNIC_GROUP_C),]$ETHNIC_GROUP_C <- replaceNA(badcol$ETHNIC_GROUP_C);
badcol[is.na(badcol$PCP_PROV_ID),]$PCP_PROV_ID <- replaceNA(badcol$PCP_PROV_ID);
# Putting this columns back in meta as meta2
meta2 <- meta;
meta2[names(badcol)] <- badcol[names(badcol)];
# Resettin vnl.train and vnl.test with meta2
vnl.train <- subset(meta2[training, ]);#, select = c(-INT_FLAG));
vnl.test <- subset(meta2[testing, ]);#, select = c(-INT_FLAG));

vnl.knn <- train.kknn(formula = formula(`THRESHOLD_TIME` ~ .),
                      data = vnl.train, kmax = 50, distance = 1);
vnl.knn;
# Call:
#     train.kknn(formula = formula(THRESHOLD_TIME ~ .), data = vnl.train,     kmax = 50, distance = 1)
#
# Type of response variable: continuous
# minimal mean absolute error: 104.2534
# Minimal mean squared error: 22701.78
# Best kernel: optimal
# Best k: 28
plot(vnl.train$THRESHOLD_TIME, vnl.knn$fitted.values[[28]][1:356], pch = vnl.train$INT_FLAG+22);
# And it's terrible!

# Try again!
vnl.knn2 <- kknn(formula = formula(`THRESHOLD_TIME` ~ .),
                train = vnl.train, test = vnl.test, k = 28, distance = 1);
# Extracting the prediction
fit <- ceil(fitted(vnl.knn2));
table(vnl.test$THRESHOLD_TIME, fit);
plot(vnl.test$THRESHOLD_TIME, fit, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5), xlim = range(0:20), ylim = range(0:20));
plot(vnl.test$THRESHOLD_TIME, fit, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
# Also terrible.

# 3rd try
vnl.knn3 <- kknn(formula = formula(`THRESHOLD_TIME` ~ `COMPONENT_ID` + #`INT_FLAG` +
                                      `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
                                      `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
                                      `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
                                      `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
                                      `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
                                      `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
                                      `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
                                      `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`),
                train = vnl.train, test = vnl.test, k = 28, distance = 1);
# Extracting the prediction
fit3 <- fitted(vnl.knn3);
table(vnl.test$THRESHOLD_TIME, fit3);
plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5), xlim = range(0:20), ylim = range(0:20));
plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
# Much better!
# Comparing to earlier glm plot
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());

plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
       col = alpha("blue", 1), bg = alpha("blue", .5))
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());


meta <- read.arff("mergedDFg.arff");
# Simple example choosing k
classifier <- IBk(THRESHOLD_TIME ~ `COMPONENT_ID` + `INT_FLAG` +
                      `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
                      `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
                      `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
                      `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
                      `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
                      `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
                      `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
                      `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`, data = meta, na.action = na.omit );
summary(classifier);

