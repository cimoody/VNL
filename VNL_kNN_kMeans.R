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
# mergedDF <-  merge.data.frame(goodDataOrdered10DaysBeforeThreshold, goodpop);
mergedDF <-  merge.data.frame(goodDataOrdered10DaysBeforeThreshold_alignMax, goodpop);

createMeta <- function(mDF){
    # Function takes merged dataframe of the time population data and Ordered10DaysBeforeThreshold
    # to create and clean up a new dataframe that is returned.
    mergedDFg <- mDF;
    # Fixing CPT_CODE
    mergedDFg$CPT_CODE <- factor(mergedDFg$CPT_CODE, labels = c("80048", "80053.01", "80069", "CBCD", "85027"));
#     summary(mergedDFg$CPT_CODE);
#     summary(table(mergedDFg$CPT_CODE));
    # Fixing REFERENCE_UNIT
#     str(mergedDFg);
#     summary(table(mergedDFg$REFERENCE_UNIT));
#     summary((mergedDFg$REFERENCE_UNIT));
#     class(mergedDFg$REFERENCE_UNIT);
    mergedDFg$REFERENCE_UNIT <- factor(mergedDFg$REFERENCE_UNIT, labels = c("mg/dL", "mmol/L", "K/uL"));
    # Fixing REFERENCE_LOW
#     summary(table(mergedDFg$REFERENCE_LOW));
#     summary((mergedDFg$REFERENCE_LOW));
#     class(mergedDFg$REFERENCE_LOW);
    mergedDFg$REFERENCE_LOW <- factor(mergedDFg$REFERENCE_LOW, labels = c("6", "0.7", "3.5", "135", "150", "4"));
    mergedDFg$REFERENCE_LOW <- f2n(mergedDFg$REFERENCE_LOW)
    # Fixing REFERENCE_HIGH
#     summary(table(mergedDFg$REFERENCE_HIGH));
#     summary((mergedDFg$REFERENCE_HIGH));
#     class(mergedDFg$REFERENCE_HIGH);
    mergedDFg$REFERENCE_HIGH <- f2n(mergedDF$REFERENCE_HIGH)
    # Fixing ETHNIC_GROUP_C
#     str(mergedDFg$ETHNIC_GROUP_C);
#     summary(table(mergedDFg$ETHNIC_GROUP_C));
#     summary((mergedDFg$ETHNIC_GROUP_C));
#     class(mergedDFg$ETHNIC_GROUP_C);
    mergedDFg$ETHNIC_GROUP_C <- as.numeric(mergedDF$ETHNIC_GROUP_C);
    # Fixing PCP_PROV_ID
    mergedDFg$PCP_PROV_ID <- as.numeric(mergedDF$PCP_PROV_ID);
    # Fixing CO_CD
#     str(mergedDFg$CO_CD);
#     summary(table(mergedDFg$CO_CD));
#     summary((mergedDFg$CO_CD));
#     class(mergedDFg$CO_CD);
    mergedDFg$CO_CD <- as.factor(mergedDF$CO_CD); # Can remove this column all the same
    # Fixing BILL_ADMS_DT
#     str(mergedDFg$BILL_ADMS_DT);
#     (table(mergedDFg$BILL_ADMS_DT));
#     summary((mergedDFg$BILL_ADMS_DT));
#     class(mergedDFg$BILL_ADMS_DT);
    mergedDFg$BILL_ADMS_DT <- mdy(mergedDF$BILL_ADMS_DT, tz = "EST");
    # Fixing BILL_DISCHRG_DT
#     str(mergedDFg$BILL_DISCHRG_DT);
#     (table(mergedDFg$BILL_DISCHRG_DT));
#     summary((mergedDFg$BILL_DISCHRG_DT));
#     class(mergedDFg$BILL_DISCHRG_DT);
    mergedDFg$BILL_DISCHRG_DT <- mdy(mergedDF$BILL_DISCHRG_DT, tz = "EST");
    # Fixing READMT_WITHIN_30_DAYS
#     str(mergedDFg$READMT_WITHIN_30_DAYS);
#     (table(mergedDFg$READMT_WITHIN_30_DAYS));
#     summary((mergedDFg$READMT_WITHIN_30_DAYS));
#     class(mergedDFg$READMT_WITHIN_30_DAYS);
    mergedDFg$READMT_WITHIN_30_DAYS <- as.factor(mergedDF$READMT_WITHIN_30_DAYS);
    # Fixing ADMT_DIAG
#     str(mergedDFg$ADMT_DIAG);
#     (table(mergedDFg$ADMT_DIAG));
#     summary((mergedDFg$ADMT_DIAG));
#     class(mergedDFg$ADMT_DIAG);
    mergedDFg$ADMT_DIAG <- as.factor(mergedDF$ADMT_DIAG);
    # Fixing ADMT_NURSS_STTN
#     str(mergedDFg$ADMT_NURSS_STTN);
#     (table(mergedDFg$ADMT_NURSS_STTN));
#     summary((mergedDFg$ADMT_NURSS_STTN));
#     class(mergedDFg$ADMT_NURSS_STTN);
    mergedDFg$ADMT_NURSS_STTN <- as.factor(mergedDF$ADMT_NURSS_STTN);
    # Fixing ADMT_SRC
#     str(mergedDFg$ADMT_SRC);
#     (table(mergedDFg$ADMT_SRC));
#     summary((mergedDFg$ADMT_SRC));
#     class(mergedDFg$ADMT_SRC);
    mergedDFg$ADMT_SRC <- as.factor(mergedDF$ADMT_SRC);
    # Fixing ADMT_TYP
#     str(mergedDFg$ADMT_TYP);
#     summary(table(mergedDFg$ADMT_TYP));
#     summary((mergedDFg$ADMT_TYP));
#     class(mergedDFg$ADMT_TYP);
    mergedDFg$ADMT_TYP <- as.factor(mergedDF$ADMT_TYP);
    # Fixing PRINC_PROC
#     str(mergedDFg$PRINC_PROC);
#     summary(table(mergedDFg$PRINC_PROC));
#     summary((mergedDFg$PRINC_PROC));
#     class(mergedDFg$PRINC_PROC);
    mergedDFg$PRINC_PROC <- as.numeric(mergedDF$PRINC_PROC);
    # Fixing REFRL_SRC
#     str(mergedDFg$REFRL_SRC);
#     summary(table(mergedDFg$REFRL_SRC));
#     summary((mergedDFg$REFRL_SRC));
#     class(mergedDFg$REFRL_SRC);
    mergedDFg$REFRL_SRC <- as.factor(mergedDF$REFRL_SRC);
    # Fixing REFRG_MD
#     str(mergedDFg$REFRG_MD);
#     summary(table(mergedDFg$REFRG_MD));
#     summary((mergedDFg$REFRG_MD));
#     class(mergedDFg$REFRG_MD);
    mergedDFg$REFRG_MD <- as.numeric(mergedDF$REFRG_MD);
    # Fixing APR_DRG_CD
#     str(mergedDFg$APR_DRG_CD);
#     summary(table(mergedDFg$APR_DRG_CD));
#     summary((mergedDFg$APR_DRG_CD));
#     class(mergedDFg$APR_DRG_CD);
    mergedDFg$APR_DRG_CD <- as.numeric(mergedDF$APR_DRG_CD);
    # Fixing APR_SEV_LVL
#     str(mergedDFg$APR_SEV_LVL);
#     summary(table(mergedDFg$APR_SEV_LVL));
#     summary((mergedDFg$APR_SEV_LVL));
#     class(mergedDFg$APR_SEV_LVL);
    mergedDFg$APR_SEV_LVL <- as.numeric(mergedDF$APR_SEV_LVL);
    # Fixing DISCHRG_DEPT
#     str(mergedDFg$DISCHRG_DEPT);
#     summary(table(mergedDFg$DISCHRG_DEPT));
#     summary((mergedDFg$DISCHRG_DEPT));
#     class(mergedDFg$DISCHRG_DEPT);
    mergedDFg$DISCHRG_DEPT <- as.factor(mergedDF$DISCHRG_DEPT);
    # Fixing DISCHRG_DISPOS
#     str(mergedDFg$DISCHRG_DISPOS);
#     summary(table(mergedDFg$DISCHRG_DISPOS));
#     summary((mergedDFg$DISCHRG_DISPOS));
#     class(mergedDFg$DISCHRG_DISPOS);
    mergedDFg$DISCHRG_DISPOS <- as.factor(mergedDF$DISCHRG_DISPOS);

    # Columns to remove from classification
    removCol <- c("MIN_RAW_LABS", "CO_CD");

    # str(mergedDFg);
    meta <- subset(mergedDFg, select=-c(MIN_RAW_LABS, CO_CD));
    return(meta);
}

meta <- createMeta(mergedDF)

# NEED to source(VNL_graph.R) to get table meta below or use createMeta() above
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
# minimal mean absolute error: 1.633847
# Minimal mean squared error: 4.664447
# Best kernel: optimal
# Best k: 14
plot(vnl.train$THRESHOLD_TIME, vnl.knn$fitted.values[[14]][1:356], pch = vnl.train$INT_FLAG+22);
# And it's terrible!
# Another try
model1 <- train.kknn(`THRESHOLD_TIME` ~ ., data = vnl.train, kmax = 50)
# Call:
#     train.kknn(formula = THRESHOLD_TIME ~ ., data = vnl.train, kmax = 50)
#
# Type of response variable: continuous
# minimal mean absolute error: 1.924233
# Minimal mean squared error: 6.260451
# Best kernel: optimal
# Best k: 20
prediction1 <- predict(model1, vnl.test);
prediction1 <- ceil(prediction1);
CM1 <- table(vnl.test$THRESHOLD_TIME, prediction1);
accuracy1 <- (sum(diag(CM1)))/sum(CM1);
accuracy1; # 0.1797753
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
                train = vnl.train, test = vnl.test, k = 14, distance = 1);
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







write.arff(meta2, "meta2.arff", eol = "\n");
meta3 <- read.arff("meta2.arff");
# Simple example choosing k
classifier <- IBk(THRESHOLD_TIME ~ `COMPONENT_ID` + # `INT_FLAG` +
                      `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
                      `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
                      `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
                      `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
                      `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
                      `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
                      `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
                      `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`, data = meta3, na.action = na.omit );
summary(classifier);
# === Summary ===
#
#     Correlation coefficient                  0.9948
# Mean absolute error                      5.1551
# Root mean squared error                 24.6119
# Relative absolute error                  2.3572 %
# Root relative squared error             10.2073 %
# Total Number of Instances              445
classifier;
# IB1 instance-based classifier
# using 1 nearest neighbour(s) for classification
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
       col = alpha("red", 1), bg = alpha("red", .5));
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))

plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
       col = alpha("blue", 1), bg = alpha("blue", .5))
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))
# classifier is awesome. Almost too good to believe!

# Another example letting RWeka find the best value for k
classifier2 <- IBk(THRESHOLD_TIME ~ ., data = meta3);
summary(classifier2);
# === Summary ===
#
#     Correlation coefficient                  1
# Mean absolute error                      0
# Root mean squared error                  0
# Relative absolute error                  0      %
# Root relative squared error              0      %
# Total Number of Instances              445
# classifier2 (above) doesn't seem to be working.
classifier2;
# IB1 instance-based classifier
# using 1 nearest neighbour(s) for classification
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
       col = alpha("red", 1), bg = alpha("red", .5));
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier2$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))

plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
       col = alpha("blue", 1), bg = alpha("blue", .5))
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier2$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))
# Must be overfitting somewhere!

# And 3rd try
classifier3 <- IBk(THRESHOLD_TIME ~ ., data = meta3, control = Weka_control(K = 50, X = TRUE));
evaluate_Weka_classifier(classifier3, numFolds = 10);
# === 10 Fold Cross Validation ===
#
#     === Summary ===
#
#     Correlation coefficient                  0.7995
# Mean absolute error                    118.1431
# Root mean squared error                149.7029
# Relative absolute error                 53.9482 %
# Root relative squared error             61.999  %
# Total Number of Instances              445
classifier3;
# IB1 instance-based classifier
# using 14 nearest neighbour(s) for classification
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
       col = alpha("red", 1), bg = alpha("red", .5));
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier3$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))

plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
     col = alpha("red", 1), bg = alpha("red", .5));
points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
       col = alpha("blue", 1), bg = alpha("blue", .5))
points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
points(meta2$THRESHOLD_TIME, classifier3$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
       col = alpha("green", 1), bg = alpha("green", .5))
# classifier3 is terrible - worst yet!




## CLASSIFYING FOR INT_FLAG
# Trying binary classifier to predict if patient belongs to INT_FLAG==1 category.
flag.cut <- 0.515; # Because classifier returns a continous value between 0-1, not 0 or 1
vnl.knn.flag <- train.kknn(formula = formula(`INT_FLAG` ~ .),
                      data = vnl.train, kmax = 50, distance = 1);
vnl.knn.flag;
# Call:
#     train.kknn(formula = formula(INT_FLAG ~ .), data = vnl.train,     kmax = 50, distance = 1)
#
# Type of response variable: continuous
# minimal mean absolute error: 0.3679775
# Minimal mean squared error: 0.2113924
# Best kernel: optimal
# Best k: 21
vnl.knn.flag1 <- vnl.knn.flag$fitted.values[[21]][1:356];
# Need 1 or 0
vnl.knn.flag1 <-  vapply(vnl.knn.flag1, FUN = function(x){if (x > flag.cut) 1 else 0}, FUN.VALUE = c(1));
CM.flag1 <- table(vnl.knn.flag1, vnl.train$INT_FLAG);
CM.flag1;
# vnl.knn.flag1     0   1
#               0  144  76
#               1   35 101
accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
accuracy.flag1; # [1] 0.6882022
prediction.flag1 <- predict(vnl.knn.flag, vnl.test);
prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > flag.cut) 1 else 0},
                            FUN.VALUE = c(1));
CM.prediction.flag1 <- table(prediction.flag1, vnl.test$INT_FLAG);
CM.prediction.flag1;
# prediction.flag1      0  1
#                   0 40  20
#                   1 10 19
accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
accuracy.prediction.flag1; # [1] 0.6629213

# Finding best flag.cut
findbest.flag.cut <- data.frame(flag.cut = numeric(0),
                                test.accuracy = numeric(0), predict.accuracy = numeric(0));
for (j in seq(0, 1, 0.05)){
    print(j); flag.cut <- j;
    vnl.knn.flag1 <- vnl.knn.flag$fitted.values[[21]][1:356];
    vnl.knn.flag1 <-  vapply(vnl.knn.flag1, FUN = function(x){if (x > flag.cut) 1 else 0}, FUN.VALUE = c(1));
    CM.flag1 <- table(vnl.knn.flag1, vnl.train$INT_FLAG);
    accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
    print(accuracy.flag1);
    prediction.flag1 <- predict(vnl.knn.flag, vnl.test);
    prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > flag.cut) 1 else 0},
                                FUN.VALUE = c(1));
    CM.prediction.flag1 <- table(prediction.flag1, vnl.test$INT_FLAG);
    accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
    print(accuracy.prediction.flag1);
    findbest.flag.cut <- rbind(findbest.flag.cut, data.frame(flag.cut = j,
                                                    test.accuracy = accuracy.flag1,
                                                    predict.accuracy = accuracy.prediction.flag1))
}
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$test.accuracy, panel.first = grid())
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$predict.accuracy, panel.first = grid())
findbest.flag.cut[which.max(findbest.flag.cut$test.accuracy),];
# 11      0.5     0.6882022        0.6629213
findbest.flag.cut[which.max(findbest.flag.cut$predict.accuracy),];
# 12     0.55     0.6685393        0.6741573
#
# Again, finer
findbest.flag.cut <- data.frame(flag.cut = numeric(0),
                                test.accuracy = numeric(0), predict.accuracy = numeric(0));
for (j in seq(0.45, 0.65, 0.005)){ # finding best value for flag.cut between 0.45 and 0.65
    print(j); flag.cut <- j;
    vnl.knn.flag1 <- vnl.knn.flag$fitted.values[[21]][1:356];
    vnl.knn.flag1 <-  vapply(vnl.knn.flag1, FUN = function(x){if (x > flag.cut) 1 else 0}, FUN.VALUE = c(1));
    CM.flag1 <- table(vnl.knn.flag1, vnl.train$INT_FLAG);
    accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
    print(accuracy.flag1);
    prediction.flag1 <- predict(vnl.knn.flag, vnl.test);
    prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > flag.cut) 1 else 0},
                                FUN.VALUE = c(1));
    CM.prediction.flag1 <- table(prediction.flag1, vnl.test$INT_FLAG);
    accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
    print(accuracy.prediction.flag1);
    findbest.flag.cut <- rbind(findbest.flag.cut, data.frame(flag.cut = j,
                                                             test.accuracy = accuracy.flag1,
                                                             predict.accuracy = accuracy.prediction.flag1))
}
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$test.accuracy, panel.first = grid())
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$predict.accuracy, panel.first = grid())
findbest.flag.cut[which.max(findbest.flag.cut$test.accuracy),];
# 14    0.515     0.6966292        0.6853933
findbest.flag.cut[which.max(findbest.flag.cut$predict.accuracy),];
# Setting flag.cut to best f value of 0.515 on line 397



# Attempt with weights on lab value
{# Trying again with weights on the day: weightConst^(ORDERING_DATE2)
weightConst <- 1.1;
vnl.knn.flag2 <- train.kknn(formula
                            = formula(`INT_FLAG` ~ `COMPONENT_ID` + CPT_CODE +
                                          I(`ORD_NUM_VALUE_0`*(weightConst)^(`ORDERING_DATE2_0`) +
                                          `ORD_NUM_VALUE_-1`*(weightConst)^(`ORDERING_DATE2_-1`) +
                                          `ORD_NUM_VALUE_-2`*(weightConst)^(`ORDERING_DATE2_-2`) +
                                          `ORD_NUM_VALUE_-3`*(weightConst)^(`ORDERING_DATE2_-3`) +
                                          `ORD_NUM_VALUE_-4`*(weightConst)^(`ORDERING_DATE2_-4`) +
                                          `ORD_NUM_VALUE_-5`*(weightConst)^(`ORDERING_DATE2_-5`) +
                                          `ORD_NUM_VALUE_-6`*(weightConst)^(`ORDERING_DATE2_-6`) +
                                          `ORD_NUM_VALUE_-7`*(weightConst)^(`ORDERING_DATE2_-7`) +
                                          `ORD_NUM_VALUE_-8`*(weightConst)^(`ORDERING_DATE2_-8`) +
                                          `ORD_NUM_VALUE_-9`*(weightConst)^(`ORDERING_DATE2_-9`) +
                                          `ORD_NUM_VALUE_-10`*(weightConst)^(`ORDERING_DATE2_-10`)) +
                                          AGE + SEX_C + ETHNIC_GROUP_C + PCP_PROV_ID + PAT_MRN +
                                          LNT_OF_STY +
                                          READMT_WITHIN_30_DAYS + ADMT_DIAG + ADMT_MD +
                                          ADMT_NURSS_STTN + ADMT_SRC + ADMT_TYP + PRINC_PROC +
                                          REFRL_SRC + REFRG_MD + APR_DRG_CD + APR_SEV_LVL +
                                          MS_DRG + DISCHRG_DEPT + DISCHRG_DISPOS),
                            data = vnl.train, kmax = 200, distance = 1);
vnl.knn.flag2;
# Type of response variable: continuous
# minimal mean absolute error: 0.4733399
# Minimal mean squared error: 0.2418614
# Best kernel: optimal
# Best k: 105
}
# Attempt with weights on lab value
vnl.knn.flag2 <- vnl.knn.flag2$fitted.values[[105]][1:356];
# Need 1 or 0
vnl.knn.flag2 <-  vapply(vnl.knn.flag2, FUN = function(x){if (x > flag.cut) 1 else 0}, FUN.VALUE = c(1));
CM.flag2 <- table(vnl.knn.flag2, vnl.train$INT_FLAG);
CM.flag2;
#   vnl.knn.flag2   0   1
#               0 122  83
#               1  57  94
accuracy.flag2 <- (sum(diag(CM.flag2)))/sum(CM.flag2);
accuracy.flag2; # [1] 0.6067416
prediction.flag2 <- predict(vnl.knn.flag, vnl.test);
prediction.flag2 <-  vapply(prediction.flag2, FUN = function(x){if (x > flag.cut) 1 else 0},
                            FUN.VALUE = c(1));
CM.prediction.flag2 <- table(prediction.flag2, vnl.test$INT_FLAG);
CM.prediction.flag2;
#   prediction.flag2  0  1
#                   0 40 20
#                   1 10 19
accuracy.prediction.flag2 <- (sum(diag(CM.prediction.flag2)))/sum(CM.prediction.flag2);
accuracy.prediction.flag2; # [1] 0.6629213
# Attempt with weights on lab value is not as good.
# Plus weightConst was guessed arbitrarily.


############## BELOW IS NOT FINISHED!!!!
# plot(vnl.train$THRESHOLD_TIME, vnl.knn$fitted.values[[28]][1:356],
#      pch = vnl.train$INT_FLAG+22, col = vnl.train$INT_FLAG+2);
#
#
#
# # Try again!
# vnl.knn2 <- kknn(formula = formula(`THRESHOLD_TIME` ~ .),
#                  train = vnl.train, test = vnl.test, k = 28, distance = 1);
# # Extracting the prediction
# fit <- ceil(fitted(vnl.knn2));
# table(vnl.test$THRESHOLD_TIME, fit);
# plot(vnl.test$THRESHOLD_TIME, fit, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5), xlim = range(0:20), ylim = range(0:20));
# plot(vnl.test$THRESHOLD_TIME, fit, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# # Also terrible.
#
# # 3rd try
# vnl.knn3 <- kknn(formula = formula(`THRESHOLD_TIME` ~ `COMPONENT_ID` + #`INT_FLAG` +
#                                        `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
#                                        `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
#                                        `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
#                                        `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
#                                        `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
#                                        `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
#                                        `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
#                                        `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`),
#                  train = vnl.train, test = vnl.test, k = 28, distance = 1);
# # Extracting the prediction
# fit3 <- fitted(vnl.knn3);
# table(vnl.test$THRESHOLD_TIME, fit3);
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5), xlim = range(0:20), ylim = range(0:20));
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# # Much better!
# # Comparing to earlier glm plot
# plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#      col = alpha("blue", 1), bg = alpha("blue", .5),
#      xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
# points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#        col = alpha("red", 1), bg = alpha("red", .5));
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
#
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#        col = alpha("blue", 1), bg = alpha("blue", .5))
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
#
#
# # Using RWeka for INT_FLAG Classification
# classifier <- IBk(THRESHOLD_TIME ~ `COMPONENT_ID` + # `INT_FLAG` +
#                       `ORD_NUM_VALUE_0` + `ORD_NUM_VALUE_-1` + `ORD_NUM_VALUE_-2` +
#                       `ORD_NUM_VALUE_-3` + `ORD_NUM_VALUE_-4` + `ORD_NUM_VALUE_-5` +
#                       `ORD_NUM_VALUE_-6` + `ORD_NUM_VALUE_-7` + `ORD_NUM_VALUE_-8` +
#                       `ORD_NUM_VALUE_-9` + `ORD_NUM_VALUE_-10` +
#                       `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
#                       `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
#                       `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
#                       `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`, data = meta3, na.action = na.omit );
# summary(classifier);
#
# classifier;
#
# plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#      col = alpha("blue", 1), bg = alpha("blue", .5),
#      xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
# points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#        col = alpha("red", 1), bg = alpha("red", .5));
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#        col = alpha("blue", 1), bg = alpha("blue", .5))
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
#
# # Another example letting RWeka find the best value for k
# classifier2 <- IBk(THRESHOLD_TIME ~ ., data = meta3);
# summary(classifier2);
#
# classifier2;
#
# plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#      col = alpha("blue", 1), bg = alpha("blue", .5),
#      xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
# points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#        col = alpha("red", 1), bg = alpha("red", .5));
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier2$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#        col = alpha("blue", 1), bg = alpha("blue", .5))
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier2$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
#
# # And 3rd try
# classifier3 <- IBk(THRESHOLD_TIME ~ ., data = meta3, control = Weka_control(K = 50, X = TRUE));
# evaluate_Weka_classifier(classifier3, numFolds = 10);
#
# classifier3;
#
# plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#      col = alpha("blue", 1), bg = alpha("blue", .5),
#      xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
# points(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#        col = alpha("red", 1), bg = alpha("red", .5));
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier3$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
# plot(vnl.test$THRESHOLD_TIME, fit3, pch = vnl.test$INT_FLAG+22, panel.first = grid(),
#      col = alpha("red", 1), bg = alpha("red", .5));
# points(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
#        col = alpha("blue", 1), bg = alpha("blue", .5))
# points(x4$THRESHOLD_TIME, x4$fit, pch = t4, col = alpha("cyan", 1), bg = alpha("cyan", .5), panel.first = grid());
# points(meta2$THRESHOLD_TIME, classifier3$predictions, pch = meta2$INT_FLAG+22, panel.first = grid(),
#        col = alpha("green", 1), bg = alpha("green", .5))
#
