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
# Another try
model1 <- train.kknn(`THRESHOLD_TIME` ~ ., data = vnl.train, kmax = 50)
# Call:
#     train.kknn(formula = THRESHOLD_TIME ~ ., data = vnl.train, kmax = 50)
#
# Type of response variable: continuous
# minimal mean absolute error: 108.8319
# Minimal mean squared error: 22547.23
# Best kernel: optimal
# Best k: 25
prediction1 <- predict(model1, vnl.test);
prediction1 <- ceil(prediction1);
CM1 <- table(vnl.test$THRESHOLD_TIME, prediction1);
accuracy1 <- (sum(diag(CM1)))/sum(CM1);
accuracy1; # 0.0225
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
vnl.knn.flag <- train.kknn(formula = formula(`INT_FLAG` ~ .),
                      data = vnl.train, kmax = 50, distance = 1);
vnl.knn.flag;
# Call:
#     train.kknn(formula = formula(INT_FLAG ~ .), data = vnl.train,     kmax = 50, distance = 1)
#
# Type of response variable: continuous
# minimal mean absolute error: 0.06741573
# Minimal mean squared error: 0.04686007
# Best kernel: optimal
# Best k: 13
vnl.knn.flag1 <- vnl.knn.flag$fitted.values[[13]][1:356];
# Need 1 or 0
vnl.knn.flag1 <-  vapply(tflag, FUN = function(x){if (x > 0.2) 1 else 0}, FUN.VALUE = c(1));
CM.flag1 <- table(vnl.knn.flag1, vnl.train$INT_FLAG);
CM.flag1;
# vnl.knn.flag1     0   1
#               0 160   7
#               1  23 166
accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
accuracy.flag1; # [1] 0.9157303
prediction.flag1 <- predict(vnl.knn.flag, vnl.test);
prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > 0.2) 1 else 0},
                            FUN.VALUE = c(1));
CM.prediction.flag1 <- table(prediction.flag1, vnl.test$INT_FLAG);
CM.prediction.flag1;
# prediction.flag1      0  1
#                    0 41  0
#                    1  5 43
accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
accuracy.prediction.flag1; # [1] 0.9438202

plot(vnl.train$THRESHOLD_TIME, vnl.knn$fitted.values[[28]][1:356],
     pch = vnl.train$INT_FLAG+22, col = vnl.train$INT_FLAG+2);



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


# Using RWeka for INT_FLAG Classification
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

classifier;

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


# Another example letting RWeka find the best value for k
classifier2 <- IBk(THRESHOLD_TIME ~ ., data = meta3);
summary(classifier2);

classifier2;

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


# And 3rd try
classifier3 <- IBk(THRESHOLD_TIME ~ ., data = meta3, control = Weka_control(K = 50, X = TRUE));
evaluate_Weka_classifier(classifier3, numFolds = 10);

classifier3;

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

