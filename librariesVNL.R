## Libraries to install and source (so it is done once) for VNL

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

# Package for classification
library(rpart);
install.packages('rattle');
library(rattle);
install.packages('rpart.plot');
library(rpart.plot);
install.packages('RColorBrewer');
library(RColorBrewer);
install.packages('randomForest');
library(randomForest);
install.packages('party');
library(party);

# Additional package for categorical graphics
install.packages("vcd");
library(vcd);
