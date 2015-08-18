# VNL_graph
# by Cristina Moody
# Aug 2015

# Loading package
require(igraph);
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

str(mergedDF);
# Things to Check
#   CPT_CODE   ETHNIC_GROUP_C  PCP_PROV_ID     CO_CD  BILL_ADMS_DT      BILL_DISCHRG_DT
#   READMT_WITHIN_30_DAYS   ADMT_DIAG    ADMT_NURSS_STTN    ADMT_SRC    ADMT_TYP
#   PRINC_PROC  REFRL_SRC   REFRG_MD    APR_DRG_CD      APR_SEV_LVL
#   DISCHRG_DEPT    DISCHRG_DISPOS
#   REFERENCE_UNIT      REFERENCE_LOW       REFERENCE_HIGH

mergedDFg <- mergedDF;
# Fixing CPT_CODE
mergedDFg$CPT_CODE <- factor(mergedDFg$CPT_CODE, labels = c("80048", "80053.01", "80069", "CBCD", "85027"));
summary(mergedDFg$CPT_CODE);
summary(table(mergedDFg$CPT_CODE));
# Fixing REFERENCE_UNIT
str(mergedDFg);
summary(table(mergedDFg$REFERENCE_UNIT));
summary((mergedDFg$REFERENCE_UNIT));
class(mergedDFg$REFERENCE_UNIT);
mergedDFg$REFERENCE_UNIT <- factor(mergedDFg$REFERENCE_UNIT, labels = c("mg/dL", "mmol/L", "K/uL"));
# Fixing REFERENCE_LOW
summary(table(mergedDFg$REFERENCE_LOW));
summary((mergedDFg$REFERENCE_LOW));
class(mergedDFg$REFERENCE_LOW);
mergedDFg$REFERENCE_LOW <- factor(mergedDFg$REFERENCE_LOW, labels = c("6", "0.7", "3.5", "135", "150", "4"));
mergedDFg$REFERENCE_LOW <- f2n(mergedDFg$REFERENCE_LOW)
# Fixing REFERENCE_HIGH
summary(table(mergedDFg$REFERENCE_HIGH));
summary((mergedDFg$REFERENCE_HIGH));
class(mergedDFg$REFERENCE_HIGH);
mergedDFg$REFERENCE_HIGH <- f2n(mergedDF$REFERENCE_HIGH)
# Fixing ETHNIC_GROUP_C
str(mergedDFg$ETHNIC_GROUP_C);
summary(table(mergedDFg$ETHNIC_GROUP_C));
summary((mergedDFg$ETHNIC_GROUP_C));
class(mergedDFg$ETHNIC_GROUP_C);
mergedDFg$ETHNIC_GROUP_C <- as.numeric(mergedDF$ETHNIC_GROUP_C);
# Fixing PCP_PROV_ID
mergedDFg$PCP_PROV_ID <- as.numeric(mergedDF$PCP_PROV_ID);
# Fixing CO_CD
str(mergedDFg$CO_CD);
summary(table(mergedDFg$CO_CD));
summary((mergedDFg$CO_CD));
class(mergedDFg$CO_CD);
mergedDFg$CO_CD <- as.factor(mergedDF$CO_CD); # Can remove this column all the same
# Fixing BILL_ADMS_DT
str(mergedDFg$BILL_ADMS_DT);
(table(mergedDFg$BILL_ADMS_DT));
summary((mergedDFg$BILL_ADMS_DT));
class(mergedDFg$BILL_ADMS_DT);
mergedDFg$BILL_ADMS_DT <- mdy(mergedDF$BILL_ADMS_DT, tz = "EST");
# Fixing BILL_DISCHRG_DT
str(mergedDFg$BILL_DISCHRG_DT);
(table(mergedDFg$BILL_DISCHRG_DT));
summary((mergedDFg$BILL_DISCHRG_DT));
class(mergedDFg$BILL_DISCHRG_DT);
mergedDFg$BILL_DISCHRG_DT <- mdy(mergedDF$BILL_DISCHRG_DT, tz = "EST");
# Fixing READMT_WITHIN_30_DAYS
str(mergedDFg$READMT_WITHIN_30_DAYS);
(table(mergedDFg$READMT_WITHIN_30_DAYS));
summary((mergedDFg$READMT_WITHIN_30_DAYS));
class(mergedDFg$READMT_WITHIN_30_DAYS);
mergedDFg$READMT_WITHIN_30_DAYS <- as.factor(mergedDF$READMT_WITHIN_30_DAYS);
# Fixing ADMT_DIAG
str(mergedDFg$ADMT_DIAG);
(table(mergedDFg$ADMT_DIAG));
summary((mergedDFg$ADMT_DIAG));
class(mergedDFg$ADMT_DIAG);
mergedDFg$ADMT_DIAG <- as.factor(mergedDF$ADMT_DIAG);
# Fixing ADMT_NURSS_STTN
str(mergedDFg$ADMT_NURSS_STTN);
(table(mergedDFg$ADMT_NURSS_STTN));
summary((mergedDFg$ADMT_NURSS_STTN));
class(mergedDFg$ADMT_NURSS_STTN);
mergedDFg$ADMT_NURSS_STTN <- as.factor(mergedDF$ADMT_NURSS_STTN);
# Fixing ADMT_SRC
str(mergedDFg$ADMT_SRC);
(table(mergedDFg$ADMT_SRC));
summary((mergedDFg$ADMT_SRC));
class(mergedDFg$ADMT_SRC);
mergedDFg$ADMT_SRC <- as.factor(mergedDF$ADMT_SRC);
# Fixing ADMT_TYP
str(mergedDFg$ADMT_TYP);
summary(table(mergedDFg$ADMT_TYP));
summary((mergedDFg$ADMT_TYP));
class(mergedDFg$ADMT_TYP);
mergedDFg$ADMT_TYP <- as.factor(mergedDF$ADMT_TYP);
# Fixing PRINC_PROC
str(mergedDFg$PRINC_PROC);
summary(table(mergedDFg$PRINC_PROC));
summary((mergedDFg$PRINC_PROC));
class(mergedDFg$PRINC_PROC);
mergedDFg$PRINC_PROC <- as.numeric(mergedDF$PRINC_PROC);
# Fixing REFRL_SRC
str(mergedDFg$REFRL_SRC);
summary(table(mergedDFg$REFRL_SRC));
summary((mergedDFg$REFRL_SRC));
class(mergedDFg$REFRL_SRC);
mergedDFg$REFRL_SRC <- as.factor(mergedDF$REFRL_SRC);
# Fixing REFRG_MD
str(mergedDFg$REFRG_MD);
summary(table(mergedDFg$REFRG_MD));
summary((mergedDFg$REFRG_MD));
class(mergedDFg$REFRG_MD);
mergedDFg$REFRG_MD <- as.numeric(mergedDF$REFRG_MD);
# Fixing APR_DRG_CD
str(mergedDFg$APR_DRG_CD);
summary(table(mergedDFg$APR_DRG_CD));
summary((mergedDFg$APR_DRG_CD));
class(mergedDFg$APR_DRG_CD);
mergedDFg$APR_DRG_CD <- as.numeric(mergedDF$APR_DRG_CD);
# Fixing APR_SEV_LVL
str(mergedDFg$APR_SEV_LVL);
summary(table(mergedDFg$APR_SEV_LVL));
summary((mergedDFg$APR_SEV_LVL));
class(mergedDFg$APR_SEV_LVL);
mergedDFg$APR_SEV_LVL <- as.numeric(mergedDF$APR_SEV_LVL);
# Fixing DISCHRG_DEPT
str(mergedDFg$DISCHRG_DEPT);
summary(table(mergedDFg$DISCHRG_DEPT));
summary((mergedDFg$DISCHRG_DEPT));
class(mergedDFg$DISCHRG_DEPT);
mergedDFg$DISCHRG_DEPT <- as.factor(mergedDF$DISCHRG_DEPT);
# Fixing DISCHRG_DISPOS
str(mergedDFg$DISCHRG_DISPOS);
summary(table(mergedDFg$DISCHRG_DISPOS));
summary((mergedDFg$DISCHRG_DISPOS));
class(mergedDFg$DISCHRG_DISPOS);
mergedDFg$DISCHRG_DISPOS <- as.factor(mergedDF$DISCHRG_DISPOS);

# Columns to remove from classification
removCol <- c("MIN_RAW_LABS", "CO_CD");

str(mergedDFg)
meta <- subset(mergedDFg, select=-c(MIN_RAW_LABS, CO_CD));

# Testing
gtest <- head(mergedDFg, 10);
cols <- c("ENC_CSN_ID", "CPT_CODE");
df <- gtest[cols]
# Brute, slow
m <- table(df)
M <- as.matrix(m)
# Faster, better for large df
library('Matrix')
A <- spMatrix(nrow=length(unique(df$ENC_CSN_ID)),
              ncol=length(unique(df$CPT_CODE)),
              i = as.numeric(factor(df$ENC_CSN_ID)),
              j = as.numeric(factor(df$CPT_CODE)),
              x = rep(1, length(as.numeric(df$ENC_CSN_ID))) )
row.names(A) <- levels(factor(df$ENC_CSN_ID))
colnames(A) <- levels(factor(df$CPT_CODE))
A
# Brute, slow
Arow <- A %*% t(A)
# Faster, better for large df
Arow <- tcrossprod(A);
Arow;
# 7 x 7 sparse Matrix of class "dsCMatrix"
#               71153023 71219915 71395955 71507750 71599346 71690301 71715754
# 71153023        9        9        9        6        9        6        6
# 71219915        9       13       11        8       13        8        6
# 71395955        9       11       10        7       11        7        6
# 71507750        6        8        7        5        8        5        4
# 71599346        9       13       11        8       13        8        6
# 71690301        6        8        7        5        8        5        4
# 71715754        6        6        6        4        6        4        4
# To get the one-mode matrix formed by the column entities (i.e. the number of people) enter the following command:
Acol <- t(A) %*% A;
Acol;
# 2 x 2 sparse Matrix of class "dgCMatrix"
#       80048 CBCD
# 80048    48   19
# CBCD     19   11
gArow <- graph.adjacency(Arow, diag = FALSE, mode = 'undirected')
plot(gArow, layout = layout.fruchterman.reingold(gArow))
# summary(gtest$ENC_CSN_ID)
# gtable <- as.data.frame(table(gtest$ENC_CSN_ID))
#
# g <- make_empty_graph() %>%
#     add_vertices(length(gtable$Var1), color = "blue")
# %>%
#     add_edges( , color = "pink")
plot(g)


# Example
# library(igraph);
# g <- make_empty_graph() %>%
#     add_vertices(3, color = "red") %>%
#     add_vertices(2, color = "green") %>%
#     add_edges(c(1,2, 2,3, 3,4, 4,5))
# g
# V(g)[[]]
# plot(g)

