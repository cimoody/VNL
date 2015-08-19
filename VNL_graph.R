# VNL_graph
# Unsuccessful attempt at creating a graph to classify and cluster patients.
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

# Used function from VNL_kNN_kMeans.R Must source(VNL_kNN_kMeans.R) to use it.
meta <- createMeta(mergedDF)
mergedDFg <- meta;

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
# plot(g)


# Example
# library(igraph);
# g <- make_empty_graph() %>%
#     add_vertices(3, color = "red") %>%
#     add_vertices(2, color = "green") %>%
#     add_edges(c(1,2, 2,3, 3,4, 4,5))
# g
# V(g)[[]]
# plot(g)

