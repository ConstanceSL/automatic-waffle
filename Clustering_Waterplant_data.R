# Created by: Constance
# Created on: 17.05.18

library(cluster)
library(NbClust)
library(gridExtra)
library(corrplot)
library(fossil)
library(Matrix)
library(gplots)
library(factoextra)
library(plyr)
library(FactoMineR)

# Analysing the faults in a urban waste water treatment plan
# Data from http://archive.ics.uci.edu/ml/machine-learning-databases/water-treatment/
# Each column represents a different measurement at the plant, and each row a day

# Setting the seed
set.seed(456)

#----------------------------------------------------------#
#            LOADING THE DATA & CLEANING IT                #
#----------------------------------------------------------#

# Adding the column names to the csv from
# http://archive.ics.uci.edu/ml/machine-learning-databases/water-treatment/water-treatment.names
columns = c('Date', 'Q-E', 'ZN-E', 'PH-E',  'DBO-E',  'DQO-E',  'SS-E', 'SSV-E',
'SED-E', 'COND-E', 'PH-P', 'DBO-P',  'SS-P', 'SSV-P',  'SED-P',
'COND-P',  'PH-D',  'DBO-D',  'DQO-D',  'SS-D', '-D',  'SED-D',
'COND-D', 'PH-S',  'DBO-S',  'DQO-S',  'SS-S',  'SSV-S',  'SED-S',
'COND-S', 'RD-DBO-P',  'RD-SS-P',  'RD-SED-P',  'RD-DBO-S',  'RD-DQO-S',
'RD-DBO-G', 'RD-DQO-G',  'RD-SS-G',  'RD-SSED-G')
water <- read.csv('water-treatment.csv', col.names=columns)

# Looking at the dataframe
View(water)

# Replacing all the ? by NA
water[water == '?'] <- NA


# Turning the factors to doubles (as the variables are actually numeric and continuous)
as.numeric.factor <- function(x) {
    as.numeric(as.character(x))
}
water[,-1] <- apply(water[,-1], 2, as.numeric.factor)


# Checking the number of NA per row, to see how distributed they are across the days
checking.row <- function(row){
    nas <- sum(is.na(row))
    return(nas)
}
na.rows <- as.data.frame(apply(water, 1, checking.row))
table(na.rows)

# There are 380 full cases out of 526 so I will cluster over the full cases first
# List of other options to deal with missing values:
#https://www.displayr.com/5-ways-deal-missing-data-cluster-analysis/

## Selecting only the row that have no missing value
water_no_na <- na.omit(water)

## Normalising the data
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
water_normalised <- apply(water_no_na[,-1], 2, normalize)


#----------------------------------------------------------#
#                   CHECKING CORRELATIONS                  #
#----------------------------------------------------------#

## Let's check the correlations between the features
corrplot(cor(water_normalised), tl.cex= 0.5, tl.col='black')

# Some correlations look pretty high, let's check in more details
correlations <- cor(water_normalised)

# Flattening the results and turning them into a data frame
correlations <- as.data.frame(as.table(correlations))
# Selecting only the high correlations
#(more than 0.8 or less than -0.8, also needs to remove the 1s)
high_correlations <- subset(correlations, abs(Freq) >= 0.8 & Freq !=1)
View(high_correlations)
# Removing doubles: not that simple as the variables are interverted in doubles
# Took a bit of working around, and here is Timo Klingler's solution (thanks!!):
# Turning to strings, ordering and removing doubles:
high_correlations <- data.frame(lapply(high_correlations, as.character),
stringsAsFactors=FALSE)
high_correlations = t(apply(high_correlations, 1, sort))
high_correlations = as.data.frame(unique(high_correlations))
colnames(high_correlations) <- c('Corr', 'Var1', 'Var2')

# Before dropping some features, let's see which ones led to the most NA
# And see if selecting the right features can reduce the number of incomplete cases
checking.row <- function(row){
    number_of_nas <- sum(is.na(row))
    return(number_of_nas)
}
na.per.columns <- as.data.frame(apply(water, 2, checking.row))
na.per.columns <- tibble::rownames_to_column(as.data.frame(na.per.columns))

# Merging the table with NAs and the one with the correlations
# For variable 1
df1 <- merge(na.per.columns, high_correlations, by.x='rowname', by.y='Var1')
colnames(df1) <- c("Feat1", "NAs Feat1", "Correlation", "Feat2")
# For variable 2
df1 <- merge(df1, na.per.columns, by.x='Feat2', by.y='rowname')
colnames(df1) <- c("Feat2", "Feat1", "NAs Feat1", "Correlation", "NAs Feat2")
# Reordering the results to make it easier to read
number_na_high_corr_features <- df1[,c(2,3,1,5,4)]

# It seems that we could get rid of some missing values
# Let's see how they are spread across the rows
columns_to_check <- append(number_na_high_corr_features$Feat1,
number_na_high_corr_features$Feat2)
checking.row <- function(row){
    nas <- sum(is.na(row))
    return(nas)
}
na.rows <- as.data.frame(apply(water[, columns_to_check], 1, checking.row))
table(na.rows)

## It seems to be different rows, so dropping correlated columns according to their number
# of NAs makes sense
columns_to_drop <- c('COND.S', 'COND.D', 'COND.P', 'PH.D', 'PH.P', 'RD.DBO.S',
'RD.DQO.S', 'SED.E', 'SS.E', 'SED.P', 'DBO.S', 'SSV.P', 'DBO.D',
'RD.SSED.G', 'RD.SS.G')

## Reselecting the data without these columns
water_no_corr <- water[, - which(names(water) %in% columns_to_drop)]


#----------------------------------------------------------#
#                  REPLACING MISSING VALUES                #
#----------------------------------------------------------#

## Checking the NAs
checking.row <- function(row){
    nas <- sum(is.na(row))
    return(nas)
}
na.rows <- as.data.frame(apply(water_no_corr, 1, checking.row))
table(na.rows)
## It globally increased, but added only three full case...
# Let's check which columns are missing values
summary(water_no_corr)

# None of them are missing too many values compared to the number of rows
# Let's replace the missing values by a random value within the range of the column
# (I know there is a lot of debate about how to deal with NAs, and in this case
# it seemed that the best may have been to just remove the rows with NAs, which
# I did in the run of the code, and gave similar results as the current version.
# Because I did it last with the NAs replaced by random values, I'm leaving it like this.
# In any case dealing sensibly with the NAs would require knowing a lot more about the
# data, how it was collected and what the different features exactly measure, which
# was not really the aim here)
for(name in colnames(water_no_corr[2:24])){
    min_col = min(water_no_corr[,name], na.rm=TRUE)
    max_col = max(water_no_corr[,name], na.rm=TRUE)
    tot_col = sum(is.na(water_no_corr[,name]))
    water_no_corr[, name][is.na(water_no_corr[,name])] <-
    sample(seq(from=min_col, to=max_col, by=0.01) ,tot_col)
}

# Normalising
normalize <- function(x) {
    x <- (x - min(x)) / (max(x) - min(x))
    return (x)
}
water_normalized <- apply(water_no_corr[,-1], 2, normalize)


#----------------------------------------------------------#
#                         CLUSTERING                       #
#----------------------------------------------------------#
# Checking what a hierarchical euclidian clustering would do
heatmap.2(water_normalized, Colv=NA, labRow=FALSE, dendrogram ='row', trace='none',
hline='none', density.info="none")
## Looks pretty bad, let's go for k-means

# Deciding on the number of clusters for a K-means
# Methods from http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determining-the-optimal-number-of-clusters-3-must-know-methods/
# Elbow method (result k=4)
fviz_nbclust(water_normalized, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
# Silhouette method (result k=3)
fviz_nbclust(water_normalized, kmeans, method = "silhouette")+
labs(subtitle = "Silhouette method")
# Gap statistic (result k = 10)
fviz_nbclust(water_normalized, kmeans, nstart = 25,
method = "gap_stat", nboot = 500, verbose=FALSE)+
labs(subtitle = "Gap statistic method")

## The methods desagree a fair bit, but let's go with 4 as the data is big enough
# and the original paper used 13, so we'll compare 4 and 13
clusters.kmeans.4 <- kmeans(water_normalized, 4)
# Adding the clusters to the data
clusters_4 <- (clusters.kmeans.4$cluster)
water_normalized_clustered <- cbind(water_normalized, clusters_4)
# Checking if looks like something on heatmap
water_normalized_clustered <- as.data.frame(arrange(as.data.frame(water_normalized_clustered), clusters_4))
water_for_heat <- as.matrix(water_normalized_clustered[, 1:23])
heatmap.2(water_for_heat, labRow=FALSE, dendrogram ='none', trace='none',
hline='none', density.info="none", Rowv=NA, main='Heatmap by k-means clusters (k=4)')


# Doing the same for 13 clusters, as in the original paper
clusters.kmeans.13 <- kmeans(water_normalized, 13)
clusters_13 <- (clusters.kmeans.13$cluster)
water_normalized_clustered <- cbind(water_normalized_clustered, clusters_13)
water_normalized_clustered <- as.data.frame(arrange(as.data.frame(water_normalized_clustered), clusters_13))

water_for_heat <- as.matrix(water_normalized_clustered[, 1:23])
heatmap.2(water_for_heat, labRow=FALSE, dendrogram ='none', trace='none',
hline='none', density.info="none", Rowv=NA, main='Heatmap by k-means clusters (k=13)')

# The heatmap with 4 clusters shows more pattern (although not a lot),
# let's see how similar the clusters are with a plot
# (adding jitter to see the number of points)
ggplot(water_normalized_clustered, aes(x = clusters_4, y = clusters_13)) +
geom_jitter(position = position_jitter(width = 0.1),size=1)
# And checking the rand.index
rand.index(clusters_4, clusters_13)
# They have a 72% overlap, althought it doesn't seem from the plot


#----------------------------------------------------------#
#                   PLOTTING THE CLUSTERS                  #
#----------------------------------------------------------#

# Doing PCA and plotting the different clusters to see how they look
pca <- PCA(water_normalized_clustered, scale.unit=TRUE, ncp=5, graph=T)
# Getting the principal components
dimdesc(pca)
# Checking how much variance they each account for
summary(pca)
## The two first components account for only 28% of the variance, which is not very high
# Let's still plot the points and compare how the clusters look
water_normalized_clustered <- cbind(water_normalized_clustered, pca$ind$coord)
pl1 = ggplot(water_normalized_clustered, aes(x = Dim.1, y = Dim.2, col=clusters_4)) +
    geom_point(size=2) + geom_point()
pl2 = ggplot(water_normalized_clustered, aes(x = Dim.1, y = Dim.2, col=clusters_13)) +
    geom_point(size=2) + geom_point()
grid.arrange(pl1, pl2)
# It looks MUCH better with 4 clusters, where you can clearly see a pattern
# where with 4 it just looks like noise

# Turning the clusters to factors to do boxplots
water_normalized_clustered$clusters_13 <-
as.factor(water_normalized_clustered$clusters_13)

water_normalized_clustered$clusters_4 <-
as.factor(water_normalized_clustered$clusters_4)


# Doing box plots for all variables
water_normalized_clustered <- as.data.frame(water_normalized_clustered)
n = 0
for (i in names(water_normalized_clustered[1:23])){
    n = n + 1
    title = paste('Feature ', as.character(n))
    plot = ggplot(water_normalized_clustered,
    aes(x=clusters_4, y=water_normalized_clustered[, i])) +
        geom_boxplot() + ggtitle(title)
    print(plot)
}
# It's hard to see which features are more defining for the clusters, let's compare means

#----------------------------------------------------------#
#              TESTING FEATURE DIFFERENCES                 #
#----------------------------------------------------------#

## Doing t-test for all pair-wise clusters for each variables
# Keeping only the p-values to see which differences matter most
# (the idea here is not to have statistically significant results,
# as this performs too many test no to have a lot of false negatives,
# and adjusting the p-value would reduce it too much, but this will
# help us spot where the differences are the most significant)
tests_VAR1 = c(1)
for (i in names(water_normalized_clustered[1:23])){
    test.1.2 <- t.test(water_normalized_clustered[clusters_4 == 1, i],
    water_normalized_clustered[clusters_4 == 2, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.1.2)
    test.1.3 <- t.test(water_normalized_clustered[clusters_4 == 1, i],
    water_normalized_clustered[clusters_4 == 3, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.1.3)
    test.1.4 <- t.test(water_normalized_clustered[clusters_4 == 1, i],
    water_normalized_clustered[clusters_4 == 4, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.1.4)
    test.2.3 <- t.test(water_normalized_clustered[clusters_4 == 2, i],
    water_normalized_clustered[clusters_4 == 3, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.2.3)
    test.2.4 <- t.test(water_normalized_clustered[clusters_4 == 2, i],
    water_normalized_clustered[clusters_4 == 4, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.2.4)
    test.3.4 <- t.test(water_normalized_clustered[clusters_4 == 3, i],
    water_normalized_clustered[clusters_4 == 4, i])$p.value
    tests_VAR1 = append(tests_VAR1, test.3.4)
}
# Removing the value used to create the vector
tests_VAR1[-1]
# Reshaping the vector
t.tests.results <- matrix(tests_VAR1[-1], 23, 6,  byrow = T)
# Naming the columns and rows
colnames(t.tests.results) <- c('1.2', '1.3', '1.4', '2.3', '2.4', '3.4')
rownames(t.tests.results) <- names(water_normalized_clustered[1:23])
## Keeping only p.values below 0.1 to make it more readable
t.tests.results <- ifelse(t.tests.results < 0.01, t.tests.results, NA )
View(t.tests.results)

## Let's keep only the most important features, and plot them
# First let's assign the clusters back to the non normalised data
water_no_corr <- cbind(water_no_corr, clusters_4)
water_no_corr <- as.data.frame(arrange(as.data.frame(water_no_corr), clusters_4))
# And turning the clusters to factors
water_no_corr$clusters_4 <- as.factor(water_no_corr$clusters_4)

# And let's plot for the 4 significant features
plot1 <- ggplot(water_no_corr, aes(x=clusters_4, y=water_no_corr$SSV.E)) +
    geom_boxplot() +
    ggtitle('Feature: SSV.E') +
    xlab('Clusters') +
    ylab('SSV.E')

plot2 <- ggplot(water_no_corr, aes(x=clusters_4, y=water_no_corr$COND.E)) +
    geom_boxplot() +
    ggtitle('Feature: COND.E') +
    xlab('Clusters') +
    ylab('COND.E')

plot3 <- ggplot(water_no_corr, aes(x=clusters_4, y=water_no_corr$DQO.D)) +
    geom_boxplot() +
    ggtitle('Feature: DQO.D') +
    xlab('Clusters') +
    ylab('DQO.D')

plot4 <- ggplot(water_no_corr, aes(x=clusters_4, y=water_no_corr$SSV.S)) +
    geom_boxplot() +
    ggtitle('Feature: SSV.S') +
    xlab('Clusters') +
    ylab('SSV.S')

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# And here it is: the most defining features for each cluster of days
# Granted, it doesn't look very impressive...
# ... but maybe it would if I knew more about water plants ;)



