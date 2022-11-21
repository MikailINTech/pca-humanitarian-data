library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(psych)
library(reshape2)

my_wd = "c:/Users/duzen/anaconda3/Git_repos/data-analysis"
setwd(my_wd)

# Data processing

df_init <- read.csv2("Country-data.csv", header = TRUE, sep = ",")

df <- subset(df_init, select=-c(country))
sapply(df, class)
df <- sapply(df, as.numeric)
rownames(df) <- df_init$country
dim(df)

names(df)

str(df)

summary(df)

# First, lets check the correlation between the variables.

pairs.panels(df,
             gap = 0,
             pch = 21)

# Some variables such as child mortality and total fertility
# are highly correlated

# PCA
pca <- prcomp(df, center = TRUE, scale = TRUE)
summary(pca)

eig_val <- get_eigenvalue(pca)
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# We can see that 95% of the information is contained in 5 variables

fviz_pca_var(pca, col.var = "black")

fviz_pca_ind (pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             )


df_transform <- as.data.frame(-pca$x[, 1:5])
fviz_nbclust(df_transform, kmeans, method = "wss")

# It looks like 4 clusters is an optimal value
# We thus select k = 4 for our kmeans

k <- 4
kmeans_df <- kmeans(df_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_df, data = df_transform)

# The blue cluster regroups the countries where the socio-economic
# is the most critic