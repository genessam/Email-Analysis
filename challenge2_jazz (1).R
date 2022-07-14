# K-Means Clustering:

rm(list=ls())

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(stringr)
library(ggplot2)
library(tidyverse)
library(stopwords)
library(ggpubr)
```


# Loading in the Data
```{r}
setwd("~/Downloads/")
x <- read.csv("Clinton.csv")


x$Recipient <- str_replace(x$Recipient, "Secretary", "H")
x$Recipient <- str_replace(x$Recipient, "Madam Secretary", "H")
x$Recipient <- str_replace(x$Recipient, "Hillary", "H")
x$Recipient <- str_replace(x$Recipient, "Clinton  Hillary", "H")
x$Recipient <- str_replace(x$Recipient, "Secretary Clinton", "H")
x$Recipient <- str_replace(x$Recipient, "Hill", "H")
x$Recipient <- str_replace(x$Recipient, "hillcr state gov", "H")
x$Recipient <- str_replace(x$Recipient, "H preines", "H")
x$Recipient <- str_replace(x$Recipient, "hr15 mycingular blackberry net", "H")
x$Recipient <- str_replace(x$Recipient, "Clinton", "H")
x$Recipient <- str_replace(x$Recipient, " H", "H")
x$Recipient <- str_replace(x$Recipient, "H2", "H")
x$Recipient <- str_replace(x$Recipient, "HRC", "H")


x$Sender <- str_replace(x$Sender, "Secretary", "H")
x$Sender <- str_replace(x$Sender, "HRC", "H")
x$Sender <- str_replace(x$Sender "Huma clintonemail com", "H")
x$Sender <- str_replace(x$Sender, "Clinton  Hillary R", "H")
x$Sender <- str_replace(x$Sender, "Secretary of State", "H")
x$Sender <- str_replace(x$Sender, "Hillary Rodham Clinton", "H")
x$Sender <- str_replace(x$Sender, "Clinton  Hillary Rodham", "H")
x$Sender <- str_replace(x$Sender, "hrod17 clintonemail com", "H")
x$Sender <- str_replace(x$Sender, "Secretary of State", "H")
x$Sender <- str_replace(x$Sender, " H", "H")
x$Sender <- str_replace(x$Sender, "Secretary of State", "H")


nrow(unique(x[c("Recipient")]))
nrow(unique(x[c("Sender")]))


x$Recipient <- ifelse(x$Recipient=="H", 1, 0)
fromH <- x[x$Recipient==0,]
toH <- x[x$Recipient==1,]

fromH <- fromH[,10:3011]
toH <- toH[,10:3011]

# Stop Words, using stopwords package
stopwords <- stopwords::stopwords("en", source = "snowball")


### Additional Cleaning, removing stop words and irrelevant columns
fromH_words <- colnames(fromH)
fromH_words <- fromH_words[nchar(fromH_words) >= 3]
fromH <- fromH[, fromH_words]
fromH <- fromH[, -grep("X", colnames(fromH))]
fromH <- fromH[ , -which(names(fromH) %in% c(stopwords))]

toH_words <- colnames(toH)
toH_words <- toH_words[nchar(toH_words) >= 3]
toH <- toH[, toH_words]
toH <- toH[, -grep("X", colnames(toH))]
toH <- toH[ , -which(names(toH) %in% c(stopwords))]

row_lengths = apply(fromH, 1, function(z) sqrt(sum(z^2)))
fromH_norm = fromH/row_lengths
fromH_norm <- na.omit(fromH_norm)

row_lengths = apply(toH, 1, function(z) sqrt(sum(z^2)))
toH_norm = toH/row_lengths
toH_norm <- na.omit(toH_norm)

# To check the normalized sets 
sqrt(sum(fromH_norm[1,]^2))
sqrt(sum(toH_norm[1,]^2))

#for received emails
# Setting k = 9
k <- 9
set.seed(123)
toH_k9 <- kmeans(toH_norm, centers = 9)
toH_k9_words <- list()
centroid_toH <- list()
distinct_toH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_toH[[i]] <- toH_k9$centers[i, ]
  toH_k9_words[[i]] <- sort(centroid_toH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(toH_k9$centers[-i,], 2, mean)
  distinct_toH[[i]] <- sort(centroid_toH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}

# Setting k = 5
k <- 5
set.seed(123)
toH_k5 <- kmeans(toH_norm, centers = 5)
toH_k5_words <- list()
centroid_toH <- list()
distinct_toH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_toH[[i]] <- toH_k5$centers[i, ]
  toH_k5_words[[i]] <- sort(centroid_toH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(toH_k5$centers[-i,], 2, mean)
  distinct_toH[[i]] <- sort(centroid_toH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}

# Setting k = 12
k <- 12
set.seed(123)
toH_k12 <- kmeans(toH_norm, centers = 12)
toH_k12_words <- list()
centroid_toH <- list()
distinct_toH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_toH[[i]] <- toH_k12$centers[i, ]
  toH_k12_words[[i]] <- sort(centroid_toH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(toH_k12$centers[-i,], 2, mean)
  distinct_toH[[i]] <- sort(centroid_toH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}


#for sent emails
# Setting k = 9
k <- 9
set.seed(123)
fromH_k9 <- kmeans(fromH_norm, centers = 9)
fromH_k9_words <- list()
centroid_fromH <- list()
distinct_fromH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_fromH[[i]] <- fromH_k9$centers[i, ]
  fromH_k9_words[[i]] <- sort(centroid_fromH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(fromH_k9$centers[-i,], 2, mean)
  distinct_fromH[[i]] <- sort(centroid_fromH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}

# Setting k = 5
k <- 5
set.seed(123)
fromH_k5 <- kmeans(fromH_norm, centers = 5)
fromH_k5_words <- list()
centroid_fromH <- list()
distinct_fromH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_fromH[[i]] <- fromH_k5$centers[i, ]
  fromH_k5_words[[i]] <- sort(centroid_fromH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(fromH_k5$centers[-i,], 2, mean)
  distinct_fromH[[i]] <- sort(centroid_fromH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}

# Setting k = 12
k <- 12
set.seed(123)
fromH_k12 <- kmeans(fromH_norm, centers = 12)
fromH_k12_words <- list()
centroid_fromH <- list()
distinct_fromH <- list()
mean_centroid <- list()
for(i in 1:k){
  centroid_fromH[[i]] <- fromH_k12$centers[i, ]
  fromH_k12_words[[i]] <- sort(centroid_fromH[[i]], decreasing = T)[1:10]
  mean_centroid[[i]] <- apply(fromH_k12$centers[-i,], 2, mean)
  distinct_fromH[[i]] <- sort(centroid_fromH[[i]] - mean_centroid[[i]], decreasing = T)[1:10]
}

