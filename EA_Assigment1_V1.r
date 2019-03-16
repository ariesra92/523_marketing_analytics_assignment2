# MEF BDA - Marketing Analytics / Assigment 1
## US Adult Income Prediction
##### Esra Ari

# Introduction

#1- Read the data in R & Show summary
#2- Develop six cluster solution with hclust. Try different method.
#3- Develop six cluster solution with kmeans
#4- Show Scree Plot
#5-Show the cluster centers original value
#6-Simple Interpretation of Clusters


# 1- Read the data in R & Show summary
setwd("C:/Users/aries/Desktop/MEF/523_Marketing_Analytics/Assigment1")
data <-read.csv("clustering.csv")
summary(data)

str(data)

splitdf <- function(data, seed=1234) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(data)
  trainindex <- sample(index, trunc(round(length(index)*0.5)))
  trainset <- data[trainindex, ]
  testset <- data[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

tr <-splitdf(data)
training_set <- data.frame(tr$trainset)
testing_set <- data.frame(tr$testset)

# 2- Develop six cluster solution with hclust. Try different method.
my_dist <- dist(training_set[c(2,32)], method = "euclidean")
print(my_dist)

# Apply Hierarchical Clustering
fit <- hclust(my_dist, method="ward.D2")
