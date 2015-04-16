# Loading libraries
library(MASS)         # LDA and QDA
library(class)        # KNN
library(tree)         # Tree
library(rpart)        # Tree
library(randomForest) # Random Forest

# Load data
letter.recognition <- read.csv("letter-recognition.data")
attach(letter.recognition)

# Basic information about the data
names(letter.recognition)
dim(letter.recognition)

# Training
train <- sample(1:nrow(letter.recognition), 16000)

# Correlation
plot(X1,X3, pch = 20, col = "red", xlab = "X1", ylab = "X3")
cor(X1,X3)


###############################################################################
#                    Linear Discriminant Analysis                             #
###############################################################################

# Fit LDA model with all predictors
lda.fit <- lda(Letter~., letter.recognition[train,])

# Make prediction
lda.test.pred <- predict(lda.fit, letter.recognition[-train,])
lda.test.class <- lda.test.pred$class

# Test error
lda.test.rate <- mean(lda.test.class == Letter[-train])

# New fit with predictors interaction
lda2.fit <- lda(Letter~.*.+.:.:., letter.recognition[train,])

#  Make prediction
lda2.test.pred <- predict(lda2.fit, letter.recognition[-train,])
lda2.test.class <- lda2.test.pred$class

# Test error
lda2.test.rate <- mean(lda2.test.class == Letter[-train])


###############################################################################
#                    Quadratic Discriminant Analysis                          #
###############################################################################

# Let's see with QDA
qda.fit <- qda(Letter~., letter.recognition[train,])

# Make prediction
qda.test.pred <- predict(qda.fit, letter.recognition[-train,])
qda.test.class <- qda.test.pred$class

# Compute error
qda.test.rate <- mean(qda.test.class == Letter[-train])

# Now we use combinations
qda2.fit <- qda(Letter~.*., letter.recognition[train,])

#  Make again the prediction
qda2.test.pred <- predict(qda2.fit, letter.recognition[-train,])
qda2.test.class <- qda2.test.pred$class

# Compute new error
qda2.test.rate <- mean(qda2.test.class==Letter[-train])


###############################################################################
#                                      KNN                                    #
###############################################################################

# Set labels
label <- letter.recognition[train,1]

# Explore different K values
error.rate <- numeric(10)
for(i in 1:10){
  knn.pred <- knn(letter.recognition[train,-1],
                  letter.recognition[-train,-1],label, k = i)
  error.rate[i] <- 1-mean(knn.pred == letter.recognition[-train, 1])
}

# Plot error rates
plot(1:10, error.rate,"b", pch = 20,
     col = "red", xlab = "K", ylab = "Error Rate")

# Make prediction with K = 1
knn.pred <- knn(letter.recognition[train,-1],
                letter.recognition[-train,-1], label, k = 1)

# Save test rate
knn.test.rate <- mean(knn.pred == letter.recognition[-train, 1])

# Same with Cross Validation
knn.cv.pred <- knn.cv(letter.recognition[,-1], letter.recognition[,1], k = 1)
knn.cv.test.rate <- mean(knn.cv.pred == letter.recognition[, 1])


###############################################################################
#                                   Tree                                      #
###############################################################################

# Fit a tree
tree.fit <- tree(Letter~., letter.recognition[1:200,])
summary(tree.fit)

# Plot fitted tree
plot(tree.fit)
text(tree.fit,pretty=0)

# Make prediction
tree.pred <- predict(tree.fit, letter.recognition[-train,], type="class")

# Test rate
tree.test.rate <- mean(tree.pred == letter.recognition[-train, 1])

# Using rpart (more vervosity)
tree2.fit <- rpart(Letter~., letter.recognition, method="class")
summary(tree2.fit)

# Plot it
plot(tree2.fit)
text(tree2.fit)

# Make charts
par(mfrow=c(1,2))
rsq.rpart(tree2.fit)
par(mfrow=c(1,1))

# Make test prediction
tree2.pred <- predict(tree2.fit, letter.recognition[,], type="class")

# Compute test rate
tree2.test.rate <- mean(tree2.pred == letter.recognition[, 1])

###############################################################################
#                                  Bagging                                    #
###############################################################################

# Fit a bagging model
bagging.fit <- randomForest(Letter~.,letter.recognition[train,],
                            mtry=16,importance=TRUE)

# See fit info
bagging.fit

# Make prediction and save error rate
bagging.pred <- predict(bagging.fit,letter.recognition[-train,])
bagging.test.rate <- mean(bagging.pred == letter.recognition[-train, 1])
# It's almost OOB estimate error rate


###############################################################################
#                               Random Forest                                 #
###############################################################################

# Default fit
randomForest.fit <- randomForest(Letter ~ ., data=letter.recognition[train, ])

# Review the prefious fit
randomForest.fit # 500 trees (4 lenght)

# Let's see each variable importance
importance(randomForest.fit)

# Make a prediction
randomForest.pred <- predict(randomForest.fit , letter.recognition[-train, ])

# Test rate
randomForest.test.rate <- mean(
    randomForest.pred == letter.recognition[-train, 1])

# Compute number of variables to select
sqrt(dim(letter.recognition)[2]-1)

# Make the fit with differents parameters
randomForest2.fit <- randomForest(Letter ~ .,
                                  data=letter.recognition[train, ],
                                  ntree=100, mtry=4,
                                  importance=TRUE)
randomForest2.pred <- predict(randomForest2.fit , letter.recognition[-train, ])
randomForest2.test.rate<- mean(
    randomForest2.pred == letter.recognition[-train, 1])

# Explore with other parameters to ensure optimum on 4
randomForest3.fit <- randomForest(Letter ~ .,
                                 data=letter.recognition[train, ],
                                 ntree=500, mtry=5,
                                 importance=TRUE)
randomForest3.pred <- predict(randomForest3.fit , letter.recognition[-train, ])
randomForest3.test.rate <- mean(
    randomForest3.pred == letter.recognition[-train, 1])

# Let's combine previous random forestsfits
rf.1 <- randomForest(Letter ~ ., data=letter.recognition[train,])
rf.2 <- randomForest(Letter ~ ., data=letter.recognition[train,])
rf.3 <- randomForest(Letter ~ ., data=letter.recognition[train,])
rf.all <- combine(rf.1, rf.2, rf.3)

# Make prediction
randomForest.c.pred <- predict(rf.all , letter.recognition[-train,])
randomForest.c.test.rate <- mean(
    randomForest.c.pred == letter.recognition[-train, 1])

# Spin the training data
train1 <- sample(1:nrow(letter.recognition[train, ]), 16000)
rf1 <- randomForest(Letter ~ .*., data=letter.recognition[train1, ])
train2 <- sample(1:nrow(letter.recognition[train, ]), 16000)
rf2 <- randomForest(Letter ~ .*., data=letter.recognition[train2, ])
train3 <- sample(1:nrow(letter.recognition[train, ]), 16000)
rf3 <- randomForest(Letter ~ .*., data=letter.recognition[train3, ])
train4 <- sample(1:nrow(letter.recognition[train, ]), 16000)
rf4 <- randomForest(Letter ~ .*., data=letter.recognition[train4, ])
train5 <- sample(1:nrow(letter.recognition[train, ]), 16000)
rf5 <- randomForest(Letter ~ .*., data=letter.recognition[train5, ])

rfall <- combine(rf1, rf2, rf3, rf4, rf5)

# Make prediction
randomForest.a.pred <- predict(rfall , letter.recognition[-train,])
randomForest.a.test.rate <- mean(
    randomForest.a.pred == letter.recognition[-train, 1])


###############################################################################
#                                 Results                                     #
###############################################################################

# LDA
print(lda.test.rate)
print(lda2.test.rate)

# QDA
print(qda.test.rate)
print(qda2.test.rate)

# KNN
print(knn.cv.test.rate)

# Tree(simple)
print(tree2.test.rate)

# Bagging
print(bagging.test.rate)

# Random Forests
print(randomForest.a.test.rate)
