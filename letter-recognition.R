# Loading libraries
library(MASS)         # LDA and QDA

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
#                    Linear discriminant analysis                             #
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
