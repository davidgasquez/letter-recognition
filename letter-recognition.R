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
qda.fit = qda(Letter~., letter.recognition[train,])

# Make prediction
qda.test.pred = predict(qda.fit, letter.recognition[-train,])
qda.test.class = qda.test.pred$class

# Compute error
qda.test.rate = mean(qda.test.class == Letter[-train])

# Now we use combinations
qda2.fit=qda(Letter~.*., letter.recognition[train,])

#  Make again the prediction
qda2.test.pred=predict(qda2.fit, letter.recognition[-train,])
qda2.test.class=qda2.test.pred$class

# Compute new error
qda2.test.rate = mean(qda2.test.class==Letter[-train])


###############################################################################
#                                 Results                                     #
###############################################################################

# From LDA we have
lda.test.rate
lda2.test.rate

# QDA is better than LDA
qda.test.rate
qda2.test.rate
