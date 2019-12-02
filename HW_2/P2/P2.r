library(leaps)
library(gridExtra)
library(grid)
library(glmnet)
src = "ticdata2000.txt"

trainData <- read.table(src, sep = '\t')
dim(trainData)

set.seed(123)
trainTrainIndices <- sample(1:nrow(trainData), nrow(trainData)*0.75)
trainTrainData <- trainData[trainTrainIndices, ]
trainTestData<- trainData[-trainTrainIndices, ]


### Fit the ordinary least squares solution to the model.


olsFit <- lm(V86 ~ ., data = trainTrainData)
summary(olsFit)

pred <- predict(olsFit, data = trainTrainData)
predTest <- predict(olsFit, newdata=trainTestData)
olsOutput <- as.data.frame(pred)
olsOutput <- cbind(olsOutput, trainTrainData[86])
names(olsOutput)[names(olsOutput) == 'V86'] <- 'true'
plot(olsOutput$pred, olsOutput$true, xlab="Predictions", ylab="True values")


  
marginCalcFit <- lm(true ~ ., data = olsOutput)
coef(marginCalcFit)



#Finding the best margin
marginValues <- seq(from = 0, to =  1, by =  0.1/2)
trainErrors = rep(0, length(marginValues))
testErrors = rep(0, length(marginValues))
marginIndex = 1
for (marginVal in marginValues) {
  
  trainTrainErr = 0
  
  # compute and record the trainTrain error for the current  marginVal
  classification = ifelse(pred < marginVal, 0, 1)
  trainErrors[marginIndex] = sum(abs(classification - trainTrainData[, "V86"]))
  
  #compute the train-test error and record it
  classification = ifelse(predTest < marginVal, 0, 1)
  testErrors[marginIndex] = sum(abs(classification - trainTestData[, "V86"]))
  
  marginIndex = marginIndex + 1
}
missclassificationTable <- data.frame(marginValues, trainErrors, testErrors)
grid.table(missclassificationTable)
plot(marginValues, trainErrors, xlab="Margin Values", ylab="Misclassifications", type="b", col="blue")

trainAccuracyOLS = 1-(273/nrow(trainTrainData))
testAccuracyOLS = 1-(76/nrow(trainTestData))
print(paste("Training accuracy: ", trainAccuracyOLS))
print(paste("Test accuracy: ", testAccuracyOLS))


### Forward Subset selection
regfit.fwd = regsubsets(V86 ~ ., data = trainTrainData, nvmax = 85, method = "forward")
plot(regfit.fwd, scale="Cp")
fwdStSummary = summary(regfit.fwd)

#compute the misclassifications for each of the training, and test data set.
misClassTrainFwd = rep(NA, 85)
misClassTestFwd = rep(NA, 85)
trainMatrix = model.matrix(V86 ~ ., trainTrainData)
testMatrix = model.matrix(V86 ~., trainTestData)
for(i in 1:85){
  coefi = coef(regfit.fwd, id = i)
  trainSubset = trainMatrix[, names(coefi)]
  testSubset = testMatrix[, names(coefi)]
  trainPred = trainSubset%*%coefi
  testPred = testSubset%*%coefi
  trainPred = ifelse(trainPred < 0.45, 0, 1)
  testPred = ifelse(testPred < 0.45, 0, 1)
  misClassTrainFwd[i] <- sum(abs(trainPred - trainTrainData[, "V86"]))
  misClassTestFwd[i] <- sum(abs(testPred - trainTestData[, "V86"]))
}

trainAccuracyFwd = 1 - (misClassTrainFwd/nrow(trainTrainData))
testAccuracyFwd = 1 -  (misClassTestFwd / nrow(trainTestData))

plot(trainAccuracyFwd, ylab="Training Accuracy", pch=19, type="b")
legend("topright", legend = c("Train"), col=c("black"), pch=19)
plot(testAccuracyFwd, ylab="Test Accuracy", col="blue", pch = 19, type = "b")
legend("topright", legend = c("Test"), col=c("blue"), pch=19)

### Using backward selection

regfit.bkwd = regsubsets(V86 ~ ., data = trainTrainData, nvmax = 85, method = "backward")
plot(regfit.bkwd, scale="Cp")

#compute the misclassifications for each of the training, and test data set.
misClassTrainBwd = rep(NA, 85)
misClassTestBwd = rep(NA, 85)
trainMatrix = model.matrix(V86 ~ ., trainTrainData)
testMatrix = model.matrix(V86 ~., trainTestData)
for(i in 1:85){
  coefi = coef(regfit.bkwd, id = i)
  trainSubset = trainMatrix[, names(coefi)]
  testSubset = testMatrix[, names(coefi)]
  trainPred = trainSubset%*%coefi
  testPred = testSubset%*%coefi
  trainPred = ifelse(trainPred < 0.45, 0, 1)
  testPred = ifelse(testPred < 0.45, 0, 1)
  misClassTrainBwd[i] <- sum(abs(trainPred - trainTrainData[, "V86"]))
  misClassTestBwd[i] <- sum(abs(testPred - trainTestData[, "V86"]))
}

trainAccuracyBwd = 1 - (misClassTrainBwd/nrow(trainTrainData))
testAccuracyBwd = 1 -  (misClassTestBwd / nrow(trainTestData))

plot(trainAccuracyBwd, ylab="Training Accuracy", pch=19, type="b")
legend("topright", legend = c("Train"), col=c("black"), pch=19)
plot(testAccuracyBwd, ylab="Test Accuracy", col="blue", pch = 19, type = "b")
legend("topright", legend = c("Test"), col=c("blue"), pch=19)

### Ridge regression

trainTrainX = trainTrainData[, -which(names(trainTrainData) == "V86")]
trainTrainY = trainTrainData[, which(names(trainTrainData) == "V86")]
ridge.mod = glmnet(as.matrix(trainTrainX), trainTrainY, alpha = 0)
X = as.matrix(trainTrainX)
cvRidge = cv.glmnet(X, trainTrainY, alpha=0)
plot(cvRidge)

ridgeBestLam <- cvRidge$lambda.min
round(ridgeBestLam, digits = 2)


testMatrix = as.matrix(trainTrainData[, -which(names(trainTrainData) == "V86")])
ridge.pred2 <- predict(ridge.mod, s = ridgeBestLam, type="response", newx = testMatrix)
trainTrainPred <- ridge.pred2
trainTrainPred = ifelse(trainTrainPred < 0.45, 0, 1)
misClassTrainRidge <- sum(abs(trainTrainPred - trainTrainData[, "V86"]))
trainAccuracyRidge <- 1 - (misClassTrainRidge/nrow(trainTrainData))
trainAccuracyRidge

testMatrix = as.matrix(trainTestData[, -which(names(trainTestData) == "V86")])
ridge.pred2 <- predict(ridge.mod, s = ridgeBestLam, type="response", newx = testMatrix)
trainTestPred <- ridge.pred2
trainTestPred = ifelse(trainTestPred < 0.45, 0, 1)
misClassTestRidge <- sum(abs(trainTestPred - trainTestData[, "V86"]))
testAccuracyRidge <- 1 - (misClassTestRidge/nrow(trainTestData))
testAccuracyRidge



### The LASSO

lasso.mod <- glmnet(as.matrix(trainTrainX), trainTrainY, alpha = 1)
plot(lasso.mod)

cv.out = cv.glmnet(as.matrix(trainTrainX), trainTrainY, alpha = 1)
plot(cv.out)

lassoBestLam = cv.out$lambda.min
lassoBestLam

predict <- predict(lasso.mod, s = lassoBestLam, type = "coefficients")
predict


lassoPredictTrainTrain <- predict(lasso.mod, s = lassoBestLam, type = "response", newx = as.matrix(trainTrainX))
trainTrainPred = ifelse(lassoPredictTrainTrain < 0.45, 0, 1)
misClassTrainLasso <- sum(abs(trainTrainPred - trainTrainY))
trainTrainAccuracyRidge <- 1 - (misClassTrainLasso / nrow(trainTrainData))
trainTrainAccuracyRidge

trainTestX = trainTestData[, -which(names(trainTestData) == "V86")]
trainTestY = trainTestData[, which(names(trainTestData) == "V86")]
lassoPredictTrainTest <- predict(lasso.mod, s = lassoBestLam, type = "response", newx = as.matrix(trainTestX))
trainTestPred = ifelse(lassoPredictTrainTest < 0.45, 0, 1)
misClassTestLasso <- sum(abs(trainTestPred - trainTestY))
trainTestAccuracyRidge <- 1 - (misClassTestLasso / nrow(trainTestData))
trainTestAccuracyRidge



### Predicting the models on the unseen data.


src = "ticeval2000.txt"
newPredictors <- read.table(src, sep = '\t')
src = "tictgts2000.txt"
response <- read.table(src, sep='\t')
newDataComplete = data.frame(newPredictors, response)
names(newDataComplete)[names(newDataComplete) == 'V1.1'] <- "V86"
newDataMatrix = model.matrix(V86 ~ ., newDataComplete)


newDataPred <- predict(olsFit, data = newPredictors)
newDataPred = ifelse(newDataPred < 0.45, 0, 1)
missClasNewPred <- sum(abs(newDataPred - response))
newDataAccuracyOLS <- 1 - (missClasNewPred / nrow(newPredictors))
newDataAccuracyOLS

coefi = coef(regfit.fwd, id = 10)
newFwdPred = newDataMatrix[, names(coefi)]%*%coefi
newFwdPred = ifelse(newFwdPred < 0.45, 0, 1)
newMisClassFwd <- sum(abs(newFwdPred - response))
newAccFwd <- 1 - (newMisClassFwd/nrow(newPredictors))
newAccFwd


coefi = coef(regfit.bkwd, id = 10)
newBwdPred = newDataMatrix[, names(coefi)]%*%coefi
newBwdPred = ifelse(newBwdPred < 0.45, 0, 1)
newMisClassBwd <- sum(abs(newBwdPred - response))
newAccBwd <- 1 - (newMisClassBwd/nrow(newPredictors))
newAccBwd

trainTestX = newDataComplete[, -which(names(newDataComplete) == "V86")]
trainTestY = newDataComplete[, which(names(newDataComplete) == "V86")]


testMatrix = as.matrix(newDataComplete[, -which(names(trainTestData) == "V86")])
ridge.pred2 <- predict(ridge.mod, s = ridgeBestLam, type="response", newx = testMatrix)
trainTestPred <- ridge.pred2
trainTestPred = ifelse(trainTestPred < 0.45, 0, 1)
misClassTestRidge <- sum(abs(trainTestPred - trainTestY))
testAccuracyRidge <- 1 - (misClassTestRidge/length(trainTestY))
testAccuracyRidge

lassoPredictTrainTest <- predict(lasso.mod, s = lassoBestLam, type = "response", newx = as.matrix(trainTestX))
trainTestPred = ifelse(lassoPredictTrainTest < 0.45, 0, 1)
misClassTestLasso <- sum(abs(trainTestPred - trainTestY))
trainTestAccuracyRidge <- 1 - (misClassTestLasso / nrow(newDataComplete))
trainTestAccuracyRidge
