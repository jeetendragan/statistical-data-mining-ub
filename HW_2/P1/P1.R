library(ISLR)
library(glmnet)
library(pls)
library(gridExtra)
library(grid)

data("College")
names(College)

summary(College)

corrgram::corrgram(College)

### a) Validation set approach


data <- na.omit(College)
isPrivate = as.numeric(data$Private) - 1
data$Private <- isPrivate
set.seed(1)
trainingIndices = sample(1:nrow(data), round(nrow(data)*0.7))
trainingData = data[trainingIndices, ]
validationData = data[-trainingIndices, ]
validationSetFit <- lm(Apps ~ ., data = trainingData)

summary(validationSetFit)


trainPred <- predict(validationSetFit, trainingData)
err <- trainPred - trainingData$Apps
trainRSS = sum((err)^2)
trainRMSE = sqrt(trainRSS/nrow(trainingData))


round(trainRSS,digits = 2)
  
trainRmseVS = round(trainRMSE, digits = 2)
trainRmseVS

pred <- predict(validationSetFit, validationData)
err <- pred - validationData$Apps
testRSS = sum((err)^2)
round(testRSS, digits = 2)

testRMSE = sqrt(testRSS / nrow(validationData))
testRmseVS = round(testRMSE, digits = 2)
testRmseVS
### b) Ridge regression

ridge.mod = glmnet(as.matrix(data[, c(-2)]), data[, c(2)], alpha = 0)
X = as.matrix(trainingData[, -c(2)])
Y = trainingData[, 2]
cvRidge = cv.glmnet(X, Y, alpha=0)
plot(cvRidge)

bestLam <- cvRidge$lambda.min
round(bestLam, digits = 2)

ridge.pred <- predict(ridge.mod, s = bestLam, type="coefficients")
ridge.pred

testMatrix = as.matrix(validationData[, -2])
ridge.pred2 <- predict(ridge.mod, s = bestLam, type="response", newx = testMatrix)
y_pred <- ridge.pred2
ridgeRSS = sum((y_pred - validationData[, 2])^2)
ridgeRSS
ridgeRMSE = sqrt(ridgeRSS / nrow(validationData))

round(ridgeRMSE, digits = 2)


### c) The LASSO

lasso.mod <- glmnet(as.matrix(trainingData[, -2]), trainingData[, 2], alpha = 1)
plot(lasso.mod)

cv.out = cv.glmnet(as.matrix(trainingData[, -2]), trainingData[, 2], alpha = 1)
plot(cv.out)

lassoBestLam = cv.out$lambda.min
round(lassoBestLam, digits = 2)

predict <- predict(lasso.mod, s = lassoBestLam, type = "coefficients")
predict

lassoPredictTest <- predict(lasso.mod, s = lassoBestLam, type = "response", newx = as.matrix(validationData[, -2]))
lassoRss <- sum((lassoPredictTest - validationData[, 2])^2)
lassoRMSE <- sqrt(lassoRss / nrow(validationData))
  
lassoRMSE


### e) Principle component analysis

set.seed(2)
pcr.fit = pcr(Apps ~., data = trainingData, scale = TRUE, validation = "none")
summary(pcr.fit)

# Evaluate performance of the model with "i" components in the pca regression for test and training.
pcrTrainRMSE <- c()
pcrTestRMSE <- c()
for (i in 1:17){
  pcr.pred.train = predict(pcr.fit, trainingData, ncomp = i)
  pcr.pred.test = predict(pcr.fit, validationData, ncomp = i)
  train.error <- sqrt(mean((pcr.pred.train - trainingData[, 2])^2))
  test.error <- sqrt(mean((pcr.pred.test - validationData[, 2])^2))
  pcrTrainRMSE <- c(pcrTrainRMSE, train.error)
  pcrTestRMSE <- c(pcrTestRMSE, test.error)
}
plot(pcrTrainRMSE, col = "blue", xlab="Principle Components", ylab="Root MSE", pch=19, type="b",ylim=range(1000, 4500))
points(pcrTestRMSE, pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col=c("blue","black"), pch=19)

pcr.fitCv = pcr(Apps ~. , data = trainingData, scale = TRUE, validation = "CV")
validationplot(pcr.fitCv, val.type = "MSEP")

testRmsePcr = round(pcrTestRMSE[9], digits=2)
testRmsePcr

### f) Partial Least Squares
set.seed(1)
pls.fit = plsr(Apps ~., data = trainingData, scale = TRUE, validation = "none")
summary(pls.fit)

plsTrainRMSE <- c()
plsTestRMSE <- c()
for (i in 1:17){
  pls.pred.train = predict(pls.fit, trainingData, ncomp = i)
  pls.pred.test = predict(pls.fit, validationData, ncomp = i)
  train.error <- sqrt(mean((pls.pred.train - trainingData[, 2])^2))
  test.error <- sqrt(mean((pls.pred.test - validationData[, 2])^2))
  plsTrainRMSE <- c(plsTrainRMSE, train.error)
  plsTestRMSE <- c(plsTestRMSE, test.error)
}
plot(plsTrainRMSE, col = "blue", xlab="Principle Components", ylab="Root MSE", pch=19, type="b",ylim=range(1000, 2000))
points(plsTestRMSE, pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col=c("blue","black"), pch=19)

pls.fitCv = plsr(Apps ~., data = trainingData, scale = TRUE, validation = "CV")
summary(pls.fitCv)
validationplot(pls.fitCv, val.type = "MSEP")

testRmsePls = round(plsTrainRMSE[6], digits = 2)
testRmsePls

### Commment on the results

ModelNames = c("LS", "Ridge","Lasso", "PCR", "PLS")
TestErrors = c(testRmseVS, ridgeRMSE, lassoRMSE, testRmsePcr, testRmsePls)
TestErrorComplete = data.frame(ModelNames, TestErrors)
grid.table(TestErrorComplete)
hist(data$Apps)

boxplot(data$Apps)
summary(data$Apps)
