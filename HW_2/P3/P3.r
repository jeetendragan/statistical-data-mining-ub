library(leaps)
set.seed(500)
beta = rnorm(14, 2, 3)
beta[15] <- 6
beta[16] <- 0
beta[17] <- 0
beta[18] <- 0
beta[19] <- 0
beta[20] <- 0
beta <- sample(beta, 20)
#beta[1] <- 1


print('Here are the true model coefficients.')

beta

xranges = matrix(nrow = 20, ncol=2) # a matrix that contains the min and max for each range.
set.seed(678)
for(i in 1:20){
  rangeNum = sort(runif(2, min = 10, max = 4000))
  xranges[i, 1] <- floor(rangeNum[1])
  xranges[i, 2] <- floor(rangeNum[2])
}
# Approach - For each feature, generate 1000 random numbers according to a randomly generated range.

print('I have randomly generated the range of each predictor. Each predictor will take on values from the following range.')

xranges

#Using the ranges of predictors, I have generated 1000 values for each predictor.

# Randomly generate X values in the ranges generated for predictors.
X = matrix(nrow = 1000, ncol = 20)
set.seed(123)
for(i in 1:20){
  rangeLow <- xranges[i, 1]
  rangeHigh <- xranges[i, 2]
  predictors = runif(1000, min = rangeLow, max = rangeHigh)
  for(j in 1:1000)
  {
    X[j, i] <- predictors[j]
  }
}

#This is the head of predictors.

head(X)

#The error values are generated from a normal distribution with mean = 0, and standard deviation = 1600. The following cell shows the head of errors.



set.seed(99)
errors = rnorm(1000, 0, 1600)
head(errors)

#**Why did I choose such a large value of standard deviation for error?**
  
  
  #1. The range of values taken by predictors are large 
#2. To add variance to the data so that the coefficients with 0 values are not accidentally valued by the model.


#**This is the head of the response variable.**
  
  
# generate the model
intercept = 0
Y = intercept + X%*%beta + errors
head(Y)

#**Following is the correlation matrix of the complete data.**
  
# Generating the test set and training set.
trainingIndices = sample(1:nrow(X), round(nrow(X)*0.9))
data <- as.data.frame(X)
data <- cbind(data, Y)
corrgram::corrgram(data)
trainingData = data[trainingIndices, ]
testData <- data[-trainingIndices, ]


#A few variables are strongly-positively correlated with the response, and only a few are strongly-weakly correlated. There is a little to no correlation between some variables and response. Also, the predictors themselves are not correlated to each other.

#***Results of subset selection***
  
  
 # Following are a few graps for each of the subsets

trainingSetFit = regsubsets(Y ~ ., trainingData, nvmax = 20, method = "exhaustive")
trainSummary = summary(trainingSetFit)
subsetSize = 1:20


plot(subsetSize, trainSummary$cp, ylab = "CP", type = "b", xlab = "Subset Size")
plot(subsetSize, trainSummary$adjr2, ylab="Adj R^2", type="b", xlab = "Subset Size")


#The graph below shows that as the subset size increases, the CP and adjusted R-square, which estimate the test error also decreases.


plot(subsetSize, trainSummary$rsq, ylab="R^2",type = "b", xlab = "Subset Size")

#R-square increases as the number of variables are added to the model. This is explained because addition of variables helps us explain variance in the **training set**.


plot(subsetSize, trainSummary$rss, ylab="RSS",type="b", xlab = "Subset Size")


#RSS also decreases with an increase in the number of predictors. We essentially overfit the data as we add more variables, hence the RSS gets reduced. 


#Following graph shows the root mean squared error for each model on the training data. The decrease in RMSE is expected as the RSS also decreases, which indicates an increase in variance which can further lead to overfitting.

testErrors = rep(NA, 19)
testMatrix = model.matrix(Y ~ ., testData)
for(i in 1:20){
  coefi = coef(trainingSetFit, id = i)
  subset = testMatrix[, names(coefi)]
  predictions = subset%*%coefi
  testErrors[i] = mean((testData$Y - predictions)^2)
}
trainRssMeans = trainingSetFit$rss[-1] / 1000
plot(sqrt(trainRssMeans), col = "blue", pch = 19, type = "b", xlab="Model size", ylab="Root MSE")
legend("topright", legend = c("Training"), col=c("blue"), pch=19)


#The graph below shows the root mean squared error on  the test data. 


plot(sqrt(testErrors), ylab="Root MSE", pch=19, type="b", xlab="Model size")
legend("topright", legend = c("Test"), col=c("black"), pch=19)


#**For which model size does the test set MSE take on its minimum value?**
  
bestTestSetModel = which(testErrors == min(testErrors))
bestTestSetModel

#There is not much decrease in the RMSE after 12, actually it increases slightly and does not drop back. The model with 11 predictors also has the same RMSE. It might perform similar to the one with 12.

#The best model has the following coefficient values.

coefi = coef(trainingSetFit, id = bestTestSetModel)
coefi

#For a model with 11 coefficients has the following values.

coefi =  coef(trainingSetFit, id = 11)
coefi

#It drops the predictor V8. The true value of the the coefficient is -2.252910. Model with 12 features does a better job of estimating the true model.

#The true model is:
  
for(i in 1:20){
  cat(sprintf("V%d: %f\n", i, beta[i]))
}


#There is a striking resemblance between the true coefficients and the coefficients for the model with least test error. Best subset selection has removed each of the 4 zero coefficients that I added before and it has also removed those coefficients that have a very small value. Also, the coefficient values are very close to the true model.

#V17 which does not have a very small coefficient, has been removed in the model with 12 coefficients. It has been added to the model with 13 coefficients as shown below. 


coefi =  coef(trainingSetFit, id = 13)
coefi


#We could also choose this model for deployment. But there will be an obvious bias towards the choice as we already know the true values of the coefficients. In practice we would choose a simpler model.
