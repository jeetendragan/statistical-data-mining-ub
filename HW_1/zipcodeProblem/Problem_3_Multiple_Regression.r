
########################################################################
# Zip code prediction starts here
########################################################################

# Load the Elem of stat learning library
library("ElemStatLearn");

###########################################################
#### Funtn to find the accuracy of the prediction
###########################################################
calculateAccuracy <- function(predictions, actualValues){
	correctPredictionCnt = replicate(10, 0); # 10 digits
    totalTypes = replicate(10, 0); # 10 digits

    for(index in 1:length(actualValues)){
        predictedValue = predictions[index];
        actualValue = actualValues[index];
        if(predictedValue == actualValue){
            # becoz 0 will be at index 1, 1 will be at index 2, and so on
            correctPredictionCnt[predictedValue+1] = correctPredictionCnt[predictedValue+1] + 1;
        }
        totalTypes[predictedValue+1] = totalTypes[predictedValue+1] + 1;
    }

    accuracy = sum(correctPredictionCnt)/sum(totalTypes);
	return(accuracy)
}

# read the training data into a data frame
trainingData <- data.frame(zip.train);

# get all column names
columnNames = names(trainingData);

# rename all the column names as V(#) - just as a convension(because I noticed column names changing)
newColumnNames = {};
count = 1;
for (columnName in columnNames)
{
    newColumnNames[count] = paste("V",count, sep="");
    count = count + 1;
}
# Set the new column names
names(trainingData) = newColumnNames; 

# strip data off to only include 2's, and 3's
trainingData <- subset(trainingData, V1 == 2 | V1 == 3);

# get the feature names
featureNames = names(trainingData)[-1];

# format the feature names as '+' separated values
formatedFeatureNames = paste(featureNames, collapse = "+");

# build the linear model formula
linearModelFormula = paste("V1 ~ ", formatedFeatureNames, sep="");

# generate a multiple regression fit
fit = lm(formula = linearModelFormula, data = trainingData);

# predictions below 2.5 will be classified as digits 2, and those above it will be classified as digits 3
yBoundary = 2.5;

# generate the summary of the fit
# summary(fit);

# read the test data to a data frame
testData = data.frame(zip.test);

# get all column names
columnNames = names(testData);

# rename all the column names as V(#) - just as a convension(because I noticed column names changing)
newColumnNames = {};
count = 1;
for (columnName in columnNames)
{
    newColumnNames[count] = paste("V",count, sep="");
    count = count + 1;
}
# Set the new column names
names(testData) = newColumnNames;

# strip the test data to include only 2's and 3's
testData <- subset(testData, V1 == 2 | V1 == 3);

prediction = predict(fit, testData);

predVect = as.vector(prediction);
xValues = seq(from=1, to=length(predVect), by = 1)

predVect[predVect < yBoundary] = 2;
predVect[predVect >= yBoundary] = 3;

testSetAccuracy = calculateAccuracy(predVect, testData$V1);
# find the accuracy
print(testSetAccuracy);

trainingSetPred = predict(fit, trainingData);
predVect = as.vector(trainingSetPred);
print("Training set prediction summary(Quantitative)");
summary(predVect);
xValues = seq(from=1, to=length(predVect), by = 1);

jpeg("Fit result");
plot(xValues, predVect, xlab="Counter", ylab="Dependent Variable(Training set)");
dev.off();

predVect[predVect < yBoundary] = 2; predVect[predVect >= yBoundary] = 3;

trainingSetAccuracy = calculateAccuracy(predVect, trainingData$V1);
# find the accuracy
print(trainingSetAccuracy);
