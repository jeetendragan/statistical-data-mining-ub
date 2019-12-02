# the script takes one command line argument, the value of K. 
# the script applies KNN for all of the values of K and plots the accuracy on a graph
# Load the Elem of stat learning library

library("ElemStatLearn");
# load class library
require(class);
library("ggplot2");

########################################################################
##  Function to fit the data to KNN
########################################################################

knnFit <- function(trainingData, testData, k){
    set.seed(1);
    testFeatures = subset(testData, select=-c(V1));
    # get the feature names
    featureNames = names(trainingData)[-1];
    prediction <- knn(trainingData[, c(featureNames)], testFeatures, trainingData$V1, k)
    predictionValues = c(2, 3)[sapply(prediction, as.numeric)];
    actualTestDataValue = subset(testData, select=c(V1));
    actualTestDataValue = as.data.frame(actualTestDataValue);

    # find the accuracy
    correctPredictionCnt = replicate(10, 0); # 10 digits
    totalTypes = replicate(10, 0); # 10 digits

    for(index in 1:length(actualTestDataValue$V1)){
        predictedValue = predictionValues[index];
        actualValue = actualTestDataValue$V1[index];
        if(predictedValue == actualValue){
            # becoz 0 will be at index 1, 1 will be at index 2, and so on
            correctPredictionCnt[predictedValue+1] = correctPredictionCnt[predictedValue+1] + 1;
        }
        totalTypes[predictedValue+1] = totalTypes[predictedValue+1] + 1;
    }

    print(paste("For K:", k, sep=""));
    print(correctPredictionCnt)
    print(totalTypes)

    accuracy = sum(correctPredictionCnt)/sum(totalTypes);
    print(accuracy)
    return (accuracy);
}

args = commandArgs(trailingOnly=TRUE);

if(length(args) != 0){
    K = args[1];
} else {
    K = 15;
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

#### Reading the test data
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

kValues = seq(from=1, to=K, by=2);
print("KValues");
print(kValues);
accuracyVectorTestData = replicate(length(kValues), 0);
accuracyVectorTrainingData = replicate(length(kValues), 0);

for(ki in seq(from=1, length(kValues), by=1)){
    print(paste("(Test set)Computation for k", kValues[ki], sep=":"));
    accuracyVectorTestData[ki] = knnFit(trainingData, testData, kValues[ki]);
    #accuracyVectorTrainingData[ki] = knnFit(trainingData, trainingData, kValues[ki]);
}

for(ki in seq(from=1, length(kValues), by=1)){
    print(paste("(Trainging set) Computation for K", kValues[ki], sep=":"));
    accuracyVectorTrainingData[ki] = knnFit(trainingData, trainingData, kValues[ki]);
}

print("Test accuracy:");
print(accuracyVectorTestData);
testError = 1 - accuracyVectorTestData;

print("Training accuracy");
print(accuracyVectorTrainingData);
trainingError = 1 - accuracyVectorTrainingData;

print(kValues);
plotData = {};
plotData$kValues = kValues;
plotData$testError = testError;
plotData$trainingError = trainingError;
write.csv(plotData, file="knn-errors.csv");

x11();
jpeg("Test errors-Knn");
qplot(x = kValues, y = testError, xlab="K", ylab="Test Error", geom="line");
dev.off();

x11();
jpeg("Training error-Knn");
qplot(x = kValues, y = trainingError, xlab="K values", ylab="Training Error", geom="line")
dev.off();

print("Test error summary:");
summary(testError);

print("Training error summary:");
summary(trainingError);