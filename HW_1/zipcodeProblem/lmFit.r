
# Load the Elem of stat learning library
library("ElemStatLearn");

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

prediction = predict(fit, trainingData);
summary(prediction);
print(prediction);

vec = as.vector(prediction);
xValues = seq(from=1, to=length(vec), by = 1)
plot(xValues, vec)

# find the accuracy