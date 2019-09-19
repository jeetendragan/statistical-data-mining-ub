library(MASS);
head(Boston);

# get the feature names
featureNames = names(Boston)[-1];

# format the feature names as '+' separated values
formatedFeatureNames = paste(featureNames, collapse = "+");

# build the linear model formula
linearModelFormula = paste("crim ~ ", formatedFeatureNames, sep="");

fit = lm(formula = linearModelFormula, data = Boston);

