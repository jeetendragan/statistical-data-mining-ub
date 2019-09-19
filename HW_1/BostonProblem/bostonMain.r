library(MASS);
library(ggplot2);
head(Boston);

# get the feature names
featureNames = names(Boston)[-1];

# format the feature names as '+' separated values
formatedFeatureNames = paste(featureNames, collapse = "+");

# build the linear model formula
linearModelFormula = paste("crim ~ ", formatedFeatureNames, sep="");

fit = lm(formula = linearModelFormula, data = Boston);

fit

## CRime rate boston
summary(Boston$crim);
x11();
#jpeg("Crime rate histogram");
hist(Boston$crim, xlab="Crime", main="Crime rate histogram");
#dev.off();

x11();
xValues = seq(from = 1, to= length(Boston$crim), by=1);
#jpeg("Suburbs");
qplot(xValues, Boston$crim, geom="line", xlab="Suburbs", ylab="Crime rate");
#dev.off();

x11();
#jpeg("Crime Rate");
boxplot(Boston$crim, xlab="Crim Rate", horizontal=TRUE);
#dev.off();

# lower half
sub1 = subset(Boston, crim <=  quantile(Boston$crim, c(0.85)));

# upper half
sub2 = subset(Boston, crim > quantile(Boston$crim, c(0.85)));

summary(sub1);
summary(sub2);

summary(Boston$ptratio);
x11();
#jpeg("Crime rate histogram");
hist(Boston$ptratio, xlab="PtRatio", main="PT Ratio histogram");
#dev.off();

x11();
xValues = seq(from = 1, to= length(Boston$ptratio), by=1);
#jpeg("Suburbs");
qplot(xValues, Boston$ptratio, geom="line", xlab="Suburbs", ylab="PT Ratio");
#dev.off();

x11();
#jpeg("Crime Rate");
boxplot(Boston$ptratio, xlab="Pt Ratio", horizontal=TRUE);

# lower half
sub1 = subset(Boston, ptratio <=  quantile(Boston$ptratio, c(0.85)));

# upper half
sub2 = subset(Boston, ptratio > quantile(Boston$ptratio, c(0.85)));

summary(sub1);
summary(sub2);

summary(Boston$tax);
x11();
#jpeg("Crime rate histogram");
hist(Boston$tax, xlab="Tax", main="Tax histogram");
#dev.off();

x11();
xValues = seq(from = 1, to= length(Boston$tax), by=1);
#jpeg("Suburbs");
qplot(xValues, Boston$tax, geom="line", xlab="Suburbs", ylab="Tax");
#dev.off();

x11();
#jpeg("Crime Rate");
boxplot(Boston$tax, xlab="Tax", horizontal=TRUE);

# lower half
sub1 = subset(Boston, tax <=  quantile(Boston$tax, c(0.85)));

# upper half
sub2 = subset(Boston, tax > quantile(Boston$tax, c(0.85)));

summary(sub1);
summary(sub2);

sub1 = subset(Boston, rm > 7);
print(paste("Suburbs with more than 7 rooms per dwelling:", dim(sub1)[1]));
sub2 = subset(Boston, rm > 8);
print(paste("Suburbs with more than 8 rooms per dwelling: ", dim(sub2)[1]));