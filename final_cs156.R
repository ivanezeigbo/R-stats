library(Matching)
library(foreign)
library(rbounds)
library(cluster)
setwd("C:/Users/DELL/Downloads")
x <- read.dta("basic.dta")
#covariates
set.seed(14432)
x <- na.omit(x) #Leaving  out, specifically, the missing notwot values
num = 1:nrow(x)
sub1 = sample(num, 0.5*nrow(x))
train <- x[sub1,] #training set
sub2 = num[! num %in% sub1]
test <- x[sub2,] #test set

treat <- test$anygirls
#Did not pick x$perf because x$female should be more important in my reasoning #x$aauw, x$rtl, x$rgroup, x$statalph
#Xi <- cbind(test$female, test$white, test$repub, test$age, test$srvlng,  test$demvote)
Xi <- cbind(test$female, test$white, test$repub, test$age, I(test$age ^2), test$srvlng, I(test$srvlng^2), test$demvote)
BalanceMat <- cbind(test$female, test$white, test$repub, test$age, I(test$age ^2), test$srvlng, I(test$srvlng^2), test$demvote)
Y <- test$nowtot
genout <- GenMatch(Tr=treat, X = Xi, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=500, max.generations=100, wait.generations=15, cluster = c("localhost", "localhost"))
mout <- Match(Y=Y, Tr=treat, X = Xi, estimand="ATT", Weight.matrix=genout)
summary(mout)
#summary(X)
#Check Balance
mb <- MatchBalance(treat~test$female + test$white + test$repub + test$age + I(test$age ^2) + test$srvlng + I(test$srvlng^2) + test$demvote,
                   match.out=mout, nboots=500)
#Run linear regression
basemod <- lm(nowtot~female + white + repub + age + I(age ^2) + srvlng + I(srvlng^2) + demvote, train)
plot(basemod)#plot regression line


#Root Mean Square
rmse1.0 <- sqrt(mean(residuals(basemod)^2)) #calculate RMSE
rmse1.0
basemod2 <- lm(test$nowtot~test$female + test$white + test$repub + test$age + I(test$age ^2) + test$srvlng + I(test$srvlng^2) + test$demvote, test)
rmse2.0 <- sqrt(mean(residuals(basemod2)^2)) #calculate RMSE
rmse2.0

#Calculating model's accuracy on itself
#2: confidence interval
train_pred1 <- predict(basemod, newdata = train, type = 'response', interval='predict')
accurate11 = 0
misclassified11 = 0
countz = 1
while (countz <= length(train$nowtot)){
  if (!is.na(train_pred1[countz, 2])){
    if ((train_pred1[countz, 3] >= train$nowtot[countz]) && (train_pred1[countz, 2] <= train$nowtot[countz])) {
      accurate11 = accurate11 + 1
    }
    else{
      misclassified11 = misclassified11 + 1
    }
  }
  countz = countz + 1
}
accuracy11 = accurate11/(accurate11 + misclassified11)
print(paste('Accuracy:', accuracy11, 'or,', accuracy11 * 100, '%'))

#Out-of-sample prediction
test_pred1 <- predict(basemod, newdata = test, type = 'response', interval='predict')
accurate12 = 0
misclassified12 = 0
county = 1
while (county <= length(test$nowtot)){
  if (!is.na(test_pred1[county, 2])){
    if ((test_pred1[county, 3] >= test$nowtot[county]) && (test_pred1[county, 2] <= test$nowtot[county])) {
      accurate12 = accurate12 + 1
    }
    else{
      misclassified12 = misclassified12 + 1
    }
  }
  county = county + 1
}
accuracy12 = accurate12/(accurate12 + misclassified12)
print(paste('Accuracy:', accuracy12, 'or,', accuracy12 * 100, '%'))


#Calculating counterfactuals
counter_test = test
counter_test$anygirls = 1- counter_test$anygirls
ctest_pred <- predict(basemod, newdata = counter_test, type = 'response', interval = 'predict')

#Calculation of Gower's Metric - meaurement of dissimilarity along the rows of x
Mat <- cbind(test$nowtot, test$anygirls, test$female, test$white, test$repub, test$age, I(test$age ^2), test$srvlng, I(test$srvlng^2), test$demvote)
Mat2 <- cbind(train$nowtot, train$anygirls, train$female, train$white, train$repub, train$age, I(train$age ^2), train$srvlng, I(train$srvlng^2), train$demvote)
Mat3 <- cbind(ctest_pred[,1],counter_test$anygirls, counter_test$female, counter_test$white, counter_test$repub, counter_test$age, I(counter_test$age ^2), counter_test$srvlng, I(counter_test$srvlng^2), counter_test$demvote)
Mat4 <- rbind(Mat, Mat3)
Mat5 <- rbind(Mat2, Mat3)

daisy.mat <- as.matrix(daisy(Mat, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the test data is", max(daisy.mat)))

daisy.mat2 <- as.matrix(daisy(Mat2, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the train data is", max(daisy.mat2)))

daisy.mat3 <- as.matrix(daisy(Mat3, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the counterfactuals is", max(daisy.mat3)))

daisy.mat4 <- as.matrix(daisy(Mat4, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the test data plus counterfactual is", max(daisy.mat4)))

daisy.mat5 <- as.matrix(daisy(Mat5, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the train data plus counterfactual is", max(daisy.mat5)))


#Compare with Genetic Matching Counterfactuals
gentreat <- test[mout$index.treated,]
gencontl <- test[mout$index.control,]
c_treat <- counter_test[mout$index.treated,]
c_contl <- ctest_pred[mout$index.treated,]
MatchRes <- cbind(gentreat$nowtot, gencontl$nowtot, c_contl)
colnames(MatchRes) <- c('Genetic Matching Treatment', 'Genetic Matching Control', 'Counterfactual Prediction for GenMatch Treat', 'Lower Confidence Interval', 'Upper Confidence Interval')
print(paste('Mean of Genetic Matching Treatment:', mean(gentreat$nowtot)))
print(paste('Mean of Genetic Matching Control:', mean(gencontl$nowtot)))
print(paste('Mean of Predicted Counterfactual for Genetic Matching Treatment:', mean(c_contl[,1])))


#Calculating Counterfactual Accuracy Against the Control Group of GenMatch
accurate= 0
misclassified = 0
count0 = 1
while (count0 <= nrow(MatchRes)){
  if (!is.na(c_contl[count0, 2])){
    if ((c_contl[count0, 3] >= gencontl$nowtot[count0]) && (c_contl[count0, 2] <= gencontl$nowtot[count0])) {
      accurate = accurate + 1
    }
    else{
      misclassified = misclassified + 1
    }
  }
  count0 = count0 + 1
}
accuracy = accurate/(accurate + misclassified)
print(paste('Number of observations that fell into confidence interval:', accuracy, 'or,', accuracy * 100, '%'))

#Download Data and see the prediction and GenMatch control results
write.csv(MatchRes, file = 'PredictedCounterfactuals.csv')



parcoord(BalanceMat, col = c(1, 2), lty = 1, var.label = TRUE)

par(mfrow = c(1,2))
plot(density(rnorm(1000)))
hist(rnorm(1000))
plot(density(test$age[treat==1], bw = 2), lwd = 3, col = "red")
lines(density(test$age[treat==0], bw = 2), lwd = 3, col = "blue")
plot(density(test$demvote[treat==1], bw = 2), lwd = 3, col = "red")
lines(density(test$demvote[treat==0], bw = 2), lwd = 3, col = "blue")
#After
plot(density(test$age[mout$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(test$age[mout$index.control], bw = 2), lwd = 3, col = "blue")
plot(density(test$demvote[mout$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(test$demvote[mout$index.control], bw = 2), lwd = 3, col = "blue")