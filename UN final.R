library(foreign)
library(ResourceSelection)
library(ROCR)
library(cluster)
library(tree)
library(randomForest)
library(rpart)
setwd("C:/Users/DELL/Downloads")
datafile <- read.dta("peace.dta")
set.seed(1234)
print(paste("Number of 1's in pbs2s3 is ", length(datafile$pbs2s3[which(datafile$pbs2s3 == 1)]), "and number of 0's is", length(datafile$pbs2s3[which(datafile$pbs2s3 == 0)])))
print(paste("Number of 1's in un2int is ", length(datafile$un2int[which(datafile$un2int == 1)]), "and number of 0's is", length(datafile$un2int[which(datafile$un2int == 0)])))

num = 1:nrow(datafile)
sub1 = sample(num, 0.5*nrow(datafile))
train <- datafile[sub1,] #training set
sub2 = num[! num %in% sub1]
test <- datafile[sub2,] #test set
fit <- glm(pbs2s3 ~ un2int + wartype + wardur + exp + logdead + factnum + log(trnsfcap) + develop + treaty + untype4 + wardur*untype4, family = binomial, data = train)
#summary(fit)

#Decision tree has the 'd_' prefix
#Decision tree fit
d_fit <- rpart(pbs2s3 ~ un2int + wartype + wardur + exp + logdead + factnum + trnsfcap + develop + treaty + untype4, data = train, method = 'class')
#summary(d_fit)

anova(fit, test="Chisq")
dat <- cbind(train$pbs2s3, train$un2int, train$wartype, train$wardur, train$exp, train$logdead, train$factnum, train$trnsfcap, train$develop, train$treaty, train$untype4) #creating matrix containing explanatory variables used in logistic regression
#Calculating model's accuracy on itself
train_pred <- predict(fit, newdata = train, type = 'response')
d_train_pred <- predict(d_fit, train)
accurate1 = 0
d_accurate1 = 0
misclassified1 = 0
d_misclassified1 = 0
count = 1
while (count <= length(train$pbs2s3)){
  if (!is.na(train_pred[[count]])){
    if ((train_pred[[count]] > 0.5) && (train$pbs2s3[count] ==1)) {
      accurate1 = accurate1 + 1
    }
    else if ((train_pred[[count]] <= 0.5) && (train$pbs2s3[count] ==0)){
      accurate1 = accurate1 + 1
    }
    else{
      misclassified1 = misclassified1 + 1
    }
  }
  if ((d_train_pred[[count, 2]] > d_train_pred[[count, 1]]) && (train$pbs2s3[count] ==1)) {
    d_accurate1 = d_accurate1 + 1
  }
  else if ((d_train_pred[[count, 2]] <= d_train_pred[[count, 1]]) && (train$pbs2s3[count] ==0)){
    d_accurate1 = d_accurate1 + 1
  }
  else{
    d_misclassified1 = d_misclassified1 + 1
  }
  count = count + 1
}
accuracy1 = accurate1/(accurate1 + misclassified1)
print(paste('Accuracy:', accuracy1, 'or,', accuracy1 * 100, '%'))
#confint(fit)#its confidence interval

d_accuracy1 = d_accurate1/(d_accurate1 + d_misclassified1)
print(paste('Accuracy of decision tree:', d_accuracy1, 'or,', d_accuracy1 * 100, '%'))

#Calculating model's accuracy on test set
test_pred <- predict(fit, newdata = test, type = 'response')
d_test_pred <- predict(d_fit, test)
accurate2 = 0
d_accurate2 = 0
misclassified2 = 0
d_misclassified2 = 0
count2 = 1
while (count2 <= length(test$pbs2s3)){
  if (!is.na(test_pred[[count2]])){
    if ((test_pred[[count2]] > 0.5) && (test$pbs2s3[count2] ==1)) {
      accurate2 = accurate2 + 1
    }
    else if ((test_pred[[count2]] <= 0.5) && (test$pbs2s3[count2] ==0)){
      accurate2 = accurate2 + 1
    }
    else{
      misclassified2 = misclassified2 + 1
    }
  }
  if ((d_test_pred[[count2, 2]] > d_test_pred[[count2, 1]]) && (test$pbs2s3[count2] ==1)) {
    d_accurate2 = d_accurate2 + 1
  }
  else if ((d_test_pred[[count2, 2]] <= d_test_pred[[count2, 1]]) && (test$pbs2s3[count2] ==0)){
    d_accurate2 = d_accurate2 + 1
  }
  else{
    d_misclassified2 = d_misclassified2 + 1
  }
  count2 = count2 + 1
}
accuracy2 = accurate2/(accurate2 + misclassified2)
print(paste('Accuracy:', accuracy2, 'or,', accuracy2 * 100, '%'))
#plot(fit)

d_accuracy2 = d_accurate2/(d_accurate2 + d_misclassified2)
print(paste('Accuracy:', d_accuracy2, 'or,', d_accuracy2 * 100, '%'))


#Measurement of Performace using ROC curves and the AUC
pr <- prediction(test_pred, test$pbs2s3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")





qrf <- performance(pr, measure = "tnr", x.measure = "tpr") #Specificity against Sensitivity
frf <- performance(pr, measure = 'fpr', x.measure = 'fnr')
wrf <- performance(pr, measure = "tpr", x.measure = "acc")
rrf <- performance(pr, measure = "tnr", x.measure = "acc")
plot(qrf)
plot(prf)
plot(frf)
plot(wrf)
plot(rrf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('Area Under the Curve is', auc))

#Calculating the Hosmer-Lemeshow Goodness of Fit

bf = fitted(fit) #calculates fitness of model
indx = as.numeric(names(bf))
sub11 = sub1[sub1 %in% indx]#making sure they have the same length with the fitted(fit)
train1 = datafile[sub11,]
hoslem.test(train1$pbs2s3, bf)

#Root Mean Squared Error Calculation
rmse1.0 <- sqrt(mean(residuals(fit)^2)) #calculate RMSE
rmse1.0
fit2 <- glm(pbs2s3 ~ un2int + wartype + wardur + exp + logdead + factnum + log(trnsfcap) + develop + treaty + untype4 + wardur*untype4, family = binomial, data = test)
rmse2.0 <- sqrt(mean(residuals(fit2)^2)) #calculate RMSE
rmse2.0

#Calculation of Gower's Metric - meaurement of dissimilarity along the rows of x
daisy.mat <- as.matrix(daisy(dat, metric="gower"))
print(paste("Maximum Gower Metric (G^2) in the train data is", max(daisy.mat)))

#Prediction for 1 - UNOP4 or 1 - untype4 which is the counterfactual for if there was a multidimensional type of UN peace operation
#Here I want to see what would happen if the counterfactual of untype4 was the case. So I assume that the predictors are independent and that no other predictor is correlated with untype4
#Then if my model has strong predictive power, then I can actually try and predict relatively accurately whether there would be a UN intervention success in the test set if there in fact was no multidimensional UN operation
#In doing this, I'll rather use my counterfactual as the untype4 for the test set and calculate what my model gives me as the pbs2s3 for the test set with untype4 counterfactual
counter_test = test
counter_test$untype4 = 1- counter_test$untype4
ctest_pred <- predict(fit, newdata = counter_test, type = 'response', interval = 'predict')
nochange = 0
change = 0
count3 = 1
g = 0 #predicted successes for counterfactual
l = 0 #actual successes for 'factual'
while (count3 <= length(test$pbs2s3)){
  if (!is.na(ctest_pred[[count3]])){
    if (ctest_pred[[count3]] > 0.5){
      g = g + 1
    }
    if (test$pbs2s3[count3] ==1){
      l = l + 1
    }
    if ((ctest_pred[[count3]] > 0.5) && (test$pbs2s3[count3] ==1)) {
      nochange = nochange + 1
    }
    else if ((ctest_pred[[count3]] <= 0.5) && (test$pbs2s3[count3] ==0)){
      nochange = nochange + 1
    }
    else{
      change = change + 1
    }
  }
  count3 = count3 + 1
}
change_percent = change/(change + nochange)
print(paste('Proportion of changes in final outcome:', change_percent, 'or,', change_percent * 100, '%'))
print(paste('Proportion of no changes in final outcome:', 1 - change_percent, 'or,', (100 - change_percent * 100), '%'))
print(paste("Number of predicted successes for counterfactual is", g, "while number of actual successes for 'factual' is", l))


#Regression Tree Diagram for Test Set Prediction
# create an indicator variable for "success" of UN intervention 
# and append it to datafile dataframe

datafile2 <- datafile
yes <- which(datafile2$pbs2s3 == 1)
no <- which(datafile2$pbs2s3 == 0)
successYES <- rep(0, nrow(datafile2))
successYES[yes] <- "Yes"
successYES[no] <- "No"
datafile2 <- data.frame(successYES, datafile2)

#Have to take out interaction term since regression trees cannot handle interaction terms

regtree = tree(successYES ~ un2int + wartype + wardur + exp + logdead + factnum + log(trnsfcap) + develop + treaty + untype4, data = datafile2)
# create a figure
par(mfrow = c(1,1))
plot(regtree)
text(regtree, pretty = 0)

# perform training set/test set analysis
traintree <- datafile2[sub1,] #training set for regression tree
testtree <- datafile2[sub2,] #test set for regression tree


tree.train = tree(successYES ~ un2int + wartype + wardur + exp + logdead + factnum + log(trnsfcap) + develop + treaty + untype4, data = datafile2, subset = sub1)

maze.pred=predict(tree.train, testtree, type = "class")

table(maze.pred, testtree$successYES)

# create a figure
plot(tree.train)
text(tree.train, pretty = 0)

#Cross Validation using Regression trees
cv.datafile2 =cv.tree(tree.train ,FUN=prune.misclass )
cv.datafile2
par(mfrow=c(1,2))
plot(cv.datafile2$size ,cv.datafile2$dev ,type="b")
plot(cv.datafile2$k ,cv.datafile2$dev ,type="b")

prune.datafile2 <- prune.misclass(tree.train, best=5)
par(mfrow = c(1,1))
plot(prune.datafile2 )
text(prune.datafile2, pretty =0)
tree.pred <- predict(prune.datafile2, testtree , type="class")
table(tree.pred, testtree$successYES)
