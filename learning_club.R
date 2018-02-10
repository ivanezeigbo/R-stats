setwd('~/Downloads/')
datafile1 <- read.csv('approve.csv')
loan1 <- as.data.frame(datafile1$loan_amnt)
employment1 <- as.data.frame(datafile1$emp_length)
state1 <- as.factor(datafile1$addr_state)
dti1 <- as.data.frame(as.integer(datafile1$dti))
success1 <- as.data.frame(matrix('1', length(dti1), 1))
df1 <- as.data.frame(cbind(loan1, employment1, state1, dti1, success1))
df1 <- na.omit(df1) #Leaving  out, specifically, the missing notwot values
datafile2 <- read.csv('reject.csv')
loan2 <- as.data.frame(datafile2$Amount.Requested)
employment2 <- as.data.frame(datafile2$Employment.Length)
state2 <- as.factor(datafile2$State)
dti2 <- as.matrix(datafile2$Debt.To.Income.Ratio)
for (u in 1:length(dti2)){
  dti2[u] = as.integer(unlist(strsplit(dti2[u], split='%', fixed=TRUE))[1])
}
dti2 <- as.data.frame(dti2)
success2 <- as.data.frame(matrix('0', length(dti2), 1))
df2 <- as.data.frame(cbind(loan2, employment2, state2, dti2, success2))
colnames(df1) <- c('Loan Requested', 'Employment Experience', 'State', 'DTI', 'Loaned')
#We divide employment years into 3: A. Less than 1 years, B. Greater than 1 and less than 10 years, and C. Greater than 10 years
#A is given numerical value 1, and B is given numericaal value 2, and C is 3.
emp1 <- as.matrix(df1$`Employment Experience`)
for (m in 1:length(emp1)){
  if (emp1[m] == '< 1 year'){
    emp1[m] = 1
  }
  else if (emp1[m] == '10+ years'){
    emp1[m] = 3
  }
  else{
    emp1[m] = 2
  }
}
df1$`Employment Experience` = emp1
colnames(df2) <- c('Loan Requested', 'Employment Experience', 'State', 'DTI', 'Loaned')
df2 <- na.omit(df2) #Leaving  out, specifically, the missing notwot values
emp2 <- as.matrix(df2$`Employment Experience`)
#We divide employment years into 3: A. Less than 1 years, B. Greater than 1 and less than 10 years, and C. Greater than 10 years

for (n in 1:length(emp2)){
  if (emp2[n] == '< 1 year'){
    emp2[n] = 1
  }
  else if (emp2[n] == '10+ years'){
    emp2[n] = 3
  }
  else{
    emp2[n] = 2
  }
}
df2$`Employment Experience` = emp2
data <- as.data.frame(rbind(df1, df2))
data$State <- as.integer(data$State)
data$Loaned <- as.factor(data$Loaned)
data$DTI <- as.numeric(data$DTI)
set.seed(1234)

#normalize data
normalize<- function(dat){
  return((dat - min(dat))/ (max(dat) - min(dat)))
}

Data <- as.data.frame(data)
Data$DTI <- normalize(Data$DTI)
Data$`Loan Requested` <- normalize(Data$`Loan Requested`)
Data$State <- normalize(Data$State)
#Shuffle rows
ind <- runif(nrow(Data))
Data <- Data[order(ind),]
num = 1:nrow(Data)
sub1 = sample(num, 0.7*nrow(Data))
train <- Data[sub1,] #training set
sub2 = num[! num %in% sub1]
test <- Data[sub2,] #test set
summary(Data, c(1:5))

outcome_train <- train[,5] 
outcome_test <- test[,5]
require(class)
#Calculate kNN
#Define k as 13
knn_pred <- knn(train[,c(1:4)], test[,c(1:4)], outcome_train, k= 13)
Table <- table(outcome_test, knn_pred)

#Check Accuracy in Percentage
Accuracy = (100*sum(outcome_test == knn_pred))/(length(outcome_test))
print(paste('Accuracy:', Accuracy, '%'))
