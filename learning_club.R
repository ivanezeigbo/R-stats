setwd('~/Downloads/')
datafile1 <- read.csv('approve.csv')
loan1 <- as.data.frame(datafile1$loan_amnt)
employment1 <- as.data.frame(datafile1$emp_length)
state1 <- as.factor(datafile1$addr_state)
dti1 <- as.data.frame(datafile1$dti)
success1 <- as.data.frame(matrix(1, length(dti1), 1))
df1 <- as.data.frame(cbind(loan1, employment1, state1, dti1, success1))
df1 <- na.omit(df1) #Leaving  out, specifically, the missing notwot values
datafile2 <- read.csv('reject.csv')
loan2 <- as.data.frame(datafile2$Amount.Requested)
employment2 <- as.data.frame(datafile2$Employment.Length)
state2 <- as.factor(datafile2$State)
dti2 <- as.matrix(datafile2$Debt.To.Income.Ratio)
for (u in 1:length(dti2)){
  dti2[u] = unlist(strsplit(dti2[u], split='%', fixed=TRUE))[1]
}
dti2 <- as.data.frame(dti2)
success2 <- as.data.frame(matrix(0, length(dti2), 1))
df2 <- as.data.frame(cbind(loan2, employment2, state2, dti2, success2))
colnames(df1) <- c('Loan Requested', 'Employment Experience', 'State', 'DTI', 'Loaned')
#We divide employment years into 3: A. Less than 1 years, B. Greater than 1 and less than 10 years, and C. Greater than 10 years
emp1 <- as.matrix(df1$`Employment Experience`)
for (m in 1:length(emp1)){
  if (emp1[m] == '< 1 year'){
    emp1[m] = 'A'
  }
  else if (emp1[m] == '10+ years'){
    emp1[m] = 'C'
  }
  else{
    emp1[m] = 'B'
  }
}
df1$`Employment Experience` = emp1
colnames(df2) <- c('Loan Requested', 'Employment Experience', 'State', 'DTI', 'Loaned')
df2 <- na.omit(df2) #Leaving  out, specifically, the missing notwot values
emp2 <- as.matrix(df2$`Employment Experience`)
#We divide employment years into 3: A. Less than 1 years, B. Greater than 1 and less than 10 years, and C. Greater than 10 years

for (n in 1:length(emp2)){
  if (emp2[n] == '< 1 year'){
    emp2[n] = 'A'
  }
  else if (emp2[n] == '10+ years'){
    emp2[n] = 'C'
  }
  else{
    emp2[n] = 'B'
  }
}
df2$`Employment Experience` = emp2
data <- as.data.frame(rbind(df1, df2))
set.seed(1234)
num = 1:nrow(data)
sub1 = sample(num, 0.5*nrow(data))
train <- data[sub1,] #training set
sub2 = num[! num %in% sub1]
test <- data[sub2,] #test set

comment_fit <- lm(comment ~ total_followers + type + category + month + weekday + paid, family = binomial, data = train)
c_sm = summary(comment_fit)
c_sm
mse1 <- mean(c_sm$residuals^2)
mse1 #print mse

#Calculating model's accuracy on itself for comments
train_pred1 <- predict(comment_fit, newdata = train, type = 'response')
train_data1 <- cbind(train$comment, train_pred1)
View(train_data1)
confint(comment_fit)#its confidence interval
test_pred1 <- predict(comment_fit, newdata = test, type = 'response')
test_data1 <- cbind(test$comment, test_pred1)
View(test_data1)

train_pred2 <- predict(like_fit, newdata = train, type = 'response')
train_data <- cbind(train$like, train_pred2)

