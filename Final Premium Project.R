rm(list=ls())
Premium = read.csv(file.choose(), header=T, stringsAsFactors = T)
attach(Premium)
head(Premium)
summary(Premium)
any(is.na(Premium))

# categorize the "children" variable
as.factor(children)

# scatter plot of Age vs. Charges
plot(age , charges, col ="blue", xlab="Age",ylab ="Charges ",xlim=c(18,65), ylim=c(0,65000), main="Age vs. Charges")

# scatter plot of Age vs. Charges, color is "smoker" variable
plot(age , charges, col =smoker, xlab="Age",ylab ="Charges ",xlim=c(18,65), ylim=c(0,65000), main="Age vs. Charges")

# histogram of charges
hist(charges, breaks =10, col ="red", xlab ="Charges", xlim=c(1000, 70000),main="Histogram of Charges")

# boxplot of Ages per Region
boxplot(age ~ region, data=Premium, col="orange", main="Ages per Region")

# pie chart showing gender distribution
gender_count <- table(sex)
pie(gender_count,main = "Gender Distribution", col = c("pink", "skyblue"), labels = paste0(names(gender_count), " (", gender_count, ")"),cex = 1,radius = 1)

Premium_df <- Premium
Premium_df$sex <- as.numeric(Premium_df$sex)
Premium_df$smoker <- as.numeric(Premium_df$smoker)
Premium_df$region <- as.numeric(Premium_df$region)

# Calculate the correlation matrix
correlation_matrix <- cor(Premium_df)
correlation_matrix

set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(Premium), 0.8 * nrow(Premium))
train_data <- Premium[train_indices, ]
test_data <- Premium[-train_indices, ]

# Creating our Models

Model1= lm(charges~age+sex+bmi+children+smoker+region,data=train_data)
summary(Model1)

Model2= lm(charges~age+sex+bmi+children+smoker,data= train_data)
summary(Model2)

Model3= lm(charges~age+sex+children+region+(smoker*bmi),data=train_data)
summary(Model3)

Model4= lm(charges~age+sex+children+(bmi*smoker),data=train_data)
summary(Model4)

Model5= lm(charges~age+children+(bmi*smoker),data=train_data)
summary(Model5)

# Regression Tree
set.seed(3)
train= sample(1:nrow(Premium), nrow(Premium)/2)
tree.Premium = tree(charges~.,Premium,subset=train)

cv.Premium=cv.tree(tree.Premium, K=10)
cv.Premium

prune.Premium=prune.tree(tree.Premium,best=2)
plot(prune.Premium)
text(prune.Premium,pretty=0)

Premium.test = Premium[-train,"charges"]
Premium.pred = predict(prune.Premium, newdata=Premium[-train])

# find the mean squared error of the regression tree
TreeMSE <- mean((Premium.pred-Premium.test)^2)

# Training and testing our models
predictions1 <- predict(Model1, newdata = test_data)
predictions2 <- predict(Model2, newdata = test_data)
predictions3 <- predict(Model3, newdata = test_data)
predictions4 <- predict(Model4, newdata = test_data)
predictions5 <- predict(Model5, newdata = test_data)

plot(test_data$charges, predictions1, main = "Model 1 Actual vs Predicted", xlab = "Actual", ylab = "Predicted") 
abline(0, 1, col = "red")  # Adds a diagonal line for comparison

plot(test_data$charges, predictions2, main = "Model 2 Actual vs Predicted", xlab = "Actual", ylab = "Predicted") 
abline(0, 1, col = "red")  # Adds a diagonal line for comparison

plot(test_data$charges, predictions3, main = "Model 3 Actual vs Predicted", xlab = "Actual", ylab = "Predicted") 
abline(0, 1, col = "red")  # Adds a diagonal line for comparison

plot(test_data$charges, predictions4, main = "Model 4 Actual vs Predicted", xlab = "Actual", ylab = "Predicted") 
abline(0, 1, col = "red")  # Adds a diagonal line for comparison

plot(test_data$charges, predictions5, main = "Model 5 Actual vs Predicted", xlab = "Actual", ylab = "Predicted") 
abline(0, 1, col = "red")  # Adds a diagonal line for comparison

## Evaluating our Models
# 5-Fold Cross Validation
set.seed(1)
k=5
M1CVMSE=rep(0,k)
M2CVMSE=rep(0,k)
M3CVMSE=rep(0,k)
M4CVMSE=rep(0,k)
M5CVMSE=rep(0,k)

folds=sample(1:k,nrow(test_data),replace=TRUE)
# Model 1
for(j in 1:k)
{
  M1CV=lm(charges~age+sex+bmi+children+smoker+region,data=test_data[folds!=j,])
  M1CVMSE [j]=mean((test_data$charges-predict(M1CV,test_data))[folds==j]^2)
}

# Model 2
for(j in 1:k)
{
  M2CV=lm(charges~age+sex+bmi+children+smoker,data=test_data[folds!=j,])
  M2CVMSE [j]=mean((test_data$charges-predict(M2CV,test_data))[folds==j]^2)
}

# Model 3
for(j in 1:k)
{
  M3CV=lm(charges~age+sex+children+region+(smoker*bmi),data=test_data[folds!=j,])
  M3CVMSE [j]=mean((test_data$charges-predict(M3CV,test_data))[folds==j]^2)
}

# Model 4
for(j in 1:k)
{
  M4CV=lm(charges~age+sex+children+(bmi*smoker),data=test_data[folds!=j,])
  M4CVMSE [j]=mean((test_data$charges-predict(M4CV,test_data))[folds==j]^2)
}

# Model 5
for(j in 1:k)
{
  M5CV=lm(charges~age+children+(bmi*smoker),data=test_data[folds!=j,])
  M5CVMSE [j]=mean((test_data$charges-predict(M5CV,test_data))[folds==j]^2)
}
MeanM1MSE=mean(M1CVMSE) ###CVMSE-M1###
MeanM2MSE=mean(M2CVMSE) ###CVMSE-M2###
MeanM3MSE=mean(M3CVMSE) ###CVMSE-M3###
MeanM4MSE=mean(M4CVMSE) ###CVMSE-M4###
MeanM5MSE=mean(M5CVMSE) ###CVMSE-M4###

# Mean Absolute Error (MAE)
mae1 <- mean(abs(test_data$charges - predictions1))
print(paste("Model 1 Mean Absolute Error: ", mae1))

mae2 <- mean(abs(test_data$charges - predictions2))
print(paste("Model 2 Mean Absolute Error: ", mae2))

mae3 <- mean(abs(test_data$charges - predictions3))
print(paste("Model 3 Mean Absolute Error: ", mae3))

mae4 <- mean(abs(test_data$charges - predictions4))
print(paste("Model 4 Mean Absolute Error: ", mae4))

mae5 <- mean(abs(test_data$charges - predictions5))
print(paste("Model 5 Mean Absolute Error: ", mae5))

# Root Mean Squared Error (RMSE)
rmse1 <- sqrt(mean((test_data$charges - predictions1)^2))
print(paste("Model 1 Root Mean Squared Error: ", rmse1))

rmse2 <- sqrt(mean((test_data$charges - predictions2)^2))
print(paste("Model 2 Root Mean Squared Error: ", rmse2))

rmse3 <- sqrt(mean((test_data$charges - predictions3)^2))
print(paste("Model 3 Root Mean Squared Error: ", rmse3))

rmse4 <- sqrt(mean((test_data$charges - predictions4)^2))
print(paste("Model 4 Root Mean Squared Error: ", rmse4))

rmse5 <- sqrt(mean((test_data$charges - predictions5)^2))
print(paste("Model 5 Root Mean Squared Error: ", rmse5))

# Residual plots for Model 1
plot(predict(Model1), residuals(Model1))
plot(predict(Model1), rstudent(Model1))

# Residual plots for Model 2
plot(predict(Model2), residuals(Model2))
plot(predict(Model2), rstudent(Model2))

# Residual plots for Model 3
plot(predict(Model3), residuals(Model3))
plot(predict(Model3), rstudent(Model3))

# Residual plots for Model 4
plot(predict(Model4), residuals(Model4))
plot(predict(Model4), rstudent(Model4))

# Residual plots for Model 5
plot(predict(Model5), residuals(Model5))
plot(predict(Model5), rstudent(Model5))

