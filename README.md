# Insurance Charges with R

In this data mining project, my team and I used R programming to analyze a data set concerning the descriptive variables that relate to one's insurance charges, and using this information, we created a total of 6 different models (5 linear regression and 1 using a regression tree), to see which model would best represent our data. We tested the model's efficiency and performance through multiple statistical measures such as the MSE, MAE, RMSE, and Multiple R^2. Upon completion of this project, we concluded that adding an interaction effect between the "smoker" and "BMI" variable ultimately had the best performance (Model3 = lm(charges~age+sex+children+region+(smoker*bmi))).

The link to the data set that we used can be seen below: https://www.kaggle.com/datasets/teertha/ushealthinsurancedataset
