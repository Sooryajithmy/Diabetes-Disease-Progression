#######STATISTICAL REGRESSION ANALYSYIS ON DIABETES DISEASE PROGRESSION########

#Note: Box plot, Histogram, Correlation Matrix, Correlation Scatter plot were plotted using JMP. 

########installing necessary libraries#######
install.packages("psych")
install.packages("caTools")
install.packages("car")
install.packages("MASS")

########reading the data and variables#######
diabetesdata=read.csv("1 SP Jain/Semester 2/Statistical data analysis/Diabetes.csv")

########key statistics of the data set#######
head(diabetesdata)
library("psych") #for the function 'describe'
describe(diabetesdata)
summary(diabetesdata)
attach(diabetesdata)

########train-test split#######
library(caTools)
set.seed(53)
split=sample.split(Y,SplitRatio = 0.80)# training data = 80%, testing data = 20%
train_data<-subset(diabetesdata,split==T)
test_data<-subset(diabetesdata,split==F)

########building the model using all independent variables#######
model1 = lm(Y~.,train_data) 
model1 #calling the model
summary(model1)

########plotting#######
plot(model1)# 4 plots are obtained

########conducting ncvTest to know the statistic values#######
library(car)
ncvTest(model1) 

########checking the multicollinearity#######
vif(model1)#from library 'car'

########solving multicollinearity#######
library(MASS)
model2=stepAIC(model1,direction = 'both')

########conducting vif for model2####### 
library(car)
vif(model2)

########model2#######
summary(model2)

########conducting nvctest#######
library(car)
ncvTest(model2)

########plotting#######
plot(model2)#4 plots are obtained

########building Anova table#######
anova(model2)

########Evaluating the model using Evaluation metrics#######
pred = predict(model2) # predictions of Y variable in train data
res= residuals(model2) # residuals of train data
mse = mean(res^2) #mean squared error
mse
rmse = sqrt(mse) #root mean squared error
rmse
plot(train_data$Y,pred)#plotting Y vs predicted Y
abline(a=0, b=1)


########Influence index plot#######
infIndexPlot(model2, vars=c("Cook", "Studentized", "Bonf", "hat"),id=TRUE, grid=TRUE, main="Diagnostic Plots")

########Evaluating using test data#######
p = predict(model2,test_data) # predictions of Y variable in test data
r = r = abs(test_data$Y-p) #residuals in test data
mse_test = mean(r^2) 
mse_test
rmse_test = sqrt(mean(r^2))
rmse_test
plot(test_data$Y,p)#plotting Y vs predicted Y of test data
abline(a=0, b=1)


#########################THE END#################################

