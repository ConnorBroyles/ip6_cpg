# Importing the dataset
rm(list = ls())
ds=read.csv('hd.csv')

#Displaying the count of null values per column
colSums(is.na(ds))
plot(ds$biking)
plot(ds$smoking)

# Missing data
#na. rm = TRUE to exclude missing values
ds$biking[is.na(ds$biking)]<-mean(ds$biking,na.rm=TRUE)
ds$smoking[is.na(ds$smoking)]<-mean(ds$smoking,na.rm=TRUE)
ds$heart.disease[is.na(ds$heart.disease)]<-mean(ds$heart.disease,na.rm=TRUE)
#Create multiple copies of the dataset with no missing data
ds1<-ds
ds2<-ds
ds3<-ds
ds4<-ds
##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split=sample.split(ds$heart.disease,SplitRatio=0.8)
training_set=subset(ds,split==TRUE)
testing_set=subset(ds,split==FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor_MLR<-lm(formula=heart.disease~.,data=training_set)


# Predicting the Validation set results
y_pred=predict(regressor_MLR,newdata=testing_set)
#new <- data.frame( )
new<-data.frame(biking=45.0972,smoking=21.38562)
new1<-data.frame(biking=8.279743,smoking=6.42372)
new2<-data.frame(biking=42.34586,smoking=20.74133)
new3<-data.frame(biking=30.77425,smoking=23.61017)
#predict(regressor, newdata = new)
predict(regressor_MLR,newdata=new)
predict(regressor_MLR,newdata=new1)
predict(regressor_MLR,newdata=new2)
predict(regressor_MLR,newdata=new3)
#RMSE
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set
regressor_SVR=svm(formula=heart.disease~.,data=training_set,type='eps-regression',kernel='radial')
# Fitting SVR to the dataset
library(e1071)

# Predicting the Validation set results
y_pred=predict(regressor_SVR,newdata=testing_set)
new<-data.frame(biking=45.0972,smoking=21.38562)
new1<-data.frame(biking=8.279743,smoking=6.42372)
new2<-data.frame(biking=42.34586,smoking=20.74133)
new3<-data.frame(biking=30.77425,smoking=23.61017)
#new <- data.frame( )
predict(regressor_SVR,newdata=new)
predict(regressor_SVR,newdata=new1)
predict(regressor_SVR,newdata=new2)
predict(regressor_SVR,newdata=new3)
#RMSE
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

#########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(rpart)
regressor_DT=rpart(formula=heart.disease~.,data=training_set)
# Predicting the Validation set results
new<-data.frame(biking=45.0972,smoking=21.38562)
new1<-data.frame(biking=8.279743,smoking=6.42372)
new2<-data.frame(biking=42.34586,smoking=20.74133)
new3<-data.frame(biking=30.77425,smoking=23.61017)
#new <- data.frame( )
predict(regressor_DT,newdata=new)
predict(regressor_DT,newdata=new1)
predict(regressor_DT,newdata=new2)
predict(regressor_DT,newdata=new3)
#RMSE
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(randomForest)
set.seed(1234)
regressor_RF=randomForest(x=training_set[,1:2],y=training_set$heart.disease,ntree=20)
y_pred=predict(regressor_RF,newdata=testing_set)
# Predicting the Validation set results
new<-data.frame(biking=45.0972,smoking=21.38562)
new1<-data.frame(biking=8.279743,smoking=6.42372)
new2<-data.frame(biking=42.34586,smoking=20.74133)
new3<-data.frame(biking=30.77425,smoking=23.61017)
#new <- data.frame( )
predict(regressor_RF,newdata=new)
predict(regressor_RF,newdata=new1)
predict(regressor_RF,newdata=new2)
predict(regressor_RF,newdata=new3)
#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
