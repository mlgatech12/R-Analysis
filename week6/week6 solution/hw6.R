#clear environment
rm(list = ls())

#set seed
set.seed(1)


#=========================================================================================
#START CODE FOR Q14.1
#=========================================================================================

data = read.table("14.1breast-cancer-wisconsin.dataSummer2018.txt", header = F, sep =',')
head(data)
##       V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11
##1 1000025  5  1  1  1  2  1  3  1   1   2
##2 1002945  5  4  4  5  7 10  3  2   1   2
##3 1015425  3  1  1  1  2  2  3  1   1   2
##4 1016277  6  8  8  1  3  4  3  7   1   2
##5 1017023  4  1  1  3  2  1  3  1   1   2
##6 1017122  8 10 10  8  7 10  9  7   1   4


#Missing Data in the given dataset is marked with ?. Display data points which have ?
missing = which(data$V7 == "?") 
missing
##[1]  24  41 140 146 159 165 236 250 276 293 295 298 316 322 412 618

#calculate mean
mean_val = mean(as.numeric(data[-missing, ]$V7))
mean_val_r = round(meanVal)
mean_val_r
##3

#Use mean value to update the missing values
data_mean = data
data_mean[missing,]$V7 = mean_val_r

#Check that there is no missing data
missing_1 = which(data_mean$V7 == "?") 
missing_1
##integer(0)

#Use regression to impute values for missing data
#Use other variables to predict V7 values
data_new = data[-missing,c(2:10)]
head(data_new)

data_new$V7 <- as.integer(data_new$V7)

model = lm(V7~V2+V3+V4+V5+V6+V8+V9+V10 , data = data_new)
step(model, direction = "backward")

#Using the factors as given by stepwise model 
model_1 = lm(V7~V2+V3+V4+V5+V9 , data = data_new)
summary(model_1)
##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   1.9696     0.1371   14.37  < 2e-16 ***
##  V2            0.0717     0.0345    2.08  0.03823 *  
##  V3            0.1132     0.0600    1.89  0.05963 .  
##V4            0.1193     0.0607    1.97  0.04981 *  
##  V5           -0.0657     0.0367   -1.79  0.07374 .  
##V9            0.1305     0.0357    3.66  0.00028 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##Residual standard error: 1.89 on 677 degrees of freedom
##Multiple R-squared:  0.231,	Adjusted R-squared:  0.225 
##F-statistic: 40.6 on 5 and 677 DF,  p-value: <2e-16


#Calculate cross validation R squared value
library(DAAG)
model_cv = cv.lm(data_new, model_1, m=5)
SST = sum((as.numeric(data[-missing,]$V7) - mean(as.numeric(data[-missing,]$V7)))^2)
R2_cv = 1 - attr(model_cv,"ms")*nrow(data[-missing,])/SST
R2_cv
##0.214


#Calculate V7 misiing values using this model
V7_hat = predict(model_1, newdata = data[missing,])
V7_hat
##24   41  140  146  159  165  236  250  276  293  295  298  316  322  412  618 
##3.92 4.25 2.34 2.58 2.46 2.63 2.84 2.48 2.72 5.64 2.34 3.46 4.31 2.48 2.34 2.34
#Fill missing values
data_temp = data
data_temp[missing,]$V7 = round(V7_hat)

#set the values between ranglr 1 and 10
data_temp$V7[data_temp$V7 > 10] = 10
data_temp$V7[data_temp$V7 < 1] = 1

data_temp[missing,]$V7 
##[1] 4 4 2 3 2 3 3 2 3 6 2 3 4 2 2 2

#Use regression with perturbation to impute missing data
count = nrow(data[missing,])
V7_hat_perturb = rnorm(count, V7_hat, sd(V7_hat))
V7_hat_perturb
##[1] 3.474 3.090 1.473 0.547 3.764 2.850 1.228 3.731 3.546 3.519 2.603 4.052 5.888 2.776 3.740 1.406
round(V7_hat_perturb)
## [1] 3 3 1 1 4 3 1 4 4 4 3 4 6 3 4 1

data_temp_1 = data
data_temp_1[missing,]$V7 = round(V7_hat_perturb)

#Check the qulaity of classification using knn nmodels for 3 data sets create above.
library(kknn)

#Use 70% data for training and 30% for validation
split = sample(1:nrow(data), size = round(nrow(data)*0.7), replace = FALSE)
accuracy = rep(0,20)

#1.Using mean 
data.train = data_mean[split,] #489 observations
data.test = data_mean[-split,] #210 observations

for (k in 1:5) {
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data.train, data.test, k=k)
  
  # Get the prediction class 2 or 4 
  pred <- as.integer(fitted(knn_model)+0.5) 
  
  accuracy[k] = sum(pred == data.test$V11) / nrow(data.test)
}


#2.Using regression 
data.train = data_temp[split,] #489 observations
data.test = data_temp[-split,] #210 observations

for (k in 1:5) {
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data.train, data.test, k=k)
  
  # Get the prediction class 2 or 4 
  pred <- as.integer(fitted(knn_model)+0.5) 
  
  accuracy[k+5] = sum(pred == data.test$V11) / nrow(data.test)
}

#3.Using regression with perturbation
data.train = data_temp_1[split,] #489 observations
data.test = data_temp_1[-split,] #210 observations

for (k in 1:5) {
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data.train, data.test, k=k)
  
  # Get the prediction class 2 or 4 
  pred <- as.integer(fitted(knn_model)+0.5) 
  
  accuracy[k+10] = sum(pred == data.test$V11) / nrow(data.test)
}

#4.Remove rows with missing data
data_temp_2 = data[-missing,]
split = sample(1:nrow(data_temp_2), size = round(nrow(data_temp_2)*0.7), replace = FALSE)
data.train = data_temp_2[split,] #478 observations
data.test = data_temp_2[-split,] #205 observations
for (k in 1:5) {
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data.train, data.test, k=k)
  
  # Get the prediction class 2 or 4 
  pred <- as.integer(fitted(knn_model)+0.5) 
  
  accuracy[k+15] = sum(pred == data.test$V11) / nrow(data.test)
}

accuracy
##[1] 0.957 0.957 0.919 0.919 0.919 0.962 0.962 0.914 0.914 0.914 0.968 0.968 0.928 0.928 0.928 0.951 0.951 0.927 0.927
##[20] 0.917

