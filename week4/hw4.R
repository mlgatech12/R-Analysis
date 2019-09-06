#clear environment
rm(list = ls())

#set seed
seed(1)


#=========================================================================================
#START CODE FOR Q9.1
#=========================================================================================
data = read.table("9.1uscrimeSummer2018.txt", header = T, sep ='\t')
head(data)

pca = prcomp(data[, 1:15], scale. = TRUE)
summary(pca)

pca$x

screeplot(pca, type="lines")

#Retrieve first 4 principal components to build linear regression model
pc_data = cbind(pca$x[,1:4] , data[,16])
pc_data

#convert it into datframe
pc_df = as.data.frame(pc_data)
pc_df

#Build linear regression model
lmodel_1 = lm(pc_df[,5] ~. ,  data= pc_df[,1:4])
summary(lmodel_1)

##Call:
##lm(formula = pc_df[, 5] ~ ., data = pc_df[, 1:4])
##
##Residuals:
##  Min     1Q Median     3Q    Max 
##-557.8 -210.9  -29.1  197.3  810.3 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    905.1       49.1   18.44   <2e-16 ***
##  PC1             65.2       20.2    3.23   0.0024 ** 
##  PC2            -70.1       29.6   -2.36   0.0227 *  
##  PC3             25.2       35.0    0.72   0.4760    
##PC4             69.4       46.0    1.51   0.1387    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##Residual standard error: 336 on 42 degrees of freedom
##Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 
##F-statistic:  4.7 on 4 and 42 DF,  p-value: 0.00318

length(lmodel_1$coefficients)
lmodel_1$coefficients[2:5]
##  PC1   PC2   PC3   PC4 
##65.2 -70.1  25.2  69.4



#Get the original factors coefficients from PC coefficients
coef =  lmodel_1$coefficients[2:5] %*% t(pca$rotation[, 1:4])
coef
##         M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
##[1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61


#scaled * ( x - mean) / sd = unscaled * x
#Get unscaled coefficients
origCoef = coef/pca$scale
origCoef
##         M   So   Ed  Po1  Po2   LF   M.F  Pop  NW    U1   U2 Wealth Ineq Prob   Time
##[1,] -16.9 21.3 12.8 21.4 23.1 -347 -8.29 1.05 1.5 -1510 1.69   0.04 -6.9  145 -0.933

#Get intercept
intercept = lmodel_1$coefficients[1] - sum(coef * sapply(data[,1:15], mean)/sapply(data[, 1:15],sd))
intercept
##1666

#estimate for new data point
prediction = origCoef[1]*14 + origCoef[2]*0 + origCoef[3]*10 + origCoef[4]*12.0 + origCoef[5]*15.5 + origCoef[6]*0.64 +origCoef[7]*94.0 +
  origCoef[8]*150 + origCoef[9]*1.1 + origCoef[10]*0.12+ origCoef[11]*3.6 + origCoef[12]*3200+ origCoef[13]*20.1 + origCoef[14]*0.04 + origCoef[15]*39 + intercept
prediction
##1113

#Compare estimates and actuals
estimates = as.matrix(data[,1:15]) %*% t(origCoef) + intercept
estimates

#Calculate R^2 and adjusted R^2
#SSE = sum((yhat -crime)^2)
#SS_Tot = sum ((crime - mean(crime))^2)
#R^2 = 1- SSE/SS_Tot
#R^2 adj = R2 - (1 - R2)* num_PCS / (nrow(dat) - num_PCS - 1)

SSE = sum((estimates - data[,16]) ^2)
SS_Tot = sum((data[,16] - mean(data[,16]))^2)
R2 = 1 - SSE/SS_Tot
R2
##0.309

R2_adj = R2 - (1 -R2) * 4 / (nrow(data) - 4 -1)
R2_adj
##0.243



#Build regression model with choosing 3 to 15 Principal components
r2 = numeric(12)
r2a = numeric(12)
r2_cv = numeric(12)
ct = 1

library(DAAG)

for (i in 3:15) {
  #Retrieve first i principal components to build linear regression model
  pc_data = cbind( data[,16] , pca$x[,1:i] )
  
  #convert it into datframe
  pc_df = as.data.frame(pc_data)
  pc_df
  
  pc_df[,1:i]
  
  #Build linear regression model
  lmodel = lm(V1 ~ . ,  data = pc_df[,1:i])
  
  model1_cv = cv.lm(pc_df, lmodel, m =5)
  
  #Calculate R^2 for cross validation 
  SS_total = sum((data$Crime - mean(data$Crime))^2) #total of sum of squared differences between data and mean
  SSres_cv = attr(model1_cv, "ms") * nrow(data)
  r2_cv[ct] =  1 - (SSres_cv/SS_total)
 
  
 
  r2[ct] = summary(lmodel)$r.squared
  r2a[ct] = summary(lmodel)$adj.r.squared
  ct = ct + 1
}

r2
##[1] 0.263 0.272 0.309 0.645 0.659 0.688 0.690 0.692 0.696 0.697 0.769 0.772 0.791

r2a
##[1] 0.230 0.221 0.243 0.602 0.607 0.632 0.625 0.617 0.612 0.602 0.688 0.683 0.700

r2_cv
##[1] 0.0910 0.0666 0.1057 0.4872 0.4628 0.4562 0.3664 0.3337 0.2954 0.1863 0.3897 0.3902 0.4736


#Create another model using 6 principal components
#Retrieve first 4 principal components to build linear regression model
pc_data = cbind(pca$x[,1:6] , data[,16])
pc_data

#convert it into datframe
pc_df = as.data.frame(pc_data)
pc_df

#Build linear regression model
lmodel_1 = lm(pc_df[,7] ~. ,  data= pc_df[,1:6])
summary(lmodel_1)

##Call:
##lm(formula = pc_df[, 7] ~ ., data = pc_df[, 1:6])

##Residuals:
##  Min     1Q Median     3Q    Max 
##-377.1 -172.2   25.8  132.1  480.4 

##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    905.1       35.3   25.60  < 2e-16 ***
##  PC1             65.2       14.6    4.48  6.1e-05 ***
##  PC2            -70.1       21.3   -3.28   0.0021 ** 
##  PC3             25.2       25.2    1.00   0.3241    
##PC4             69.4       33.1    2.10   0.0425 *  
##  PC5           -229.0       36.5   -6.28  1.9e-07 ***
##  PC6            -60.2       48.0   -1.25   0.2173    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 242 on 40 degrees of freedom
##Multiple R-squared:  0.659,	Adjusted R-squared:  0.607 
##F-statistic: 12.9 on 6 and 40 DF,  p-value: 4.87e-08


length(lmodel_1$coefficients)
lmodel_1$coefficients[2:7]
##PC1    PC2    PC3    PC4    PC5    PC6 
##65.2  -70.1   25.2   69.4 -229.0  -60.2



#Get the original factors coefficients from PC coefficients
coef =  lmodel_1$coefficients[2:7] %*% t(pca$rotation[, 1:6])
coef
##        M   So   Ed Po1 Po2   LF M.F  Pop NW   U1   U2 Wealth Ineq  Prob Time
##[1,] 87.8 43.9 20.5 123 119 45.9 113 25.9 95 1.82 29.4   45.2 5.72 -51.7 36.1


#scaled * ( x - mean) / sd = unscaled * x
#Get unscaled coefficients
origCoef = coef/pca$scale
origCoef
##        M   So   Ed  Po1  Po2   LF  M.F   Pop   NW  U1   U2 Wealth Ineq  Prob Time
##[1,] 69.9 91.7 18.3 41.4 42.4 1136 38.2 0.681 9.24 101 34.9 0.0469 1.43 -2274  5.1


#Get intercept
intercept = lmodel_1$coefficients[1] - sum(coef * sapply(data[,1:15], mean)/sapply(data[, 1:15],sd))
intercept
##-5924

prediction = origCoef[1]*14 + origCoef[2]*0 + origCoef[3]*10 + origCoef[4]*12.0 + origCoef[5]*15.5 + origCoef[6]*0.64 +origCoef[7]*94.0 +
  origCoef[8]*150 + origCoef[9]*1.1 + origCoef[10]*0.12+ origCoef[11]*3.6 + origCoef[12]*3200+ origCoef[13]*20.1 + origCoef[14]*0.04 + origCoef[15]*39 + intercept
prediction
##1248


#=========================================================================================
#START CODE FOR Q10.1
#=========================================================================================
library (tree)

treeModel = tree(Crime ~. , data = data) 
summary(treeModel)
##Regression tree:
##tree(formula = data$Crime ~ ., data = data)
##Variables actually used in tree construction:
##  [1] "Po1" "Pop" "LF"  "NW" 
##Number of terminal nodes:  7 
##Residual mean deviance:  47400 = 1900000 / 40 
##Distribution of residuals:
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##-574     -98      -2       0     111     490 

#Create data point
data_1 = data.frame(M=14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, 
                    NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

treeModel$frame
##var  n     dev yval splits.cutleft splits.cutright
##1     Po1 47 6880928  905          <7.65           >7.65
##2     Pop 23  779243  670          <22.5           >22.5
##4      LF 12  243811  550        <0.5675         >0.5675
##8  <leaf>  7   48519  467                               
##9  <leaf>  5   77757  668                               
##5  <leaf> 11  179471  800                               
##3      NW 24 3604162 1131          <7.65           >7.65
##6     Pop 10  557575  887          <21.5           >21.5
##12 <leaf>  5  146391 1049                               
##13 <leaf>  5  147771  725                               
##7     Po1 14 2027225 1305          <9.65           >9.65
##14 <leaf>  6  170828 1041                               
##15 <leaf>  8 1124985 1503                               

plot(treeModel)
text(treeModel)

#Calculate R^2
yhat = predict(treeModel)
SSres = sum((yhat-data$Crime)^2)
SStot = sum((data$Crime - mean(data$Crime))^2)
R2 = 1 - SSres/SStot
R2
##0.724



#predict data using cross validation tree model
prediction_1 = predict(treeModel, data_1)
prediction_1
##725

#Use cross validation
cv_tree_model  = cv.tree(treeModel)
plot(cv_tree_model$size, cv_tree_model$dev, type = "b")

cv_tree_model$size
##[1] 7 6 5 4 3 2 1
cv_tree_model$dev
##[1] 7841742 7916728 7834871 7956214 8248167 7727887 7741861


#Consider pruning the tree
k = 5
prune_tree = prune.tree(treeModel, best = k)
prune_tree

yhat = predict(prune_tree)
SSres = sum((yhat-data$Crime)^2)
SStot = sum((data$Crime - mean(data$Crime))^2)
R2 = 1 - SSres/SStot
R2
##0.669


#Plot pruned tree
plot(prune_tree)
text(prune_tree)



#predict data pruned tree 
prediction_3 = predict( prune_tree, data_1)
prediction_3
##887

library(randomForest)

forestModel = randomForest(data$Crime ~ ., data = data , mtry = 4 , importance = TRUE)
forestModel
##Call:
##randomForest(formula = data$Crime ~ ., data = data, mtry = 4,      importance = TRUE) 
##Type of random forest: regression
##Number of trees: 500
##No. of variables tried at each split: 4
##
##Mean of squared residuals: 84579
##% Var explained: 42.2


importance(forestModel)
##       %IncMSE IncNodePurity
##M       3.7899        195083
##So      2.0856         24209
##Ed      2.2132        220723
##Po1    11.7079       1147578
##Po2    10.9745       1047336
##LF      2.5188        264657
##M.F     0.0318        283312
##Pop    -0.5309        436200
##NW      8.6603        501283
##U1      3.0975        146209
##U2      2.1023        193739
##Wealth  3.6449        587105
##Ineq    1.5564        235133
##Prob    7.5847        775490
##Time    2.7687        228685

varImpPlot(forestModel)

#predict data using random forest model
prediction_3 = predict(forestModel, data_1)
prediction_3
##1204

plot(forestModel)

#=========================================================================================
#END CODE FOR Q10.1
#=========================================================================================

#=========================================================================================
#START CODE FOR Q10.3 - PART 1
#=========================================================================================
data = read.table("10.3germancreditSummer2018.txt", sep = ' ')
head(data)

#Convert the classification column to 0 or 1 for glm
data$V21[data$V21 == 1] = 0
data$V21[data$V21 == 2] = 1

# set seed value for reproducible results
set.seed(1)

# split data into training and test set
split = sample(1:nrow(data), size = round(nrow(data)*0.7), replace = FALSE)
split
data.train = data[split,] #700 observations
data.test = data[-split,] #300 observations

#create logistic regression model
model1 = glm(data.train$V21 ~., data = data.train , family = binomial(link = "logit"))
summary(model1)

#improve the model by using only significant factors
model2 = glm(data.train$V21 ~ V1 + V2 + V3 + V4 + V5 + V6 + V8 + V9 + V10 + V14 + V15 + V20,data = data.train , family = binomial(link = "logit") )
summary(model2)


#=========================================================================================
#START CODE FOR Q10.3 - PART 2
#=========================================================================================

#test the model using test data
y_hat = predict(model2, data.test, type="response")
y_hat


#logistic regression gives the probablitily between 0 and 1
#convert fractions to 0 or 1  using 0.5 as threshold
y_hat[y_hat > 0.5] = as.integer(1)
y_hat[y_hat <= 0.5] = as.integer(0)

t  = table(y_hat,data.test$V21)
t
##y_hat   0   1
##0 182  38
##1  36  44

#Model's accuracy = 182 + 44 / 300 = 75.3%
#Cost = 
#Since cost of bad is 5 times higher, identify the actual cost using different thresholds

initial = 0.01
t_list = seq(initial, 1, 0.01)
t_list

cost = c(100)

for (i in 1:100) {
  y_hat_r = y_hat
  y_hat_r[y_hat > t_list[i]] = as.integer(1)
  y_hat_r[y_hat <= t_list[i]] = as.integer(0)
  t = as.matrix(table(y_hat_r,data.test$V21))
  a = 0
  b = 0
  if(ncol(t) > 1) {
    a = t[1,2]
  }
  if(nrow(t) > 1) {
    b = t[2,1]
  }

  cost[i] = a*5 + b
}

length(cost)
which.min(cost)  
##21
cost[21]
##153

plot(t_list,cost, xlab = "Threshold", ylab = "Cost")
  

