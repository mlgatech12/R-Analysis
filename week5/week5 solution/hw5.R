#clear environment
rm(list = ls())

#set seed
set.seed(1)


#=========================================================================================
#START CODE FOR Q12.2
#=========================================================================================
library(FrF2)

fr_factorial_design = FrF2(nruns = 16,nfactors = 10)

fr_factorial_design
##> fr_factorial_design
##A  B  C  D  E  F  G  H  J  K
##1  -1 -1  1 -1  1 -1 -1  1  1 -1
##2   1 -1  1 -1 -1  1 -1 -1  1  1
##3  -1 -1 -1  1  1  1  1 -1  1 -1
##4   1  1 -1  1  1 -1 -1  1 -1 -1
##5  -1  1 -1 -1 -1  1 -1  1  1 -1
##6   1 -1 -1  1 -1 -1  1  1  1  1
##7  -1  1 -1  1 -1  1 -1 -1 -1  1
##8  -1  1  1  1 -1 -1  1 -1  1 -1
##9   1 -1  1  1 -1  1 -1  1 -1 -1
##10 -1 -1 -1 -1  1  1  1  1 -1  1
##11  1 -1 -1 -1 -1 -1  1 -1 -1 -1
##12 -1  1  1 -1 -1 -1  1  1 -1  1
##13 -1 -1  1  1  1 -1 -1 -1 -1  1
##14  1  1  1 -1  1  1  1 -1 -1 -1
##15  1  1 -1 -1  1 -1 -1 -1  1  1
##16  1  1  1  1  1  1  1  1  1  1
##class=design, type= FrF2 


#=========================================================================================
#START CODE FOR Q11.1
#=========================================================================================
data = read.table("11.1uscrimeSummer2018.txt", header = T, sep ='\t')
head(data)
##     M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
##1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
##2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
##3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
##4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
##5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
##6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682


#scale data except for 2nd column which has binary data and last column, which has Response variable.
scaledData = as.data.frame(scale(data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
scaledData
scaledData = cbind(scaledData[, 1:1], data[,2], scaledData[, 2:14], data[,16])
colnames(scaledData)[1] = "M"
colnames(scaledData)[2] = "So"
colnames(scaledData)[16] = "Crime"

#1. Use Stepwise regression
model_1 = lm(Crime ~. , data = scaledData)
step(model_1, direction = "backward")

#Create model using the factors received from stepwise regression
model_2 = lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = scaledData)
model_2
##Call:
##lm.default(formula = Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + 
##             Prob, data = scaledData)

##Coefficients:
##  (Intercept)            M           Ed          Po1          M.F           U1           U2         Ineq         Prob  
##905.09       117.28       201.50       305.07        65.83      -109.73       158.22       244.70       -86.31

summary(model_2)

#Use cross validation on this model

SStot = sum((data$Crime - mean(data$Crime))^2)
totsse = 0
for(i in 1:nrow(scaledData)) {
  model_i = lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = scaledData[-i,])
  pred_i = predict(model_i, newdata=scaledData[i,])
  totsse = totsse + ((pred_i - data[i,16])^2)
}
R2_cv <- 1 - totsse/SStot
R2_cv
##0.667621

#2. Use Lasso
library(glmnet)

#create model, use alpha = 1 for lasso
model_lasso = cv.glmnet(x=as.matrix(data[,-16]), y = as.matrix(data[,16]), alpha = 1, nfolds = 5, 
                     type.measure = "mse", family = "gaussian")
model_lasso

coef(model_lasso, s=model_lasso$lambda.min)
##16 x 1 sparse Matrix of class "dgCMatrix"
##1
##(Intercept) -4315.174518
##M              64.106051
##So             49.606315
##Ed             95.353431
##Po1           104.941252
##Po2             .       
##LF             90.100979
##M.F            16.037973
##Pop             .       
##NW              0.239049
##U1           -230.894946
##U2             42.789862
##Wealth          .       
##Ineq           42.588734
##Prob        -3469.699253
##Time            .   


#Removing insignificant factors as suggested by Lasso
model_lasso = lm(Crime ~ M + So + Ed + Po1 + LF + M.F + NW + U1 + U2 + Ineq + Prob, data = scaledData)
model_lasso
##Call:
##lm.default(formula = Crime ~ M + So + Ed + Po1 + LF + M.F + NW + 
##             U1 + U2 + Ineq + Prob, data = scaledData)

##Coefficients:
##  (Intercept)            M           So           Ed          Po1           LF          M.F           NW  
##892.63       106.61        36.57       209.15       295.60       -10.69        74.96        13.01  
##U1           U2         Ineq         Prob  
##-109.08       151.47       233.00       -96.00  


AIC(model_lasso)
##644.92

summary(model_lasso)


#Use cross validation on this model
totsse = 0
for(i in 1:nrow(scaledData)) {
  model_i = lm(Crime ~ M + So + Ed + Po1 + LF + M.F + NW + U1 + U2 + Ineq + Prob, data = scaledData[-i,])
  pred_i = predict(model_i, newdata=scaledData[i,])
  totsse = totsse + ((pred_i - data[i,16])^2)
}
R2_cv <- 1 - totsse/SStot
R2_cv
##0.596

#3. Use Elasticnet

#find the best value of alpha to be used
R2 = numeric(10)
ct = 1
for (i in 1:10) {
model_elasticnet = cv.glmnet(x=as.matrix(data[,-16]), y = as.matrix(data[,16]), alpha = i/10, nfolds = 5, 
                        type.measure = "mse", family = "gaussian")

R2[ct] = model_elasticnet$glmnet.fit$dev.ratio[which(model_elasticnet$lambda == model_elasticnet$lambda.min)]
ct = ct +1

}

R2
##[1] 0.7490353 0.7446501 0.7829777 0.7781097 0.7179815 0.7541379 0.7687711 0.7832011 0.7841372 0.7886713

alpha_best = which.max(R2)/10
alpha_best
##1

model_enet = cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),alpha=alpha_best,
                      nfolds = 5,type.measure="mse",family="gaussian")

#get the coefficients to be used for model
coef(model_enet, s=model_enet$lambda.min)
##16 x 1 sparse Matrix of class "dgCMatrix"
##1
##(Intercept) 894.04865
##M           103.66132
##So           32.41958
##Ed          173.09241
##Po1         296.62268
##Po2           .      
##LF            .      
##M.F          52.65592
##Pop         -17.56833
##NW           13.84601
##U1          -69.23520
##U2          113.82743
##Wealth       50.96794
##Ineq        246.80851
##Prob        -88.83419
##Time          .     

#Removing insignificant factors as suggested by Elasticnet
model_enet = lm(Crime ~ M + So + Ed + Po1 +  M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = scaledData)
model_enet
##Call:
##  lm.default(formula = Crime ~ M + So + Ed + Po1 + M.F + Pop + 
##               NW + U1 + U2 + Wealth + Ineq + Prob, data = scaledData)

##Coefficients:
##  (Intercept)            M           So           Ed          Po1          M.F          Pop           NW  
##897.29       112.71        22.89       195.70       293.18        48.92       -33.25        19.16  
##U1           U2       Wealth         Ineq         Prob  
##-89.76       140.78        83.30       285.77       -92.75  

AIC(model_enet)
##645.4286

summary(model_enet)


#Use cross validation on this model
totsse = 0
for(i in 1:nrow(scaledData)) {
  model_i = lm(Crime ~ M + So + Ed + Po1 +  M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = scaledData[-i,])
  pred_i = predict(model_i, newdata=scaledData[i,])
  totsse = totsse + ((pred_i - data[i,16])^2)
}
R2_cv <- 1 - totsse/SStot
R2_cv
##0.590

