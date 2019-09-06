#clear environment
rm(list = ls())

#=========================================================================================
#START CODE FOR Q7.2
#=========================================================================================
data = read.table("7.2tempsSummer2018.txt", header = T, sep ='\t')
head(data)
##DAY X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003 X2004 X2005 X2006
##1 1-Jul    98    86    91    84    89    84    90    73    82    91    93
##2 2-Jul    97    90    88    82    91    87    90    81    81    89    93
##3 3-Jul    97    93    91    87    93    87    87    87    86    86    93
##4 4-Jul    90    91    91    88    95    84    89    86    88    86    91
##5 5-Jul    89    84    91    90    96    86    93    80    90    89    90
##6 6-Jul    93    84    89    91    96    87    93    84    90    82    81
##
##X2007 X2008 X2009 X2010 X2011 X2012 X2013 X2014 X2015
##1    95    85    95    87    92   105    82    90    85
##2    85    87    90    84    94    93    85    93    87
##3    82    91    89    83    95    99    76    87    79
##4    86    90    91    85    92    98    77    84    85
##5    88    88    80    88    90   100    83    86    84
##6    87    82    87    89    90    98    83    87    84

#Get the vector of temperatures only from data
data_v = as.vector(unlist(data[,2:21]))
data_v


#Create timeseries object for temperatures from 1996 to 2015 using 123 days for each year
temp = ts(data_v,start=1996, frequency=123)


#To use simple exponential smoothing 
hw = HoltWinters(temp, beta = FALSE, gamma = FALSE)
hw
## output
##Holt-Winters exponential smoothing without trend and without seasonal component.
##
##Call:
##  HoltWinters(x = temp, beta = FALSE, gamma = FALSE)
##
##Smoothing parameters:
##  alpha: 0.8388021
##beta : FALSE
##gamma: FALSE
##
##Coefficients:
##  [,1]
##a 63.30952


#To use double exponential smoothing 
hw = HoltWinters(temp, gamma = FALSE)
hw
##Holt-Winters exponential smoothing with trend and without seasonal component.
##
##Call:
##  HoltWinters(x = temp, gamma = FALSE)
##
##Smoothing parameters:
##  alpha: 0.8445729
##beta : 0.003720884
##gamma: FALSE
##
##Coefficients:
##  [,1]
##a 63.2530022
##b -0.0729933

#To use tripe exponential smoothing with trend and additive seasonal component
hw = HoltWinters(temp)
hw
##Holt-Winters exponential smoothing with trend and additive seasonal component.
##
##Call:
##  HoltWinters(x = temp)
##
##Smoothing parameters:
##  alpha: 0.6610618
##beta : 0
##gamma: 0.6248076
##
##Coefficients:
##  [,1]
##a     71.477236414
##b     -0.004362918
##s1    18.590169842
##....
##s123  -7.775306633
#


#To use tripe exponential smoothing with trend and multiplicative seasonal component
hw = HoltWinters(temp, seasonal = "multiplicative")
hw
##Holt-Winters exponential smoothing with trend and multiplicative seasonal component.
##
##Call:
##  HoltWinters(x = temp, seasonal = "multiplicative")
##
##Smoothing parameters:
##  alpha: 0.615003
##beta : 0
##gamma: 0.5495256
##
##Coefficients:
##  [,1]
##a    73.679517064
##b    -0.004362918
##s1    1.239022317
##....
##s123  0.874038407
m = matrix(hw$fitted[,4],nrow=123)
m

#get smoothed temperature values
m_smooth = matrix(hw$fitted[,1], nrow = 123)
m_smooth

#create csv file using this matrix
write(m, file = "ans7_2.csv", ncolumns = 123, append = FALSE, sep = "," )

#create csv file using smoothed temperatures matrix
write(m_smooth, file = "ans7_2_a.csv", ncolumns = 123, append = FALSE, sep = "," )


#Use cusum in excel on seasonal smoothing coefficients.

#=========================================================================================
#END CODE FOR Q7.2
#=========================================================================================

#=========================================================================================
#START CODE FOR Q8.2
#=========================================================================================
data = read.table("8.2uscrimeSummer2018.txt", header = T, sep ='\t')
head(data)
## M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob
##1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602
##2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599
##3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401
##4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801
##5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399
##6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201

##Time Crime
##1 26.2011   791
##2 25.2999  1635
##3 24.3006   578
##4 29.9012  1969
##5 21.2998  1234
##6 20.9995   682

plot(  data$Pop , data$Crime )

#Perform linear regression
model1 = lm(Crime~., data = data)
summary(model1)
##
##
##Call:
##  lm(formula = Crime ~ ., data = data)
##
##Residuals:
##  Min      1Q  Median      3Q     Max 
##-395.74  -98.09   -6.69  112.99  512.67 

##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) -5.984e+03  1.628e+03  -3.675 0.000893 ***
##  M            8.783e+01  4.171e+01   2.106 0.043443 *  
##  So          -3.803e+00  1.488e+02  -0.026 0.979765    
##Ed           1.883e+02  6.209e+01   3.033 0.004861 ** 
##  Po1          1.928e+02  1.061e+02   1.817 0.078892 .  
##Po2         -1.094e+02  1.175e+02  -0.931 0.358830    
##LF          -6.638e+02  1.470e+03  -0.452 0.654654    
##M.F          1.741e+01  2.035e+01   0.855 0.398995    
##Pop         -7.330e-01  1.290e+00  -0.568 0.573845    
##NW           4.204e+00  6.481e+00   0.649 0.521279    
##U1          -5.827e+03  4.210e+03  -1.384 0.176238    
##U2           1.678e+02  8.234e+01   2.038 0.050161 .  
##Wealth       9.617e-02  1.037e-01   0.928 0.360754    
##Ineq         7.067e+01  2.272e+01   3.111 0.003983 ** 
##  Prob        -4.855e+03  2.272e+03  -2.137 0.040627 *  
##  Time        -3.479e+00  7.165e+00  -0.486 0.630708    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##Residual standard error: 209.1 on 31 degrees of freedom
##Multiple R-squared:  0.8031,	Adjusted R-squared:  0.7078 
##F-statistic: 8.429 on 15 and 31 DF,  p-value: 3.539e-07

#plot(model1)

#Get prediction for given data

#Create data with all given values
data_1 = data.frame(M=14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, 
               NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

prediction_1 = predict(model1, data_1)
prediction_1
##155.4349 

#Create the model using only the predictors whose p-value is 0.05 or less
model2 = lm(Crime~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)
summary(model2)

##
##
##Call:
##  lm(formula = Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)
##
##Residuals:
##  Min      1Q  Median      3Q     Max 
##-470.68  -78.41  -19.68  133.12  556.23 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) -5040.50     899.84  -5.602 1.72e-06 ***
##  M             105.02      33.30   3.154  0.00305 ** 
##  Ed            196.47      44.75   4.390 8.07e-05 ***
##  Po1           115.02      13.75   8.363 2.56e-10 ***
##  U2             89.37      40.91   2.185  0.03483 *  
##  Ineq           67.65      13.94   4.855 1.88e-05 ***
##  Prob        -3801.84    1528.10  -2.488  0.01711 *  
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 200.7 on 40 degrees of freedom
##Multiple R-squared:  0.7659,	Adjusted R-squared:  0.7307 
##F-statistic: 21.81 on 6 and 40 DF,  p-value: 3.418e-11

#Predict for given data using model 2
prediction_2 = predict(model2, data_1)
prediction_2
##1304.245

#Create 3rd model using step function
step(model1)
## .. Lots of output
##Call:
##lm(formula = Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, 
##   data = data)
##
##Coefficients:
##  (Intercept)            M           Ed          Po1          M.F           U1           U2         Ineq         Prob  
##-6426.10        93.32       180.12       102.65        22.34     -6086.63       187.35        61.33     -3796.03 

model3 = lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 +Ineq + Prob, data = data)
summary(model3)

##Call:
##lm(formula = Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, 
##   data = data)
##
##Residuals:
##  Min      1Q  Median      3Q     Max 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) -6426.10    1194.61  -5.379 4.04e-06 ***
##  M              93.32      33.50   2.786  0.00828 ** 
##  Ed            180.12      52.75   3.414  0.00153 ** 
##  Po1           102.65      15.52   6.613 8.26e-08 ***
##  M.F            22.34      13.60   1.642  0.10874    
##U1          -6086.63    3339.27  -1.823  0.07622 .  
##U2            187.35      72.48   2.585  0.01371 *  
##  Ineq           61.33      13.96   4.394 8.63e-05 ***
##  Prob        -3796.03    1490.65  -2.547  0.01505 *  
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 195.5 on 38 degrees of freedom
##Multiple R-squared:  0.7888,	Adjusted R-squared:  0.7444 
##F-statistic: 17.74 on 8 and 38 DF,  p-value: 1.159e-10

#predict data using 3rd model
prediction_3 = predict( model3, data_1)
prediction_3
##1038.413

#Assess quality of fit
library(DAAG)
layout(1,1,1)

model1_cv = cv.lm(data, model1, m =5)

#Calculate R^2 for cross validation for all 3 models
SS_total = sum((data$Crime - mean(data$Crime))^2) #total of sum of squared differences between data and mean
SSres_cv = attr(model1_cv, "ms") * nrow(data)
R2_1 =  1 - (SSres_cv/SS_total)
R2_1

model2_cv = cv.lm(data, model2, m =5)
SSres2_cv = attr(model2_cv, "ms") * nrow(data)
R2_2 =  1 - (SSres2_cv/SS_total)
R2_2

model3_cv = cv.lm(data, model3, m =5)
SSres3_cv = attr(model3_cv, "ms") * nrow(data)
R2_3 =  1 - (SSres3_cv/SS_total)
R2_3

#Calculate adjusted R^2 for cross validation for all 3 models 
R2_1_adj = 1 - (1-R2_1)*((nrow(data)-1)/(nrow(data)-15-1))
R2_1_adj                     
                         
R2_2_adj = 1 - (1-R2_2)*((nrow(data)-1)/(nrow(data)-6-1))
R2_2_adj 

R2_3_adj = 1 - (1-R2_3)*((nrow(data)-1)/(nrow(data)-8-1))
R2_3_adj 
#=========================================================================================
#END CODE FOR Q8.2
#=========================================================================================