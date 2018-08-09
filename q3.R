#Question-3
#Student ID:20660329
df<-read.csv('hiv.csv')

# Derive your best multiple-regression model for predicting ‘Hiv’ from the given
# socioeconomic factors (‘Country’ is not considered a socioeconomic factor and should
# not be included in the regression). The sequence of steps you 
# take in arriving at your model should be clear in the file. Use comments to state your 
# goal in each step (what you are doing and why). Leave in place or comment out the R code 
# used in each step. Use comments to state the salient results and your interpretation
# for each step (quality or deficiencies of the model so far). 
# There is no one right (or even necessarily very good) answer for
# this question and every model will have defects.
#
# Note 1: The nature of the data is such that you should 
# use ‘log( Hiv )’ rather than ‘Hiv’ directly. This corrects 
# for right-skewedness of the response variable. If any of the 
# explanatory variables ‘Gdp’, ‘Density’, ‘Co2’, ‘Aid’, or ‘Urban’ 
# are used in your model, I suggest you use them log-transformed as well. 
# The transformed values seem to be less skewed, with more homoscedastic residuals. 
# For ‘Aid’, you need to filter out the large negative outlier to take its logarithm.
# 
# Note 2: There are many missing values (‘NA’) in the data. 
# This means part of the quality of a model is how few missing values affect it. 
# Unless you have lots of time, just ignore that aspect here. 
# Except for the model using only ‘Urban’ (or its log) as an 
# explanatory variable, all models will suffer from at least one missing value. 
# Using ‘log( Aid )’ adds another missing value since the negative 
# outlier must be filtered out (albeit for Afghanistan, which is already 
# missing ‘Gdp’ and ‘Density’ values anyways).








#I am starting with finding correlations between all columns, so that we can understand which feature has more influence on the response variable.

#I am using pearson correlation coefficient

# I am also removing the country name

df_country <- subset( df, select = -Country )
library(Hmisc)
ccs <- as.matrix(df_country)
rcorr(ccs, type="pearson")


#This is the summary here


# Literacy   Gdp Density Stability   Co2
# Literacy      1.00  0.41   -0.03      0.03  0.11
# Gdp           0.41  1.00   -0.12     -0.02  0.61
# Density      -0.03 -0.12    1.00      0.18 -0.10
# Stability     0.03 -0.02    0.18      1.00 -0.16
# Co2           0.11  0.61   -0.10     -0.16  1.00
# Growth       -0.01 -0.32   -0.06      0.00  0.25
# Aid           0.17 -0.58   -0.07      0.02 -0.22
# Urban         0.00  0.16   -0.14     -0.10  0.28
# Hiv           0.28 -0.03   -0.13     -0.22 -0.23
# Growth   Aid Urban   Hiv
# Literacy   -0.01  0.17  0.00  0.28
# Gdp        -0.32 -0.58  0.16 -0.03
# Density    -0.06 -0.07 -0.14 -0.13
# Stability   0.00  0.02 -0.10 -0.22
# Co2         0.25 -0.22  0.28 -0.23
# Growth      1.00 -0.10  0.08  0.13
# Aid        -0.10  1.00  0.02  0.08
# Urban       0.08  0.02  1.00 -0.05
# Hiv         0.13  0.08 -0.05  1.00
# 
# n
# Literacy Gdp Density Stability Co2 Growth
# Literacy        31  29      30        30  29     31
# Gdp             29  38      38        37  36     38
# Density         30  38      40        38  38     39
# Stability       30  37      38        39  37     38
# Co2             29  36      38        37  39     38
# Growth          31  38      39        38  38     40
# Aid             27  33      33        34  33     34
# Urban           31  38      40        39  39     40
# Hiv             31  38      40        39  39     40
# Aid Urban Hiv
# Literacy   27    31  31
# Gdp        33    38  38
# Density    33    40  40
# Stability  34    39  39
# Co2        33    39  39
# Growth     34    40  40
# Aid        34    34  34
# Urban      34    41  41
# Hiv        34    41  41
# 
# P
# Literacy Gdp    Density Stability Co2   
# Literacy           0.0285 0.8789  0.8555    0.5853
# Gdp       0.0285          0.4799  0.9276    0.0000
# Density   0.8789   0.4799         0.2789    0.5404
# Stability 0.8555   0.9276 0.2789            0.3381
# Co2       0.5853   0.0000 0.5404  0.3381          
# Growth    0.9722   0.0521 0.7181  0.9936    0.1323
# Aid       0.3930   0.0004 0.7109  0.9212    0.2227
# Urban     0.9892   0.3424 0.3914  0.5324    0.0858
# Hiv       0.1230   0.8751 0.4073  0.1737    0.1531
# Growth Aid    Urban  Hiv   
# Literacy  0.9722 0.3930 0.9892 0.1230
# Gdp       0.0521 0.0004 0.3424 0.8751
# Density   0.7181 0.7109 0.3914 0.4073
# Stability 0.9936 0.9212 0.5324 0.1737
# Co2       0.1323 0.2227 0.0858 0.1531
# Growth           0.5883 0.6176 0.4356
# Aid       0.5883        0.9002 0.6408
# Urban     0.6176 0.9002        0.7481
# Hiv       0.4356 0.6408 0.7481   










#From the summary I can see a strong correlation of HIV with 

# GDP, AID, Literacy, Growth, Aid, Urban

#though literacy is not having a great effect, it is strong with other 4 factors above.




# making a new dataframe with this...


df_featured<- subset( df_country, select = c("Literacy","Gdp","Growth","Aid","Urban","Hiv" ))


#replacing the huge negative outlier with its absolute value

df_featured <-abs(df_featured)

#lets take the log value of everything.

df_featured_log <- log(df_featured)

#Some NANs exist, not touching them due to less time

fit <- lm(Hiv ~ Literacy+Gdp+Growth+Aid+Urban, data=df_featured_log)
summary(fit) 

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0489 -0.6099  0.1136  0.8173  2.7668 
# 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept)  -5.2495     7.1109  -0.738
# Literacy      0.5194     0.9189   0.565
# Gdp           0.3395     0.8231   0.412
# Growth       -0.1056     0.5153  -0.205
# Aid           0.6385     0.5971   1.069
# Urban         0.1147     0.7254   0.158
# Pr(>|t|)
# (Intercept)    0.469
# Literacy       0.578
# Gdp            0.684
# Growth         0.840
# Aid            0.298
# Urban          0.876
# 
# Residual standard error: 1.512 on 20 degrees of freedom
# (15 observations deleted due to missingness)
# Multiple R-squared:  0.08624,	Adjusted R-squared:  -0.1422 
# F-statistic: 0.3775 on 5 and 20 DF,  p-value: 0.8582

residuals(fit) # residuals
anova(fit) # anova table 

#Analysis of Variance Table

# Response: Hiv
# Df Sum Sq Mean Sq F value Pr(>F)
# Literacy   1  0.335 0.33481  0.1464 0.7060
# Gdp        1  1.198 1.19789  0.5239 0.4775
# Growth     1  0.013 0.01331  0.0058 0.9399
# Aid        1  2.712 2.71246  1.1864 0.2890
# Urban      1  0.057 0.05720  0.0250 0.8759
# Residuals 20 45.727 2.28634 


vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

plot(fit)


#Summary

#I considered heavily correlated features
#removed negative outlier
#and made a regression fit






