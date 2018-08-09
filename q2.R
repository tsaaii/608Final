#Question-2
#Student ID:20660329

#Q1)
df<-read.csv('lung.csv')
df_standard <- subset(df, ChemoType == 'standard')
df_test <- subset(df, ChemoType == 'test')
plot(SurvivalDays~ChemoType, data=df)
library(mclust)
fit <- Mclust(df)
#plot(fit) #uncomment these two lines to see the clusters
#summary(fit) 
#if we plot ChemoType with various explanatory variables given in the datset, we see that most of the data is grouped in to two major types.
#Since these are clustered in to 2 types, there may be a significance among the factors, so we grouped it in to two.
#I used an extrenal library to cluster the complete dataset to see any correlation and I can see that almost all the explanatory variables are grouped in to two types.

#Q2)

#Write R code to plot Kaplan-Meier curves which compare the survival times
#of the two chemotherapy treatment groups (standard, test).

library( survival )
with( df, {
  KM.model <<- survfit( Surv( time = SurvivalDays, event = IsCensored ) ~ ChemoType )
  print( summary( KM.model ) )
  cat( sprintf( "Standard survival time max: %d\r\n", max( SurvivalDays[ ChemoType == 'standard' ] ) ) )
  cat( sprintf( "test survival time max: %d\r\n", max( SurvivalDays[ ChemoType == 'test' ] ) ) )
} )

# ChemoType=standard 
# time n.risk n.event P(yes)   P()
# 25     52       1 0.0192 0.981
# 97     35       1 0.0473 0.953
# 100     34       1 0.0753 0.925
# 123     25       1 0.1123 0.888
# 182     13       1 0.1806 0.819
# 
# ChemoType=test 
# time n.risk n.event P(yes)   P()
# 83     29       1 0.0345 0.966
# 87     27       1 0.0702 0.930
# 103     21       1 0.1145 0.885
# 231     12       1 0.1883 0.812
# 
# Standard survival time max: 553
# test survival time max: 999

library( RColorBrewer )
cols <- brewer.pal( n = 3, name = "Dark2" )

par.save <- par( mar = c( 4, 4, 3, 2 ), las = 1 )
plot( KM.model, 
      ann = FALSE, axes = FALSE, 
      xlim = c( -25, 1000 ), ylim = c( 0, 1 ),
      col = cols )
points( x = 0, y = 1, pch = 20 )
axis( side = 1, at = seq( from = 0, to = 1000, by = 300 ), labels = c( 0, 1, 2, 3 ), lwd = 0 )
axis( side = 2, at = c( 0, 0.5, 1 ), labels = c( "0", ".5", "1" ), lwd = 0 )
text( x = 750, y = 0.25, adj = 0, label = "standard", col = cols[ 1 ] )
text( x = 750, y = 0.64, adj = 0, label = "test", col = cols[ 2 ] )
mtext( side = 3, line = 1, text = "Survival days versus chemotype" )
par( par.save )


#Q3)

#Write R code to find the log-rank test statistic and
#its p-value for the situation in part 1.

s <- survival::Surv( time = df$SurvivalDays, event = df$ChemoType=="standard" )
te <- survival::Surv( time = df$SurvivalDays, event = df$ChemoType=="test" )
print(summary(s))
print(summary(te))
t.test(s)
t.test(te)

# For test
# 
# One Sample t-test
# 
# data:  te
# t = 6.4046, NA = 557.3300, NA = 557.3300, NA = 2014.2000, df =
#   273, p-value = 6.564e-10, p-value = < 2.2e-16, p-value = <
#   2.2e-16, p-value = < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   42.29243 61.27774
# sample estimates:
#   mean of x 
# 61.06204 


# For standard
# 
# One Sample t-test
# 
# data:  s
# t = 6.405, NA = NaN, NA = NaN, NA = 2014.300, df = 273, p-value =
#   6.55e-10, p-value = NA, p-value = NA, p-value = < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   42.29608      NaN
# sample estimates:
#   mean of x 
# 61.06569 

# Q4)Interpret the results from parts 2 and 3.

#The results shows that the group test have a better survival time than standard and there is no much difference in the P values of both groups meaning the grouping which was done is fair.

# Q5) Patients with a Karnofsky percent above 60
# can be considered healthier than patients with 
# a Karnofsky percent of 60 or below. Using this division, 
# write R code to plot Kaplan-Meier curves which compare 
# the survival times of the four groups determined by 
# combinations of the chemotherapy treatments and the high 
# and low Karnofsky percent ranges.

df_k60 <- subset(df, KarnofskyPercent >= 60)
df_low <- subset(df, KarnofskyPercent < 60)

with( df, {
  KM.model <<- survfit( Surv( time = SurvivalDays, event =  IsCensored ) ~ ChemoType+KarnofskyPercent )
  print( summary( KM.model ) )
  cat( sprintf( "Standard survival time max: %d\r\n", max( SurvivalDays[ ChemoType == 'standard' ] ) ) )
  cat( sprintf( "test survival time max: %d\r\n", max( SurvivalDays[ ChemoType == 'test' ] ) ) )
  cat( sprintf( "KarnofskyPercent 60+: %d\r\n", df$KarnofskyPercent >= 60   ) )
  cat( sprintf( "KarnofskyPercent 60 or low %d\r\n", df$KarnofskyPercent < 60   ) )
} )
# 
# ChemoType=standard, KarnofskyPercent=40 
# time  n.risk n.event  P(yes)     P() 
# 123.0     2.0     1.0     0.5     0.5 
# 
# ChemoType=standard, KarnofskyPercent=60 
# time  n.risk n.event  P(yes)     P() 
# 97.000   8.000   1.000   0.125   0.875 
# 
# ChemoType=standard, KarnofskyPercent=70 
# time  n.risk n.event  P(yes)     P() 
# 100.000   6.000   1.000   0.167   0.833 
# 
# ChemoType=standard, KarnofskyPercent=80 
# time  n.risk n.event  P(yes)     P() 
# 25.0000 16.0000  1.0000  0.0625  0.9375 
# 
# ChemoType=standard, KarnofskyPercent=90 
# time  n.risk n.event  P(yes)     P() 
# 182       1       1       1       0 
# 
# ChemoType=test, KarnofskyPercent=50 
# time  n.risk n.event  P(yes)     P() 
# 231.0     2.0     1.0     0.5     0.5 
# 
# ChemoType=test, KarnofskyPercent=70 
# time  n.risk n.event  P(yes)     P() 
# 103.000   7.000   1.000   0.143   0.857 
# 
# ChemoType=test, KarnofskyPercent=80 
# time  n.risk n.event  P(yes)     P() 
# 87.0     5.0     1.0     0.2     0.8 
# 
# ChemoType=test, KarnofskyPercent=99 
# time  n.risk n.event  P(yes)     P() 
# 83       1       1       1       0 
# 
# Standard survival time max: 553
# test survival time max: 999

#Q6)Write R code to find the log-rank test statistic and its p-value for the situation in part 5. 

KP_greaterthan_60 <- survival::Surv( time = df$SurvivalDays, event = df$KarnofskyPercent >=60 )
KP_lessthan_60 <- survival::Surv( time = df$SurvivalDays, event = df$KarnofskyPercent <  60 )
print(summary(KP_greaterthan_60))
t.test(KP_greaterthan_60)


# data:  KP_greaterthan_60
# t = 6.4111, NA = 206.2300, NA = 206.2300, NA = 2077.3000, df =
#   273, p-value = 6.325e-10, p-value = < 2.2e-16, p-value = <
#   2.2e-16, p-value = < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   42.35447 61.70760
# sample estimates:
#   mean of x 
# 61.12409 

print(summary(KP_lessthan_60))
t.test(KP_lessthan_60)

# data:  KP_lessthan_60
# t = 6.3985, NA = NaN, NA = NaN, NA = 2073.2000, df = 273, p-value
# = 6.797e-10, p-value = NA, p-value = NA, p-value = < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   42.23403      NaN
# sample estimates:
#   mean of x 
# 61.00365 

#Q7). Interpret the results from parts 5 and 6.

#It looks like the KarnofskyPercent is as assumed. If it is greater than 60 they are tend to be more healthy than the later
#there is a correlation between this one and the survival days too...most of the healthy are in test group.








