#Question-1
#Student ID:20660329

#Q1)
# ssx<- function(L,n){
#   n<-100
#   L<-10000
#   intervals<-sapply(1:L,function(x){
#     y<-rnorm(n)
#     bootstrap.L<-100
#     mean<- sapply(1:bootstrap.reps, function(x) {
#       y.resample <-sample(y,replace=TRUE)
#       mean(y.resample)
#       y.bar<-mean(y)
#       sigma<-sd(y)
#       final_mean <- (mean-y.bar)/sigma;
#     })
#   })
#   return final_mean;
# }
# ssx(1000,100)


#Q2)

# In applied statistics, chisquared test statistics arise 
# as sums of squared residuals, or from sums of squared effects
# or from log-likelihood differences. In all of these applications, 
# the aim is to test whether some vector parameter
# is zero vs the alternative that it is non-zero and
# the chisquare statistic is related to the squared size
# of the observed effect.

#Q3)

require(graphics)

dchisq(1, df = 1:3)
pchisq(1, df =  3)
pchisq(1, df =  3, ncp = 0:4)  # includes the above

x <- 1:10
## Chi-squared(df = 2) is a special exponential distribution
all.equal(dchisq(x, df = 2), dexp(x, 1/2))
all.equal(pchisq(x, df = 2), pexp(x, 1/2))

## non-central RNG -- df = 0 with ncp > 0:  Z0 has point mass at 0!
Z0 <- rchisq(100, df = 0, ncp = 2.)
graphics::stem(Z0)

## visual testing
## do P-P plots for 1000 points at various degrees of freedom
L <- 1.2; n <- 1000; pp <- ppoints(n)
op <- par(mfrow = c(3,3), mar = c(3,3,1,1)+.1, mgp = c(1.5,.6,0),
          oma = c(0,0,3,0))
for(df in 2^(4*rnorm(9))) {
  plot(pp, sort(pchisq(rr <- rchisq(n, df = df, ncp = L), df = df, ncp = L)),
       ylab = "pchisq(rchisq(.),.)", pch = ".")
  mtext(paste("df = ", formatC(df, digits = 4)), line =  -2, adj = 0.05)
  abline(0, 1, col = 2)
}
mtext(expression("P-P plots : Noncentral  "*
                   chi^2 *"(n=1000, df=X, ncp= 1.2)"),
      cex = 1.5, font = 2, outer = TRUE)
par(op)

## "analytical" test
lam <- seq(0, 100, by = .25)
p00 <- pchisq(0,      df = 0, ncp = lam)
p.0 <- pchisq(1e-300, df = 0, ncp = lam)
stopifnot(all.equal(p00, exp(-lam/2)),
          all.equal(p.0, exp(-lam/2)))


#Q4) Yes, it is possible to generate a linear reg model with soreted ssx and ssy, they are independant.

#Q5) We should be able to derive a statistical meaning as ssxy is dependant on ssx and ssy but there are again independant.

#which means the increase in  the value of ssx and ssy will effect the statistic here.


