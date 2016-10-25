library(psych)
r.con(r=.21, n=100)

#load population data
set.seed(8)
library(learnSampling)
lower.pop <- get_cor_data(.01,100000)
upper.pop <- get_cor_data(.39,100000)

head(lower.pop)
head(upper.pop)

#confirming p
rho.lower <- cor(lower.pop$x, lower.pop$y)
rho.upper <- cor(upper.pop$x, upper.pop$y)

#print(rho.lower)
#print(rho.upper)

#lower bound popn corr: create sampling distribution
ci.lower.bound.correlations <- get_cor_samples(pop.data=lower.pop,n=100,
                                               number.of.samples = 10000,
                                               number.of.decimals = 2)

#lower bound popn corr: middle 95%
ci.lower.bound.correlations.sorted <- sort_samples_by_r(ci.lower.bound.correlations)
lower.cilowerbound <- ci.lower.bound.correlations.sorted$r[251]
upper.cilowerbound <- ci.lower.bound.correlations.sorted$r[9750]
#print(lower.cilowerbound)
#print(upper.cilowerbound)

#graph lower bound
library(ggplot2)
lower.bound.hist <- qplot(r, data=ci.lower.bound.correlations, binwidth=.05)
#print(lower.bound.hist)

#upper bound popn corr: create sampling distribution
ci.upper.bound.correlations <- get_cor_samples(pop.data=upper.pop,n=100,
                                               number.of.samples = 10000,
                                               number.of.decimals = 2)

#upper bound popn corr: middle 95%
ci.upper.bound.correlations.sorted <- sort_samples_by_r(ci.upper.bound.correlations)
lower.ciupperbound <- ci.upper.bound.correlations.sorted$r[251]
upper.ciupperbound <- ci.upper.bound.correlations.sorted$r[9750]
#print(lower.ciupperbound)
#print(upper.ciupperbound)

#graph upper bound
library(ggplot2)
upper.bound.hist <- qplot(r, data=ci.upper.bound.correlations, binwidth=.05)
#print(upper.bound.hist)

#graph both distributions
library(ggplot2)
both.hist <- ggplot(ci.upper.bound.correlations,aes(r))
both.hist <- both.hist + geom_histogram(aes(x= ci.upper.bound.correlations$r, y=..count..),
                                        fill = "red", binwidth=.05)
both.hist <- both.hist + geom_histogram(aes(x= ci.lower.bound.correlations$r, y=..count..),
                                        fill = "blue", binwidth=.05)
#print(both.hist)

#confidence intervals
psych::r.con(r=.30, n=50)
psych::r.con(r=.30, n=500)
psych::r.con(r=.30, n=1000)

#correlation power analysis 
library(pwr)
pwr.r.test(r=.35, power=.80, alternative="two.sided") 

psych::r.con(r=.35, n=100)
library(pwr)
pwr.r.test(r =0.1649, power=.80, alternative="two.sided")

#what to expect
library(predictionInterval)
pi.r(r=0.41,n=70,rep.n=214)
