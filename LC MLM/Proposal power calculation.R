library(simr)
library(lme4)
library(dplyr)

## trying to simulate some data and run a power calculation 
### power calc


x1 <- rnorm(5000) ## creating a continuous variable 
x2 <- sample(1:26,5000,replace=T) # creating some sort of grouping variable with 26 groups
y <- rbinom(n = 5000, size = 1, prob= 0.3) # creating a binary response variable (with probability of success = 0.3)
df <- data.frame(y = y, x1 = x1, x2 = x2) #merging into one data set 

# running a mixed model 
m1 <- glmer(y ~ x1 + (1|x2), data = df, family = poisson(link="log"))
fixef(m1)



fixef(m1)["x1"] <- 0.01 #creating a set fixed effect 

summary(m1)
# running power analysis 
powerSim(m1)

## will try to run with a binary variable 

dat_sample <- dat13[sample(1:nrow(dat13), 100, replace = FALSE),]
m1 <- glmer(PFLDH ~ age + (1|cluster), family = binomial(link = "logit"), data = dat_sample)
summary(m1)
fixef(m1)["age"] <- 0.01
powerSim(m1, alpha= 0.05)


powerCurve(m1)

