################################
## loved children colinearity ##
################################

# loading in data 
ind_full <- read.csv("~/Desktop/Loved Children/LC MLM/LC_merged.csv")

# for two conintuous variables we will use correlation coefficients 
# for a continuous and a dicotomous variable we will use standardized mean differences 

# prevalence and mixed infections 

tapply(ind_full$prevalence, ind_full$mixed, summary)

m1 <- mean(ind_full$prevalence[ind_full$mixed == 0], na.rm = 1)
m2 <- mean(ind_full$prevalence[ind_full$mixed == 1], na.rm = 1)
sd1 <- sd(ind_full$prevalence[ind_full$mixed == 0], na.rm = 1)
sd2 <- sd(ind_full$prevalence[ind_full$mixed == 1], na.rm = 1)
(m1 - m2)/sqrt(sd1 + sd2) 

# wealth mean and wealth index 

tapply(ind_full$wealth_mean, ind_full$wealthindex, summary)


m1 <- mean(ind_full$wealth_mean[ind_full$wealthindex == 1], na.rm = 1)
m2 <- mean(ind_full$wealth_mean[ind_full$wealthindex == 4], na.rm = 1)
sd1 <- sd(ind_full$wealth_mean[ind_full$wealthindex == 1], na.rm = 1)
sd2 <- sd(ind_full$wealth_mean[ind_full$wealthindex == 5], na.rm = 1)
(m1 - m2)/sqrt(sd1 + sd2) 


# education and wealth mean - community level 
cor(age, weight, use = "complete.obs")

cor(ind_full$pctedu0, ind_full$wealth_mean, use = "complete.obs")

age <- Children_mod$hc1
weight <- Children_mod$hw2[Children_mod$hw2 < 9000]


## wealth and urbanicity 

tapply(ind_full$wealth_mean, ind_full$urban1rural0, summary)

m1 <- mean(ind_full$wealth_mean[ind_full$urban1rural0 == 0], na.rm = 1)
m2 <- mean(ind_full$wealth_mean[ind_full$urban1rural0 == 1], na.rm = 1)
sd1 <- sd(ind_full$wealth_mean[ind_full$urban1rural0 == 0], na.rm = 1)
sd2 <- sd(ind_full$wealth_mean[ind_full$urban1rural0 == 1], na.rm = 1)
(m1 - m2)/sqrt(sd1 + sd2) 

# prevalence and urbanicity 

tapply(ind_full$prevalence, ind_full$urban1rural0, summary)

m1 <- mean(ind_full$prevalence[ind_full$urban1rural0 == 0], na.rm = 1)
m2 <- mean(ind_full$prevalence[ind_full$urban1rural0 == 1], na.rm = 1)
sd1 <- sd(ind_full$prevalence[ind_full$urban1rural0 == 0], na.rm = 1)
sd2 <- sd(ind_full$prevalence[ind_full$urban1rural0 == 1], na.rm = 1)
(m1 - m2)/sqrt(sd1 + sd2) 

# SP use and wealth mean 

cor(ind_full$FansPercent, ind_full$wealth_mean)
