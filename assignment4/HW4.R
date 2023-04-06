library(dplyr)
library(magrittr)

data = read.csv("/Users/willkczy/Econometric/Econometric_git/assignment4/input/htv.csv")

### 1(a) 
#the range of the educ variable in the sample
range(data$educ)
#create a subset of data where educ is less than or equal to 12
edu_subset = subset(data, educ <= 12)

#count how many observations are in edu_subset
obs_edu_sub = nrow(edu_subset)

#count how many observations are in data$educ
obs = length(data$educ)

#percentage of men completed twelfth grade but no higher grade: 56.74%
obs_edu_sub/obs

#men on average have higher education tham their parents.
mean(data$educ)
parents_mean = (mean(data$motheduc) + mean(data$fatheduc))/2


### 1(b)
# 24.93% of variation in the sample is explained by parents' education. It's estimated that for every annual increase in a mother's education, her child's education is likely to increased by 0.342 years. 
model = lm(data = data, educ ~ motheduc + fatheduc)
summary(model)

### 1(c)
# abil helps explain the variations in education even after controlling for parents' education form. The adjusted R-squared is increased to 42.61%
model_2 = lm(data = data, educ ~ motheduc + fatheduc + abil)
summary(model_2)

### 2(a)
# average values of prpblck and income in the sample is 0.113 and 47053.78
data_2 = read.csv("/Users/willkczy/Econometric/Econometric_git/assignment4/input/discrim.csv")

mean(data_2$prpblck, na.rm = TRUE)
mean(data_2$income, na.rm = TRUE)
sd(data_2$prpblck, na.rm = TRUE)
sd(data_2$income, na.rm = TRUE)

# 2(b)
# psoda = 0.9563 + 0.115prpblck + 0.000001603income + u. sample size = 401, R-squared = 0.06422
# one percentage change in prpblck is estimated to increase 0.115 cents in psoda, which is not really significant economically.
model_3 = lm(data = data_2, psoda ~ prpblck + income)
summary(model_3)

# 2(c)
# The discrimination effect is larger if we control for income. 
model_4 = lm(data = data_2, psoda ~ prpblck)
summary(model_4)