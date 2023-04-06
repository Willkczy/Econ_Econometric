library(data.table)
library(multcomp)

####1
data = read.csv("/Users/willkczy/Econometric/Econometric_git/assignment5/input/discrim.csv")
# data_table = fread("/Users/willkczy/Econometric/Econometric_git/assignment5/input/discrim.csv")

my_datatable <- fread("/Users/willkczy/Econometric/Econometric_git/assignment5/input/discrim.csv", select = c("psoda", "prpblck", "income", "prppov", "hseval"))

my_datatable[, log_psoda := log(my_datatable[, psoda])]
my_datatable[, log_income := log(my_datatable[, income])]
my_datatable[, log_hseval := log(my_datatable[, hseval])]

# (a) The p-value for testing H0:   against the two-sided alternative is about .018, so that we reject H0 at the 5% level
model = lm(log_psoda ~ prpblck + log_income + prppov, data = my_datatable)
summary(model)

# (b) The correlation is about -0.84. every coefficient is very statistically significant at 5% level. The two sided p-value for prppov is 0.004 and 0.00000048 for log_income
cor(na.omit(my_datatable[, c("prppov", "log_income")]))

# (c) The coefficient of hseval suggests that one percentage increase in housing value, increases the estimated price at 0.12%. The two-sided p-value is 0.0000000000267
model_2 = lm(log_psoda ~ prpblck + log_income + prppov + log_hseval, data = my_datatable)
summary(model_2)

model_3 = lm(log_psoda ~ prpblck + log_hseval, data = my_datatable)
summary(model_3)

# (d) The added estimator makes log_income and prppov statistically insignificant, they are jointly significant at the 5% level the F statistic is about 3.517951 with p-value = 0.030
# the control variables log_incom and prppov are highly correlated, so it's reasonable that they are individually insignificant
f_statistic = ((0.1839 - 0.1694)/2)/((1-0.1839)/(396))

# (e) the third one, since it contains the most controls and log_hesval is statistically strongly significant while log_income and prppov are jointly significant. 

####2

data_2 = fread("/Users/willkczy/Econometric/Econometric_git/assignment5/input/htv.csv", select = c("educ", "motheduc", "fatheduc", "abil", "tuit17", "tuit18"))
data_2[, abil_square := (data_2[, abil])^2]

# (a) educ = 8.24 + 0.19*motheduc + 0.109*fatheduc + 0.401*abil + 0.0505*abil^2. 
# H_0 = beta_4 = 0, H_1 = beta_4 != 1. The p-value is 1.48e-09, which reject H_0 at both 5% and 1% level
model_4 = lm(educ ~ motheduc + fatheduc + abil + abil_square, data = data_2)
summary(model_4)

# (b) the t statistic is about 1.94  and the The p-value is about 0.053. 
linfct <- c(1, -1, 0, 0, 0)
test <- glht(model_4, linfct = linfct)

# (c) the F statistic is 0.84 and the p-value is 0.43. The two tuition level is jointly insignificant.
# Fit the multiple regression model
model_5 <- lm(educ ~ motheduc + fatheduc + abil + abil_square + tuit17 + tuit18, data = data_2)

library(car)
linearHypothesis(model_5, c("tuit17 = 0", "tuit18 = 0"))

# (d) The correlation between the two tuition is 0.9808333, which is very high, indicating that there is very little variation in tuition fees over a year that cannot be attributed to a common inflation factor
# add the new variable avg_tui. Although this variable improves the statistical significance of the model, the two-sided p-value remains around 0.2.
cor(na.omit(data_2[, c("tuit17", "tuit18")]))
data_2[, avg_tui := (data_2[, tuit17] + data_2[, tuit18])/2]

model_6 <- lm(educ ~ motheduc + fatheduc + abil + abil_square + avg_tui, data = data_2)
summary(model_6)

# (e) The positive coefficient on avgtuit may appear counterintuitive, given the assumption that higher tuition would discourage students from attending college, all other factors being equal. However, the model only takes into account parents' education and an ability measure, and doesn't consider other important factors that could influence college attendance. For instance, it's possible that higher tuition fees reflect higher quality of education in state colleges, or that states with higher average incomes tend to have higher tuition fees, which in turn lead to higher education rates