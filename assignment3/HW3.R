library(dplyr)
library(magrittr)

sink("HW3.log", append=TRUE, split=TRUE)

### Problem 1
cat("### Problem 1 \n", file = "HW3.log", append = TRUE)
data_1 = read.csv("/Users/willkczy/Econometric/Econometric_git/assignment3/input/HPRICE1.csv")

model_1 = lm(data$price ~ data$sqrft + data$bdrms, data = data_1)
summary(model_1)

# (a) price = -19.315 + 0.12844*sqrft + 15.19819bdrms
cat("(a) price = -19.315 + 0.12844*sqrft + 15.19819bdrms \n", file = "HW3.log", append = TRUE)

# (b) 15.19819
cat("(b) if the house had 1 more room, holding square footage constant, the price of the house would on average increase by 
$15,198 \n", file = "HW3.log", append = TRUE)

# (c) the estimated increase in price = (0.128 * 140)+ (15.198 * 1) = 17.92 + 15.198 = 33.118
cat("(c) the estimated increase in price = (0.128 * 140)+ (15.198 * 1) = 17.92 + 15.198 = 33.118 \n", file = "HW3.log", append = TRUE)

# (d) the multiple R-squared is 0.6319, suggesting that 63.19% of variations are explained with the model
cat("(d) the multiple R-squared is 0.6319, suggesting that 63.19% of variations are explained with the model \n", file = "HW3.log", append = TRUE)

# (e) = −19.315 + 0.128*2438 + 15198*4 = $353,541
cat("(e) = −19.315 + 0.128*2438 + 15198*4 = $353,541 \n", file = "HW3.log", append = TRUE)

# (f) underpaid by 353541 - 300000 = $53,541 
cat("(f) underpaid by 353541 - 300000 = $53,541 \n", file = "HW3.log", append = TRUE)

### Problem 2
cat("### Problem 2 \n", file = "HW3.log", append = TRUE)
data_2 = read.csv("/Users/willkczy/Econometric/Econometric_git/assignment3/input/MEAPSINGLE.csv")

# (a) The percentage of children not in the married-couples families has a negative impact on percentage of satisfactory level of 4th grade math. It's estimated that one percentage increase in pctsgle will result in a -0.8328 percentage decrease in satisfactory level of 4th grade math, which is a small effect.
lm(data_2$math4 ~ data_2$pctsgle, data = data_2) %>%
  summary()
cat("The percentage of children not in the married-couples families has a negative impact on percentage of satisfactory level of 4th grade math. It's estimated that one percentage increase in pctsgle will result in a -0.8328 percentage decrease in satisfactory level of 4th grade math, which is a small effect. \n", file = "HW3.log", append = TRUE)

# (b) The coefficient of pctsgle has changed from -0.8328 to -0.1996. The coefficient still suggest that an increase in the percentage of children not in married couples, the percentage of satisfactory level of 4th grade math decrease, yet this negative relation is weaken. 
lm(data_2$math4 ~ data_2$pctsgle + data_2$lmedinc + data_2$free, data = data_2) %>%
  summary()
cat("(b) The coefficient of pctsgle has changed from -0.8328 to -0.1996. The coefficient still suggest that an increase in the percentage of children not in married couples, the percentage of satisfactory level of 4th grade math decrease, yet this negative relation is weaken. \n", file = "HW3.log", append = TRUE)

# (c) The correlation is -0.74. It's quite reasonable since when the median income increase, people will be less able to get free lunch.
cor(data_2$lmedinc, data_2$free)
cat("(c) The correlation is -0.74. It's quite reasonable since when the median income increase, people will be less able to get free lunch. \n", file = "HW3.log", append = TRUE)

# (d) No, high correlations between lmedinc and free will not make it more difficult to determine the causal effect of single parenthood on student's math performance.
cat("(d) No, high correlations between lmedinc and free will not make it more difficult to determine the causal effect of single parenthood on student's math performance. \n", file = "HW3.log", append = TRUE) 