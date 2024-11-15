# Set Working Directory
setwd("C:/Users/darra/OneDrive/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")

# Clear Global Environment:
rm(list = ls())


# Install Relevant Libraries
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

summary(Prestige)
str(Prestige)

# Part A: 
# Create New Variable "professional" using ifelse
# as.factor used to convert numeric binary variable into categorical predictor.
# prof returned as 1. 
# %in% used to check multiple conditions (i.e. bc and wc) and return 0. 
# missing cases included as NA. 
Prestige$professional <- as.factor(ifelse(Prestige$type == "prof", 1, 0))
print(Prestige$professional)

# Part B: 
model_1 <- lm(prestige ~ income*professional, data = Prestige)
summary(model_1)

# Part C:
# Prediction Equation: 
# prestige = 21.142 + 0.003*income + 37.781*professional - 0.002*income*professional

# Part D:
# An interaction term moderates the individual effect of each explanatory
# variable by capturing how the relationship between the explanatory variable and 
# outcome depends on another explanatory variable. Therefore, the coefficients 
# of individual explanatory variables represent the isolated effects when the
# interacting variable is 0. 

# When the individual does not work as a professional (professional = 0) a one 
# unit (i.e. one dollar) increase in income is associated with a 0.003 increase 
# in the individuals prestige score.

# Part E: 
# When income is equal to zero, having a professional job (professional = 0) is 
# associated with a 37.781 increase in prestige score. However, in reality, 
# it is highly unlikely for an individual to hold a professional job with no income. 
# Nonetheless, it represents the baseline difference between professionals and 
# blue collar and white collar workers controlling for income. 

# Part F
answer_f <-  21.142 + 0.003*(1000) + 37.781*(1) - 0.002*(1000)*(1)
print(answer_f)

# Baseline prestige score for a professional with $0 income
baseline_prestige <- 21.142 + 37.781
print(baseline_prestige)

# Difference in prestige from a $1,000 increase in income for professionals
prestige_increase <- answer_f - baseline_prestige
print(prestige_increase)


# A professional's prestige score is equal to 58.923 (21.142 + 37.781) when their
# income is zero. Therefore, the increase in prestige score associated
# with an increase in income of $1000 for a professional can be estimated as 
# 1 (59.923 - 58.923). 

# The marginal effect for each dollar increase in income for professionals can be
# calculated by dividing 1 by 1000, which gives a value of 0.001. 

# The marginal increase in prestige for each dollar increase in income for 
# professionals can be directly verified from the prediction equation itself: 
# (0.003 - 0.002) = 0.001. 

# Part G: 
# Predicted Prestige Score for Non-Professional Occupations with 6000 dollar Income
answer_g_non_professional <- 21.142 + 0.003*(6000) + 37.781*(0)- 0.002*(6000)*(0)
print(answer_g_non_professional)

# Predicted Prestige Score for Professional Occupations with 6000 dollar Income
answer_g_professional <- 21.142 + 0.003*(6000) + 37.781*(1) - 0.002*(6000)*(1)
print(answer_g_professional)

# Marginal Effect of Professional Occupation on Prestige when Earning 6,000 dollars
prestige_increase_g <- answer_g_professional - answer_g_non_professional
print(prestige_increase_g)

# Explanation:
# The prestige score for a non-professional with a $6,000 income is 39.142, while 
# it is 64.923 for a professional with the same income. Therefore, the marginal 
# effect of switching from a non-professional to a professional occupation at 
# this income level results in a prestige increase of 25.781 (64.923 - 39.142). 

# Part A:
# Assumptions:
# Linear Relationship: There is a linear relationship between the outcome and 
# explanatory variables.
# Independence of Errors: The errors (residuals) are independent of each other. 
# Normality of errors: For any given value of the explanatory variable, the
# errors (residuals) are assumed to follow a normal distribution.
# Constant variance (Homoscedasticity): The variance of the errors is constant 
# across all values of the explanatory variable
# No Perfect Multi-Collinearity: The explanatory variables should not be perfectly 
# correlated with each other. 

# Stating Hypotheses:
# Null Hypothesis (H0): Having yard signs in a precinct does NOT affect vote 
# share (β = 0).
# Alternative Hypothesis (Ha): Having yard signs in a precinct DOES affect vote 
# share (β ≠ 0).

# Calculate the Test Statistic
TS_1 <- (0.042-0)/0.016
print(TS_1)

# Calculate the p-value
p_value_1 <- 2*pt(abs(TS_1), 131-2, lower.tail = F)
print(p_value_1)

# Conclusion: 
# Reject the Null Hypothesis as the p-value is less than 0.05 (below the 
# significance level).  
# There is sufficient evidence to conclude that having yard signs in a precinct 
# does effect vote share. 

# Part B: 
# Stating Hypotheses:
# Null Hypothesis (H0): Being next to precincts with yard signs does NOT affect 
# vote share (β = 0).
# Alternative Hypothesis (Ha): Being adjacent to precincts with yard signs DOES 
# affect vote share (β ≠ 0).

# Calculate the Test Statistic
TS_2 <- (0.042-0)/0.013
print(TS_2)

# Calculate the p-value
p_value_2 <- 2*pt(abs(TS_2), 131-2, lower.tail = F)
print(p_value_2)

# Conclusion: 
# Reject the Null Hypothesis as the p-value is less than 0.05 (below the 
# significance level).  
# There is sufficient evidence to conclude that being adjacent to precincts with 
# yard signs DOES affect vote share. 

# Part C:
# Proportion of Vote Share = 0.302 + 0.042*Precincts Assigned Lawn Signs + 0.042*Precincts Adjacent to Lawn Signs

# The constant (0.302) represents the expected proportion of the vote share that 
# went to McAuliff's opponent, Ken Cuccinelli, in precincts that neither had 
# yard signs nor were adjacent to precincts with yard signs.

# Part D: 
# The R-squared value (i.e., the proportion of variance in the outcome variable 
# explained by the explanatory variables in the model) is 0.094. This means 
# that less than 10 percent of the variance in vote share is explained by the presence 
# of yard signs in the precinct or being adjacent to a precinct with yard signs. 
# The relatively low R-squared value suggests that other factors likely have a more 
# substantial impact on vote share.