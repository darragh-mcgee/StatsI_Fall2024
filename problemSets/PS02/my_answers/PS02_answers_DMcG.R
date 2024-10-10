setwd("C:/Users/darra/OneDrive/Documents/GitHub/Applied_Statistics_1/problemSets/PS02/my_answers")

# Question 1: 
# Part A: 
  
# Step 1: Assumptions
# The data consists of categorical variables (social class and type of police
# interaction)
# The data is from a random sample.
# Observations are independent (one observation does not influence another).
# The two variables are independent if the conditional distributions across 
# categories are identical.

# Step 2: Setting Up Hypothesis
# Null Hypothesis: The relationship between social class and the type of police 
# interaction is statistically independent.
# Alternative Hypothesis: The relationship between social class and the type of 
# police interaction is statistically dependent.

# Part B:

# Step 3: Calculate the Test-Statistic
# Chi-Squared is equal to the sum of the squared difference between the 
# observed frequency and expected frequency, divided by the expected frequency
# for each cell in the contingency table. 

# Observed Frequency Table: This matrix holds the observed counts as provided. 
observed_frequency_table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, 
                                   byrow = TRUE)
# Assign row and column names to match table provided. 
rownames(observed_frequency_table) <- c("Upper class", "Lower class")
colnames(observed_frequency_table) <- c("Not Stopped", "Bribe Requested", 
                                        "Stopped/Given Warning")
# Display the table
observed_frequency_table

# Expected frequency = (row total/grand total) * column total

# Calculate row totals

row_totals <- apply(observed_frequency_table, 1, sum)
row_totals

# Calculate column totals
column_totals <- apply(observed_frequency_table, 2, sum)
column_totals

# Calculate the grand total (sum of all observed frequencies)
grand_total <- sum(observed_frequency_table)
grand_total

# Calculate Expected Frequency
expected_frequency <- (row_totals / grand_total) %*% t(column_totals)
expected_frequency

# Calculate the Test-Statistic using Chi-Squared Formula
chi_squared_statistic <- sum((observed_frequency_table - expected_frequency)^2 
                             / expected_frequency)
chi_squared_statistic

# Step 4: Calculate the p-value
# Calculate the p-value based on the test statistic. 

# Calculate the Degrees of Freedom
# df = (rows-1)*(columns-1)
degrees_of_freedom <- (2-1)*(3-1)
# use the Chi-Squared p-value formula
p_value <- pchisq(chi_squared_statistic, df = degrees_of_freedom, lower.tail=FALSE)
p_value

# Step 5: Conclusions
# The p-value is greater than the significance level of 0.01. Therefore, 
# there is insufficient evidence to reject the null hypothesis that social 
# class and the type of police interaction are statistically independent. 
# This means we fail to conclude that social class and police interactions 
# are dependent. 

# Part C: 
# Calculate the standardised residuals for each cell. 
# Standardised Residuals refer to how far away each observation is from expectation.
# The standardies residual formula is z = (observed frequency - expected frequency)/
# standard error i.e. the square root of the expected frequency

standardised_residual <- (observed_frequency_table - expected_frequency) / 
                         (sqrt(expected_frequency))
standardised_residual

# Part D: 
# How do standardised residuals help to interpret Chi-Squared Statistic and p-value? 

# The expected frequency represents the values that would be expected in each 
# cell of a contingency table if the two categorical variables were independent. 
# Standardised residuals measure how much the observed frequencies deviate from 
# the expected frequencies, helping to identify which cells contribute most to 
# the Chi-squared statistic. 
# Larger standardised residuals indicate greater deviations from expected 
# frequencies, while smaller residuals suggest the observed and expected values 
# are similar.
# In this analysis, the residuals for "Not Stopped" (e.g., 0.136 and -0.183) are small, 
# indicating minimal deviations from expected values and contributing little to 
# the Chi-squared statistic. 
# For "Bribe Requested" (e.g., -0.815 and 1.094) and 
# "Stopped or Given Warning" (e.g., 0.819 and -1.099), the residuals 
# suggest moderate deviations from expected values.
# None of the standardised residuals provide substantial evidence of 
# deviations from independence. As a result, these residuals support a higher 
# p-value and reinforce the conclusion that there is insufficient evidence to 
# reject the null hypothesis.

# Question 2: 
# Part A:
# Step 1: Assumptions about Data
# Linear relationship: There is a linear relationship between the explanatory 
# and response variables.
# Independence: The observations are independent of each other.
# Normally Distributed Errors: For any given value of the independent variable, 
# the errors (residuals) are assumed to follow a normal distribution.
# Constant variance (Homoscedasticity): The variance of the errors is constant 
# across all values of the independent variable.

# Step 2: Setting Up Hypothesis
# Null Hypothesis: The policy of reserving village council head positions for 
# women does not affect the number of new or repaired drinking-water facilities 
# in the village (β = 0)
# Alternative Hypothesis: The policy of reserving village council head positions 
# for women does affect the number of new or repaired drinking-water facilities 
# in the village.(β != 0)

# Part B: Run a Bivariate Regression Analysis
women_policies_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
str(women_policies_data)

Y <- women_policies_data$water
X <- women_policies_data$reserved

png("Figure_2_1.png", width = 1500, height = 950, res = 200)
# Produce Scatterplot to Visualize Data. 
plot(X, Y,
     xlab = "Council Head Position Reserved for Women (No = 0, Yes = 1)",
     ylab = "Number of Drinking-Water Facilities",
     main = "Scatterplot of New or Repaired Drinking Water Facilities versus 
     Council Reservation Policy for Women Leaders")
dev.off()

model <- lm(Y ~ X)
model
summary(model)

# Step 3: Calculating the Test-Statistic
# T-Statistic from summary = 2.344

# Step 4: Calculating the p-value
# p-value from summary = 0.0197

# Step 5: Conclusion
# In a bivariate regression analysis, the slope represents both the strength 
# and direction of the relationship between two variables (an explanatory and 
# response variable). Specifically, in this analysis, the slope illustrates how 
# the policy of reserving village council head positions for women impacts the 
# number of new or repaired drinking-water facilities in the village.
# If the slope is statistically significantly different from 0, it indicates 
# a relationship between the two variables. In this case, the p-value is 0.0197, 
# which is statistically significant at the 95% confidence level (α < 0.05). 
# Therefore, we reject the null hypothesis that the policy of reserving village council 
# head positions for women has no effect on the number of new or repaired 
# drinking-water facilities.
# There is sufficient evidence to conclude that the reservation 
# policy has an impact on the number of new or repaired drinking-water 
# facilities in the village.

# Part C: 
# The coefficient for the reservation policy is 9.252, which represents the 
# slope of the relationship between the reservation policy and the number of 
# new or repaired drinking-water facilities in the village. This coefficient 
# explains how a one-unit change in the explanatory variable 
# (reservation policy) affects the response variable (number of drinking-water 
# facilities).
# The reservation policy is a binary variable (coded as 0 for no reservation 
# policy and 1 for reservation policy). This indicates that, on average, 
# villages that have implemented the reservation policy tend to have 
# approximately 9.252 more new or repaired drinking-water facilities compared 
# to those that have not adopted the policy. 