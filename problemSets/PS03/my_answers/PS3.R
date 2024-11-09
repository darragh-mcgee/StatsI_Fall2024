# Set Working Directory
setwd("C:/Users/darra/OneDrive/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers")

# Read in Data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

str(inc.sub)

# Assumptions Underlying Linear Regression:
# Linear Relationship: There is a linear relationship between the outcome and 
# explanatory variables.
# Independence of Errors: The errors (residuals) are independent of each other. 
# Normality of errors: For any given value of the explanatory variable, the
# errors (residuals) are assumed to follow a normal distribution.
# Constant variance (Homoscedasticity): The variance of the errors is constant 
# across all values of the explanatory variable
# No Perfect Multi-Collinearity: The explanatory variables should not be perfectly 
# correlated with each other. 

# Question 1:
# Part 1:
model_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_1) 

# The intercept of 0.579031 indicates that when the logarithmic difference in campaign 
# spending between the incumbent and challenger is zero, the incumbent's 
# vote share is predicted to be 57.9%.

# There is a positive, statistically significant relationship between the log-transformed difference in 
# campaign spending (Incumbent - Challenger) and the incumbent's vote share, 
# with a p-value of less than 0.001 (indicated by three stars in the regression table). 
# Specifically, a one-unit increase in the logged difference in campaign spending is 
# associated with an average increase of 0.04 (or 4%) in the incumbent’s vote share.

# Part 2: 
# Plotting the Relationship:
png("Figure_1_1.png", width = 1500, height = 950, res = 200)
plot(voteshare ~ difflog, data = inc.sub, 
     xlab = "Difference in Campaign Spending (Incumbent - Challenger)",
     ylab = "Incumbent Vote Share",
     main = "Scatterplot of Effect of Campaign Spending Difference 
     on Incumbent Vote Share")
abline(model_voteshare_difflog, col = "blue", lwd = 1)
dev.off()

# Part 3:
# Save the Residuals as a Separate Object
residuals_model_1 <- residuals(model_1)

# Part 4:
# Write the Prediction Equation 
# voteshare = 0.57903 + 0.4167*difflog

# Question 2: 
# Part A: 
model_2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model_2)

# The intercept of 0.57583 indicates that when the logarithmic difference in campaign 
# spending between the incumbent and challenger is zero, the vote share of the 
# presidential candidate of the incumbent is predicted to be 57.58%.

# There is a positive, statistically significant relationship between the log-transformed difference in 
# campaign spending (Incumbent - Challenger) and the vote share of the presidential 
# candidate of the incumbent, with a p-value of less than 0.001 
# (indicated by three stars in the regression table). Specifically, a one-unit 
# increase in the logged difference in campaign spending is associated with an average 
# increase of 0.0238 (or 2.38%) in the presidential candidate of the incumbent's 
# vote share.

# Part B:
# Plot the Relationship:
png("Figure_2_1.png", width = 1500, height = 950, res = 200)
plot(presvote ~ difflog, data = inc.sub, 
     ylab = "Vote Share of Presidential Candidate of Incumbent",
     xlab = "Difference in Campaign Spending (Incumbent - Challenger)", 
     main = "Scatterplot of Effect of Campaign Spending Difference on Vote 
     Share of Presidential Candidate of Incumbent")
abline(model_voteshare_difflog, col = "green", lwd = 1)
dev.off()

# Part C: 
residuals_model_2 <- residuals(model_2)

# Part D: 
# Write the Prediction Equation 
# presvote = 0.50758 + 0.0238*difflog

# Question 3: 
# Part A: 
model_3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_3)

# The intercept of 0.4413 indicates that when the vote share of the incumbent’s 
# presidential candidate is zero, the predicted vote share of the incumbent is 44.13%

# There is a positive, statistically significant relationship between the vote share of 
# the incumbent’s presidential candidate and the incumbent’s vote share, with a 
# p-value of less than 0.001 (indicated by three stars in the regression table). 
# Specifically, a one-unit increase in the vote share of the incumbent’s presidential 
# candidate is associated with an average increase of 0.388 (or 38.8 percentage points) 
# in the incumbent’s vote share.

# Part B:
# Plot the Relationship:
png("Figure_3_1.png", width = 1500, height = 950, res = 200)
plot(presvote ~ difflog, data = inc.sub, 
     xlab = "Vote Share of Presidential Candidate of Incumbent", 
     ylab = "Incumbent Vote Share", 
     main = "Scatterplot of Effect Vote Share of Presidential Candidate of 
     Incumbent on Incumbent Vote Share")
abline(model_voteshare_difflog, col = "red", lwd = 1)
dev.off()

# Part C: 
residuals_model_3 <- residuals(model_3)

# Part D:
# Write the Prediction Equation 
# voteshare = 0.4413 + 0.3880*presvote

# Question 4: 
# Part A:
model_4 <- lm(residuals_model_1 ~ residuals_model_2)
summary(model_4)

# There is a positive, statistically significant relationship between the 
# residuals from model 1 and model 2, with a p-value of less than 0.001 
# (indicated by three stars in the regression table).Specifically, a one-unit 
# increase in the residual (error) from model 2 is associated with an average 
# increase of 0.2569 in the residual (error) from model 1.  

# Part B: 
# Plot the Relationship:
png("Figure_4_1.png", width = 1500, height = 950, res = 200)
# Adjust Margins to Prevent Cutting Off Label
par(mar = c(5, 7, 5, 3))
plot(residuals_model_1 ~ residuals_model_2, data = inc.sub, 
     xlab = "Variation in Presidential Candidate's Vote Share 
     Not Explained by Campaign Spending",
     ylab = "Variation in Incumbent's Vote Share 
     Not Explained by Campaign Spending", 
     main = "Scatterplot of Unexplained Variation in Incumbent Vote Share 
     versus Unexplained Variation in Presidential Candidate's Vote Share")
abline(model_voteshare_difflog, col = "purple", lwd = 1)
dev.off()

# Part C:
# Write the Prediction Equation 
# residuals_model_1 = 0 + 0.2569*residuals_model_2 

# Question 5: 
# Part A: 
model_5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_5)

# Part B: 
# Write the Prediction Equation 
# voteshare = 0.44864 + 0.03554*difflog + 0.25688*presvote

# Part C:
# The coefficients for residuals_model_2 in Model 4 and presvote in Model 5 are 
# the same because they are both measuring the same underlying relationship, 
# i.e., the co-variation between the presidential candidate’s vote share 
# (presvote) and the incumbent’s vote share (voteshare) that is unexplained by 
# the differences in campaign spending. In Model 4, this relationship is 
# measured through the residuals (unexplained variation) from both models, 
# while in Model 5, captures the partial effect of presvote directly. 