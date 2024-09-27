#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# Part 1
# 90% Confidence Interval = Point Estimate i.e. Mean +/- Margin of Error 
# (Critical Value*Standard Error)
# Input Data Set of Student IQs and Create Vector
Student_IQ <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 
                112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

length(Student_IQ)
hist(Student_IQ)

# Calculating Mean Student Height
mean_IQ <- mean(Student_IQ)
print(mean_IQ)

# Calculating Standard Error
standard_error_IQ <- sd(Student_IQ)/sqrt(length(Student_IQ))
print(standard_error_IQ)

# Calculate Test Statistic for 90% Confidence Level
# As the sample size is 25 (Less than 30) a T-Distribution should be used for 
# Critical Value.  

# T-Statistic Formula requires Degrees of Freedom
# Degrees of freedom (df = n - 1)
df <- 25 - 1
print(df)

# T-Statistics for 90% Confidence Interval is interested in the Critical Value 
# for the First 5% and Last 5% of the Distribution 
t_score_lower <- qt(0.05, df) # Critical Value for First 5% (Lower Bound)
print(t_score_lower)
t_score_upper <- qt(0.95, df) # Critical Value for Last 5% (Upper Bound)
print(t_score_upper)

# Construct 90% Confidence Interval: 
lower_bound <- (mean_IQ+(t_score_lower)*(standard_error_IQ))
print(lower_bound)
upper_bound <- (mean_IQ+(t_score_upper)*(standard_error_IQ))
print(upper_bound)

# 90% Confidence Interval = 93.95933 to 102.9201

# Integrated Confidence Interval Methodology (To Validate Results)
t.test(Student_IQ, conf.level = 0.9, alternative = "two.sided")

# Part 2

# Step 1: Assumptions about Data
# Sample Size is below 30. Therefore, T-Statistic should be used. 
# Population IQ = 100
# IQ represents continuous data.
# Random Sampling Conducted
# The data is approximately normally distributed. 
# Observations are independent of one another. 

Population_Mean_IQ <- 100

# Step 2: Setting Up Hypothesis
# Null Hypothesis: The average Student IQ in the sample school is less than or 
# equal to the average IQ score (100) among all the schools in the country 
# Alternative Hypothesis: The average student IQ in the sample school is greater 
# than the average IQ score (100) among all the schools in the country

# Step 3: Calculate the t-statistic 
t_statistic <- ((mean_IQ - Population_Mean_IQ) / standard_error_IQ)
print(t_statistic)

# Step 4: Calculate p-value
p_value <- pt(t_statistic, df, lower.tail = FALSE)
print(p_value)

#Step 5: Conclusion  
# Fail to reject the Null Hypothesis as the p-value is greater than 0.05 
# (exceeding the 5% significance level). 
# There is insufficient evidence to conclude that the sample mean is greater 
# than 100.

# Calculating Test Statistic and P-Value using T-Test formula to validate answers 
t.test(Student_IQ, mu = 100, alternative = c("greater"), conf.level=0.95)

#####################
# Problem 2
#####################

# Load Data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# Examine the Data Structure
str(expenditure)

# Plotting each 
png("Figure 1.1.png", width = 1500, height = 950, res = 200)
plot(expenditure$X1, expenditure$Y, col =1, 
     ylab = "per capita expenditure on housing assistance in state",  
     xlab="per capita personal income in state",
     main="Per capita expenditure on housing assistance 
     versus personal income in state")

dev.off()
# A moderate positive correlation is observable between per capita personal 
# income in state and per capita expenditure on housing assistance in state. 

plot(expenditure$X2, expenditure$Y, col =2, 
     ylab = "per capita expenditure on housing assistance in state",  
     xlab="financially insecure residents per 100,000 in state",
     main="Per capita expenditure on housing assistance 
     versus financially insecure residents per 100,000 in state")

# A moderate positive correlation is observable between per capita expenditure 
# on housing assistance and financially insecure residents per 100,000 in the 
# state.

plot(expenditure$X3, expenditure$Y, col =3, 
     ylab = "per capita expenditure on housing legassistance in state",  
     xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance 
     versus people residing in urban areas per 1000 in state")

# A moderate positive correlation is observable between people residing 
# in urban areas per 1000 in state and per capita expenditure on housing legal 
# assistance in state. 

plot(expenditure$X1, expenditure$X2, col =4, 
     ylab = "per capita personal income in state",  
     xlab="financially insecure residents per 100,000 in state",
     main="Per capita expenditure on housing assistance 
     versus personal income")

# There is no clear linear correlation observable between the per capita 
# personal income in state and financially insecure residents per 100,000 in 
# state. 

plot(expenditure$X1, expenditure$X3, col =5, 
     ylab = "per capita personal income in state",  
     xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance 
     versus personal income")

# A moderate positive correlation is observable between people residing in 
# urban areas per 1000 in state and per capita personal income in state. 

plot(expenditure$X2, expenditure$X3, col =6, 
     ylab = "financially insecure residents per 100,000 in state",  
     xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance 
     versus personal income")

# There is no clear linear correlation observable between people residing in 
# urban areas per 1000 in state and financially insecure residents per 100,000 
# in state.

# Calculate correlation Co-efficient to provide numerical validation of 
# graphical interpretation: 
cor(expenditure$X1, expenditure$Y)
cor(expenditure$X2, expenditure$Y)
cor(expenditure$X3, expenditure$Y)
cor(expenditure$X1, expenditure$X2)
cor(expenditure$X1, expenditure$X3)
cor(expenditure$X2, expenditure$X3)

# Produce Box Plot to compare averages between regions. 

boxplot(expenditure$Y ~ expenditure$Region, 
        main="Per capita expenditure on housing assistance per region",
        ylab="Region",
        xlab="Per capita expenditure on housing assistance",
        names=c("Northeast", "North Central", "South", "West"))

# The box plot visualises the interquartile range (IQR) - range from the 
# 25th percentile (Q1) to the 75th percentile (Q3). 
# The West Region has the highest median per capita expenditure on housing 
# assistance. 
# There is wide variation in the data for this region, as evidenced by the 
# largest Interquartile Range across Regions. 


plot(expenditure$X1, expenditure$Y, col =1, 
     ylab = "per capita expenditure on housing assistance in state",  
     xlab="per capita personal income in state",
     main="Per capita expenditure on housing assistance 
     \nversus personal income in state")

# A moderate positive correlation is observable between per capita personal 
# income in state and per capita expenditure on housing assistance in state. 
# The data points are relatively spread out, indicating moderate variability in 
# expenditure at similar income levels.  
# There are some outliers in the data, particularly at higher end of per capita 
# personal income in state. 

# Creating vector of colours for identification
colours <- c("Northeast" = 1, "North Central" = 2, "South" = 3, "West" = 4)
icons <- c("Northeast" = 1, "North Central" = 2, "South" = 3, "West" = 4)

plot(expenditure$X1, expenditure$Y, col = colours[expenditure$Region], 
     pch = icons[expenditure$Region], 
     ylab = "per capita expenditure on housing assistance in state", 
     xlab = "per capita personal income in state",
     main = "Per capita expenditure on housing assistance 
     versus personal income in state")

# Adding in a Legend to Identify which Icons and colours relate to each Region
legend("topleft", legend =c("Northeast", "North Central", "South", "West"), 
       col = c(1, 2, 3, 4), pch = c(1, 2, 3, 4), title = "Region")

# Overall, there is a general upward trend, indicating that states with higher 
# personal income per capita tend to spend more on housing assistance per capita.
# The Northeast and West Regions show higher-income states spending more on 
# housing assistance.
# North Central states show moderate expenditures on housing assistance at 
# moderate income levels, with some variability.
# Southern States display tend to have lower income and lower expenditure. 

setwd("C:\\Users\\darra\\Documents\\GitHub\\Applied_Statistics_1\\problemSets\\PS01\\my_answers")
