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

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####################
# Problem 2
#####################

# Load Data: 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

str(expenditure)

par(mfrow = c(2,4))

plot(expenditure$X1, expenditure$Y, col =1, ylab = "per capita expenditure on housing assistance in state",  xlab="per capita personal income in state",
     main="Per capita expenditure on housing assistance versus personal income in state")

plot(expenditure$X2, expenditure$Y, col =2, ylab = "per capita expenditure on housing assistance in state",  xlab="financially insecure residents per 100,000 in state",
     main="Per capita expenditure on housing assistance versus financially insecure residents per 100,000 in state")

plot(expenditure$X3, expenditure$Y, col =3, ylab = "per capita expenditure on housing assistance in state",  xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance versus people residing in urban areas per 1000 in state")

plot(expenditure$X1, expenditure$X2, col =1, ylab = "per capita personal income in state",  xlab="financially insecure residents per 100,000 in state",
     main="Per capita expenditure on housing assistance versus personal income")

plot(expenditure$X1, expenditure$X3, col =1, ylab = "per capita personal income in state",  xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance versus personal income")

plot(expenditure$X2, expenditure$X3, col =1, ylab = "financially insecure residents per 100,000 in state",  xlab="people residing in urban areas per 1000 in state",
     main="Per capita expenditure on housing assistance versus personal income")
