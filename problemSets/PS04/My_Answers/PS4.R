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

# load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggplot2", "ggfortify", "stargazer", "survminer"),  pkgTest)


#####################
# Problem 1
#####################

# We're interested in modeling the historical causes of child mortality. We have data from
# 26855 children born in Skelleftea, Sweden from 1850 to 1884. Using the "child" dataset in
# the eha library, fit a Cox Proportional Hazard model using mother's age and infant's gender
# as covariates. Present and interpret the output.

# Load data
data(child)
head(child)

# Build a survival object out of the `child` data.frame
child_surv <- with(child, Surv(enter, exit, event))

# Plot 
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

km_mothersage <- survfit(child_surv ~ m.age, data = child)
summary(km_mothersage)
plot(km_mothersage)


child$age_group <- cut(child$m.age, breaks = seq(15, 55, 5), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54"))
km_mothersage <- survfit(child_surv ~ age_group, data = child)
summary(km_mothersage)
plot(km_mothersage)


# Run a Cox Proportional Hazard regression
cox <- coxph(child_surv ~ m.age + sex, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

stargazer(cox, type = "text", coef = exp(coef(cox)))
stargazer(cox, type = "text", coef = exp(coef(cox)))
stargazer(cox, type = "text", coef = list(exp(coef(cox))))


