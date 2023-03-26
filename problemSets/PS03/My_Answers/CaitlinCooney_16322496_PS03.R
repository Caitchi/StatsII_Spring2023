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

lapply(c("dplyr", "MASS", "nnet", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
getwd()

# Read in our data
dat <- read.csv("https://github.com/ASDS-TCD/StatsII_Spring2023/raw/main/datasets/gdpChange.csv")
head(dat)

# 1. Construct and interpret an unordered multinomial logit with GDPWdiff as the output
# and "no change" as the reference category, including the estimated cutoff points and
# coefficients.

# EDA
summary(dat)
ftable(xtabs(~ REG + OIL + GDPWdiff, data = dat))

# Data wrangling 

dat <- mutate(dat, GDPWdiff = ifelse(GDPWdiff > 0, "positive", ifelse(GDPWdiff < 0, "negative", "no change")))

dat$GDPWdiff <- factor(dat$GDPWdiff, 
                              levels = c("no change", "positive", "negative"),
                              ordered = FALSE)

# Construct the multinomial logit model
unord_model <- multinom(GDPWdiff ~ OIL + REG, dat)

# Display the model summary
summary(unord_model)
exp(coef(unord_model))

# Interpretation: 
# For a one-unit increase in OIL, the log-odds of being in the positive category 
# increase by 4.576321 units, holding all other variables constant. 
# Similarly, for a one-unit increase in REG, the log-odds of being in the positive category
# increase by 1.769007 units, holding all other variables constant. 
# The intercept term (4.533759) represents the log-odds of being in the reference category.

# For a one-unit increase in OIL, the log-odds of being in the negative category 
# increase by 4.783968 units, holding all other variables constant. 
# For a one-unit increase in REG, the log-odds of being in the negative category 
# increase by 1.379282 units, holding all other variables constant. 
# The intercept term (3.805370) represents the log-odds of being in the reference category.

# get z and p values

z <- summary(unord_model)$coefficients/summary(unord_model)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# The results suggest that the predictor variable OIL has a statistically significant 
# effect on both the positive and negative categories. 
# The p-values for OIL are 0.5062612 and 0.4871792 for the positive and negative 
# categories, respectively. Since both p-values are greater than the common threshold 
# of 0.05, we cannot reject the null hypothesis that the coefficient for OIL is zero 
# in either category.

# The predictor variable REG, on the other hand, appears to have a statistically 
# significant effect only on the positive category. The p-value for REG is 0.02109459 
# for the positive category, but it is 0.07276308 for the negative category. 
# Since the p-value for the positive category is less than 0.05, 
# we can reject the null hypothesis that the coefficient for REG is zero in 
# the positive category, and conclude that REG has a statistically significant 
# effect on the odds of being in the positive category compared to the reference category. 
# However, we cannot reject the null hypothesis that the coefficient for REG is 
# zero in the negative category.

# we can use predicted probabilities to help interpret our coefficients
pp <- data.frame(fitted(unord_model))
head(data.frame(GDPWdiff = dat$GDPWdiff,
                no.change = pp$no.change,
                positive = pp$positive,
                negative = pp$negative))

dat$REG <- as.factor(dat$REG)

ftable(xtabs(~ REG + OIL + GDPWdiff, data = dat))

# Generate predicted probabilities
pred_probs <- predict(unord_model, newdata = dat, type = "probs")

head(pred_probs)

# Estimate the cutoff points for each category using the quantile function
cutoff_NoChange <- quantile(pred_probs[,1], probs = 0.5)
cutoff_Positive <- quantile(pred_probs[,2], probs = 0.5)
cutoff_Negative <- quantile(pred_probs[,3], probs = 0.5)

# Create table of estimated cutoff points

cutoff_table <- data.frame(
  Category = c("no change", "positive", "negative"),
  Cutoff = c(cutoff_NoChange, cutoff_Positive, cutoff_Negative)
)

print(cutoff_table)

# 2. Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome
# variable, including the estimated cutoff points and coefficients.

# Relevel
# set a reference level for the outcome
dat$GDPWdiff_ord <- factor(dat$GDPWdiff, 
                       levels = c("negative", "no change", "positive"),
                       ordered = FALSE)

ord_model <- polr(GDPWdiff_ord ~ OIL + REG, data = dat, Hess = TRUE)
summary(ord_model)

# Calculate a p value
ctable <- coef(summary(ord_model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord_model))

# convert to odds ratio
exp(cbind(OR = coef(ord_model), ci))

# To estimate cutoff points:
# First, generate predicted probabilities
pred_probs2 <- predict(ord_model, newdata = dat, type = "probs")

head(pred_probs2)

# Estimate the cutoff points for each category using the quantile function
cutoff_Negative2 <- quantile(pred_probs2[,1], probs = 0.5)
cutoff_NoChange2 <- quantile(pred_probs2[,2], probs = 0.5)
cutoff_Positive2 <- quantile(pred_probs2[,3], probs = 0.5)


# Create table of estimated cutoff points

cutoff_table2 <- data.frame(
  Category = c("no change", "positive", "negative"),
  Cutoff = c( cutoff_Negative2, cutoff_NoChange2, cutoff_Positive2)
)

print(cutoff_table2)



#####################
# Problem 2
#####################

mexico <- read.csv("https://github.com/ASDS-TCD/StatsII_Spring2023/raw/main/datasets/MexicoMuniData.csv")
head(mexico)

# (a) Run a Poisson regression because the outcome is a count variable. Is there evidence
# that PAN presidential candidates visit swing districts more? Provide a test statistic
# and p-value.

model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
             data = mexico, family = "poisson")

# Summarize model results
summary(model)

# The resulting z-value for the coefficient of competitive.district is -0.477, 
# and the corresponding p-value is 0.6336. This means that the coefficient is not 
# statistically significant at the 5\% level. This means that there is no evidence 
# of a difference in the expected number of visits between swing and non-swing 
# districts, after controlling for marginality.06 (a measure of economic and social 
# marginality in the district), and whether the district has a PAN governor or not 
# (PAN.governor.06).

# (b) Interpret the marginality.06 and PAN.governor.06 coefficients.

# The marginality.06 coefficient is -2.08014. 
# This means that, holding other variables constant, a one-unit increase in the 
# marginality score (which measures poverty) is associated with a decrease in the 
# expected log-count of PAN visits by 2.08014 units. In other words, the more 
# impoverished a district is, the less likely it is that the PAN presidential 
# candidate visited it.

# The PAN.governor.06 coefficient is -0.31158. 
# This means that, holding other variables constant, being in a state with a 
# PAN-affiliated governor is associated with a decrease in the expected log-count 
# of PAN visits by 0.31158 units. However, this coefficient is only marginally 
# significant (p-value = 0.0617), meaning that we cannot say with certainty that 
# the presence of a PAN-affiliated governor had a significant effect on the number 
# of PAN visits.

# (c) Provide the estimated mean number of visits from the winning PAN presidential candi-
# date for a hypothetical district that was competitive (competitive.district=1), had
# an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).


# Create a data frame with the predictor values for the hypothetical district
hypothetical_district <- data.frame(competitive.district = 1, 
                                    marginality.06 = 0, 
                                    PAN.governor.06 = 1)

# Use the predict() function to estimate the mean number of visits for the hypothetical district

pred_mex <- cbind(predict(model, hypothetical_district , type ="response", se.fit = TRUE), hypothetical_district)

# Print the estimated mean number of visits
print(pred_mex)

# Alternative model using the model equation:

# log (E(Y | X)) = β0 + β1 X1 + β2 X2 + β3 X3
model.equation = -3.81023 - 0.08135*1 - 2.08014*0 - 0.31158*1
model.equation

exp(model.equation)

# where Y is the number of PAN visits, X1 is competitive.district, X2 is marginality.06, X3 is PAN.governor.06, and β0, β1, β2, and β3 are the estimated coefficients from the model.


