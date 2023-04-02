Multinomial Logistic Regression (MLR) in R

Read the file

library("foreign")
data <- read.csv("https://github.com/ASDS-TCD/StatsII_Spring2023/raw/main/datasets/gdpChange.csv")
head(data)

# Re-leveling data

head(data)

data <- mutate(data, GDPWdiff = ifelse(GDPWdiff > 0, "positive", ifelse(GDPWdiff < 0, "negative", "no change")))

data$GDPWdiff <- factor(data$GDPWdiff, 
                       levels = c("no change", "positive", "negative"),
                       ordered = FALSE)


# Now we’ll execute a multinomial regression with two independent variable.

library("nnet")

test <- multinom(GDPWdiff ~ REG + OIL, data = data)

summary(test)

## Call:
## multinom(formula = prog2 ~ ses + write, data = ml)
##
## Coefficients:
##          (Intercept)  sesmiddle    seshigh      write
## general     2.852198 -0.5332810 -1.1628226 -0.0579287
## vocation    5.218260  0.2913859 -0.9826649 -0.1136037
##
## Std. Errors:
##          (Intercept) sesmiddle   seshigh      write
## general     1.166441 0.4437323 0.5142196 0.02141097
## vocation    1.163552 0.4763739 0.5955665 0.02221996
##
## Residual Deviance: 359.9635
## AIC: 375.9635


Interpretation

1. Model execution output shows some iteration history and includes the final 
negative log-likelihood 179.981726. This value is multiplied by two as shown in 
the model summary as the Residual Deviance.

2. The summary output has a block of coefficients and another block of standard errors. 
Each blocks has one row of values corresponding to one model equation. 
In the block of coefficients, we see that the first row is being compared to prog = “general” 
to our baseline prog = “academic” and the second row to prog = “vocation” to our baseline 
prog = “academic”.

3. A one-unit increase in write decreases the log odds of being in general 
program vs. academic program by 0.0579

4. A one-unit increase in write decreases the log odds of being in vocation 
program vs. academic program by 0.1136

5. The log odds of being in general program than in academic program will 
decrease by 1.163 if moving from ses=”low” to ses=”high”.

6. On the other hand, Log odds of being in general program than in academic 
program will decrease by 0.5332 if moving from ses=”low” to ses=”middle”

7. The log odds of being in vocation program vs. in academic program will 
decrease by 0.983 if moving from ses=”low” to ses=”high”

8. The log odds of being in vocation program vs. in academic program will 
increase by 0.291 if moving from ses=”low” to ses=”middle”

Now we’ll calculate Z score and p-Value for the variables in the model.

z <- summary(test)$coefficients/summary(test)$standard.errors
z

##          (Intercept)  sesmiddle   seshigh     write
## general     2.445214 -1.2018081 -2.261334 -2.705562
## vocation    4.484769  0.6116747 -1.649967 -5.112689

p <- (1 - pnorm(abs(z), 0, 1))*2
p

##           (Intercept) sesmiddle    seshigh        write
## general  0.0144766100 0.2294379 0.02373856 6.818902e-03
## vocation 0.0000072993 0.5407530 0.09894976 3.176045e-07

exp(coef(test))

##          (Intercept) sesmiddle   seshigh     write
## general     17.32582 0.5866769 0.3126026 0.9437172
## vocation   184.61262 1.3382809 0.3743123 0.8926116

The p-Value tells us that ses variables are not significant.  

pred_probs <- predict(unord_model, type = 'probs')
cutoff_points <- t(apply(pred_probs, 2, function(x) quantile(x, probs = 0.5)))

colnames(cutoff_points) <- c("no change", "negative", "positive")
rownames(cutoff_points) <- names(unord_model$coefficients)

################
# Question 2
################

Let’s quickly understand the data.

The data set has a dependent variable known as apply. It has 3 levels namely “unlikely”, “somewhat likely”, and “very likely”, coded in 1, 2, and 3 respectively. 3 being highest and 1 being lowest. This situation is best for using ordinal regression because of presence of ordered categories. Pared (0/1) refers to at least one parent has a graduate degree; public (0/1) refers to the type of undergraduate institute.

For building this model, we will be using the polr command to estimate an ordered logistic regression. Then, we’ll specify Hess=TRUE to let the model output show the observed information matrix from optimization which is used to get standard errors.

ord.log <- polr(GDPWdiff ~ REG + OIL, data = data, Hess=TRUE)
summary(ord.log)

## Call:
## polr(formula = apply ~ pared + public + gpa, data = dat, Hess = TRUE)
##
## Coefficients:
##           Value Std. Error t value
## pared   1.04769     0.2658  3.9418
## public -0.05879     0.2979 -0.1974
## gpa     0.61594     0.2606  2.3632
##
## Intercepts:
##                             Value   Std. Error t value
## unlikely|somewhat likely     2.2039  0.7795     2.8272
## somewhat likely|very likely  4.2994  0.8043     5.3453
##
## Residual Deviance: 717.0249
## AIC: 727.0249

We see the usual regression output coefficient table including the value of each coefficient, standard errors, t values, estimates for the two intercepts, residual deviance and AIC. AIC is the information criteria. Lesser the better.

Now we’ll calculate some essential metrics such as p-Value, CI, Odds ratio

ctable <- coef(summary(ord.log))

##                                   Value Std. Error    t value
## pared                        1.04769010  0.2657894  3.9418050
## public                      -0.05878572  0.2978614 -0.1973593
## gpa                          0.61594057  0.2606340  2.3632399
## unlikely|somewhat likely     2.20391473  0.7795455  2.8271792
## somewhat likely|very likely  4.29936315  0.8043267  5.3452947

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

##                                   Value Std. Error    t value      p value
## pared                        1.04769010  0.2657894  3.9418050 8.087072e-05
## public                      -0.05878572  0.2978614 -0.1973593 8.435464e-01
## gpa                          0.61594057  0.2606340  2.3632399 1.811594e-02
## unlikely|somewhat likely     2.20391473  0.7795455  2.8271792 4.696004e-03
## somewhat likely|very likely  4.29936315  0.8043267  5.3452947 9.027008e-08

# confidence intervals
> ci <- confint(m)

## Waiting for profiling to be done...

##             2.5 %    97.5 %
## pared   0.5281768 1.5721750
## public -0.6522060 0.5191384
## gpa     0.1076202 1.1309148

exp(coef(ord.log))

##     pared    public       gpa
## 2.8510579 0.9429088 1.8513972

## OR and CI
exp(cbind(OR = coef(ord.log), ci))

##               OR     2.5 %   97.5 %
## pared  2.8510579 1.6958376 4.817114
## public 0.9429088 0.5208954 1.680579
## gpa    1.8513972 1.1136247 3.098490
Interpretation

1. One unit increase in parental education, from 0 (Low) to 1 (High), the odds of “very likely” applying versus “somewhat likely” or “unlikely” applying combined are 2.85 greater .

2. The odds “very likely” or “somewhat likely” applying versus “unlikely” applying is 2.85 times greater .

3. For gpa, when a student’s gpa moves 1 unit, the odds of moving from “unlikely” applying to “somewhat likely” or “very likley” applying (or from the lower and middle categories to the high category) are multiplied by 1.85.