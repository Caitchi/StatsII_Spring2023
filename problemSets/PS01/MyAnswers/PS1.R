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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# Write an R function that implements this test where the reference distribution
# is normal. 
# Using R generate 1,000 Cauchy random variables and perform the test.

set.seed(123)
data <- (rcauchy(1000, location = 0, scale = 1))

kolsmir.test <- function(data) {
  # Create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  # Calculate p-value
  p.value <- 1 - pnorm(sqrt(length(data)) * D)
  
  return(list(D = D, p.value = p.value))
}


kolsmir.test(data)

# As D is not greater than the critical value, we cannot reject the null hypothesis. 


#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Estimate an OLS regression in R that uses the Newton-Raphson algorithm 
# (specifically BFGS, which is a quasi-Newton method), and show that you 
# get the equivalent results to using lm.

linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta [1:k]
  sigma2<- theta [k+1] ^2
  e <- y-X%*%beta
  logl<- -.5*n*log( 2*pi )-.5*n*log( sigma2 )-((t(e) %*% e)/(2*sigma2))
  return(-logl)
}

linear.MLE <- optim(fn = linear.lik, par = c(1, 1, 1), 
                       hessian = TRUE, y = data$y , X = cbind(1, data$x), 
                       method = "BFGS")

# Find parameters that specify this point
linear.MLE$par


# Equivalent results to using lm
summary(lm(y~x, data))








