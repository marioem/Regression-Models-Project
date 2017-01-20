# Question 1
# Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use 
# of the autolander as the outcome (variable name use). Fit a logistic regression model 
# with autolander (variable auto) use (labeled as "auto" 1) versus not (0) as predicted 
# by wind sign (variable wind). Give the estimated odds ratio for autolander use comparing 
# head winds, labeled as "head" in the variable headwind (numerator) to tail winds 
# (denominator).
# 
# 0.031
# -0.031
# 0.969 <-
# 1.327
# 
# Solution:

library(MASS)
library(gdata)
sh <- shuttle
mapLevels(sh$use)

# We need to convert internal representation of the use levels to auto = 1, noauto = 2.
# In this case we simply need to convert use variable to integer and substract 2 from the 
# observations where it has value of 2.

sh$use <- as.integer(sh$use)
sh$use[sh$use == 2] <- sh$use[sh$use == 2] - 2

fit <- glm(use ~ wind, data = sh, family = binomial)
summary(fit)$coef

# Answer:
exp(coef(fit)[1])/exp(coef(fit)[1]+coef(fit)[2])

# Question 2
# Consider the previous problem. Give the estimated odds ratio for autolander use comparing 
# head winds (numerator) to tail winds (denominator) adjusting for wind strength from 
# the variable magn.
# 
# 1.485
# 1.00
# 0.684
# 0.969 <-
# 
# Solution:

fit <- glm(use ~ wind + magn, sh, family = binomial)
summary(fit)$coef

# Answer:
exp(coef(fit)[1])/exp(coef(fit)[1]+coef(fit)[2])

# Question 3
# If you fit a logistic regression model to a binary variable, for example use of 
# the autolander, then fit a logistic regression model for one minus the outcome (not 
# using the autolander) what happens to the coefficients?
# 
# The coefficients get inverted (one over their previous value).
# The coefficients reverse their signs. <-
# The intercept changes sign, but the other coefficients don't.
# The coefficients change in a non-linear fashion.
# 
# Solution:
fit <- glm(use ~ wind, data = sh, family = binomial)
summary(fit)$coef

fit <- glm(I(1-use) ~ wind, data = sh, family = binomial)
summary(fit)$coef

# Answer: Coefficients change sign

# Question 4
# Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor 
# level. Report the estimated relative rate comapring spray A (numerator) to spray B 
# (denominator).
# 
# 0.321
# -0.056
# 0.136
# 0.9457 <- 
# 
# Solution:

fit <- glm(count ~ spray, data = InsectSprays, family = poisson)
summary(fit)

exp(coef(fit)[1])/exp(coef(fit)[1]+coef(fit)[2])

# Question 5
# Consider a Poisson glm with an offset, t. So, for example, a model of the form 
# glm(count ~ x + offset(t), family = poisson) where x is a factor variable comparing 
# a treatment (1) to a control (0) and t is the natural log of a monitoring time. 
# What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), 
# family = poisson) where t2 <- log(10) + t? In other words, what happens to 
# the coefficients if we change the units of the offset variable. (Note, adding log(10) 
# on the log scale is multiplying by 10 on the original scale.)
# 
# The coefficient is subtracted by log(10).
# The coefficient estimate is divided by 10.
# The coefficient estimate is unchanged <-
# The coefficient estimate is multiplied by 10.
# 
# Solution: The coefficients are estimations of the ratios, so their value is unchanged
# 
# 
# Question 6
# Consider the data
# 
# x <- -5:5
# y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
# Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0. Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?
# 
# 1.013 <- 
# 2.037
# -1.024
# -0.183
# 
# Solution:
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)
length(x)
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
summary(lm(y ~ xMat - 1))

# Answer: slope for the positive x is xMatx + xMat = -1.02416 + 2.03723

