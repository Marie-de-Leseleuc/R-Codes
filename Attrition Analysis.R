
#Set working directory

setwd("C:\\Users\\mdeleseleuc\\Documents")

# loading data into memory

attrdata <- read.csv(file = "attrition.csv", head = TRUE, sep = ";")


# Fitting Generalized Linear Model to the data
fitlogit <- glm(Churn ~ Gender + Age  + Income  + FamilySize +  Education   + Calls +   Visits, data = attrdata, family = "binomial")

# Summerizing results
summary(fitlogit)

# Clearly we can observe in the summary that all the variables are significant at least at 95% Confidence.

# Analysis of variances
round(x = anova(fitlogit), digits = 4)

library(aod)
# Variance - Covariance table/matrix
round(x = vcov(fitlogit), digits = 4)

# Coefficient of variables in fitted model
round(x = coef(fitlogit), digits = 4)

# Confidence Intervals using profiled log-likelihood in the test
round(x = confint(fitlogit), digits = 4)

# Confidence Intervals using standard errors in the test
round(x = confint.default(fitlogit), digits = 4)

# Calculating odds ratios for the variables
round(x = exp(coef(fitlogit)), digits = 4)

# We can say that:
# For one member increase in family size, the odds of being churned out (attrition or leaving the brand) increases by a factor 2.2 approx.
# Similarly, 1 visit at the service center, if led to dissatisfaction, would increase attrition by a factor 1.5 approx.

## Calculating odds ratios with 95% confidence interval
round(x = exp(cbind(OR = coef(fitlogit), confint(fitlogit))), digits = 4)