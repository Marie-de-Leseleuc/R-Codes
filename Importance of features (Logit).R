
------- # IMPORTANCE OF FEATURES IN LOGISTIC REGRESSION ----------------

# Source: https://www.r-bloggers.com/logistic-regression-in-r-part-one/ 
# Source2: http://www.ats.ucla.edu/stat/r/dae/logit.htm

setwd("C:\\Users\\mdeleseleuc\\Documents")
mydata<- read.csv(file="name.csv",head=TRUE,sep=",")
head(mydata)

  ----- # Part 1 -----

mydata$isy <- factor(mydata$isy)

library(caret)
library(aod)
library(ggplot2)
library(Rcpp)
# library(arm)

Train <- createDataPartition(mydata$isx, p=0.6, list=FALSE)
training <- mydata[Train, ]
testing <- mydata[-Train, ]

head(training)
head(testing)

glm.fit=glm(isx~isy+kills+neutralizations+deaths+
              days+lastseen+sessions+purchased+transactions+lifespan+lifetime, 
            data=training,family="binomial")

 

summary(glm.fit) # estimates 

confint(glm.fit) 

## odds ratios
odds_ratios = exp(coef(glm.fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(glm.fit), confint(glm.fit)))

# for a one unit increase in isy1, the off of completing the game increase by a factor of 1.92

# In logistic regression the odds ratio represents the constant effect of a predictor X, 
# on the likelihood that one outcome will occur.
# Source: http://www.theanalysisfactor.com/why-use-odds-ratios/

  # Can also use predicted probabilities to help understand the model. 
  # Predicted probabilities can be computed for both categorical and continuous predictor variables. 
  # start by calculating the predicted probability of admission at each value of isy, 
  # holding other variable at their means. 
  # First create and view the data frame.

newdata1 <- with(mydata,
                 data.frame(purchased = mean(purchased), 
                            transactions = mean(transactions), 
                            kills = mean(kills),
                            neutralizations = mean(neutralizations),
                            deaths = mean(deaths),
                            lifespan = mean(lifespan),
                            days = mean(days),
                            lastseen = mean(lastseen),
                            lifetime = mean(lifetime),
                            sessions = mean(sessions),
                            isy = factor(0:1)))

head(newdata1)

newdata1$isyP <- predict(glm.fit, newdata = newdata1, type = "response")
newdata1

  ----- # Part 2 ------

# Source: https://www.r-bloggers.com/logistic-regression-in-r-part-two/ 

# make predictions from the fitted model; provides vector of fitted probas
glm.probs=predict(glm.fit,newdata=testing, type="response") 
glm.probs[1:5]

# Goodness of Fit: Likelihood Ratio Test

 # Is the model more efficient than a smaller one?
 # The null hypothesis, H_0  holds that the reduced model is true

glm.fit.2=glm(isx~kills+deaths, data=training,family="binomial")

library(lmtest)
lrtest(glm.fit, glm.fit.2) # compare to smaller model

lrtest(glm.fit) # compare to null model

# Goodness of Fit: Pseudo R^2 

 # R^2  statistic tells us the proportion of variance in the dependent variable that is explained by the predictors.
 # no equivalent for Logit reg but can use McFadden's R^2 
 # (= 1-(log likelihood of fitted model/log likelihood of null model)
 # The measure ranges from 0  to just under 1 , with values closer to zero indicating that the model has no predictive power.

library(pscl)
pR2(glm.fit) # look for 'McFadden'

# Goodness of Fit: Hosmer-Lemeshow Test 

 # examines whether the observed proportion of events are similar to the predicted probabilities of occurences 
 # in subgroups of the dataset using a pearson chi-square statistic from the 2 x g table of observed and expected frequencies.
 # Small values with large p-values indicate a good fit to the data while large values with p-values below 0.05 
 # indicate a poor fit. The null hypothesis holds that the model fits the data.

library(MKmisc)
HLgof.test(fit = fitted(glm.fit), obs = training$isx)

library(ResourceSelection)
hoslem.test(training$isx, fitted(glm.fit), g=10)

# Tests of Individual Predictors: Wald Test

 # A wald test is used to evaluate the statistical significance of each coefficient in the model 
 # The idea is to test the hypothesis that the coefficient of an independent variable in the model 
 # is not significantly different from zero. 
 # If the test fails to reject the null hypothesis, this suggests that removing the variable from the model will not 
 # substantially harm the fit of that model.

library(survey)

regTermTest(glm.fit, "isy") # significant
regTermTest(glm.fit, "purchased") # 
regTermTest(glm.fit, "lifespan")
regTermTest(glm.fit, "kills")

# Tests of Individual Predictors: Variable Importance

 # To assess the relative importance of individual predictors in the model, 
 # we can also look at the absolute value of the t-statistic for each model parameter.
 # The t-statistic for each model parameter helps us determine if it's significantly different from zero.

mod_fit <- train(isx ~ ., data=training, method="glm", family="binomial")

varImp(mod_fit)

# Validation of Predicted Values: Classification Rate

 #  Regards how well the model does in predicting the target variable on out of sample observations.

pred = predict(glm.fit, newdata=testing)
accuracy <- table(pred, testing[,"isx"])
sum(diag(accuracy))/sum(accuracy)

pred = predict(glm.fit, newdata=testing)
confusionMatrix(data=pred, testing$isx)

# Validation of Predicted Values: ROC Curve

 # The receiving operating characteristic is a measure of classifier performance. 
 # It's based on the proportion of positive data points that are correctly considered as positive
 # and the proportion of negative data points that are accuratecly considered as negative.

library(pROC)
# Compute AUC for predicting Class with the variable isy
f1 = roc(isx ~ isy, data=training)
plot(f1, col="red")

library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(glm.fit, newdata=testing, type="response")
pred <- prediction(prob, testing$isx)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)


auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc # area under the curve  

 # That metric ranges from 0.50 to 1.00, and values above 0.80 indicate that the model does a great job 
 # in discriminating between the two categories which comprise our target variable.

