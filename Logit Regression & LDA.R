
require(ISLR) # contains dataset we need 

names(Smarket)
summary(Smarket)
?Smarket 
pairs(Smarket,col=Smarket$Direction) # let's plot our data; use binary response as color
head(Smarket,10)

x <- sample(1:3, 1)

# Logistic regression

 # will use direction as a response and see if can predict it as binary response using logistic regression
 # Direction = whether the market had a positive or negative return on a given day
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=Smarket,family=binomial)
summary(glm.fit) # none of the coeficients are significant here (not big surprise for stock market data)

# Null deviance = deviance just for the mean model 
# Residual deviance = deviance for model with all the predictors
# In this case 1731.2 vs. 1727.6 i.e. very modest chance in deviance 

# Deviance is a measure of goodness of fit of a generalized linear model. 
# Or rather, it's a measure of badness of fit-higher numbers indicate worse fit.

# R reports two forms of deviance - the null deviance and the residual deviance. 
# The null deviance shows how well the response variable is predicted by a model 
 # that includes only the intercept (grand mean).

# For our example, we have a value of 43.9 on 31 degrees of freedom. Including the independent variables 
  # (weight and displacement) decreased the deviance to 21.4 points on 29 degrees of freedom, a significant reduction in deviance.
  # The Residual Deviance has reduced by 22.46 with a loss of two degrees of freedom.
# (For more: http://www.theanalysisfactor.com/r-glm-model-fit/) 

glm.probs=predict(glm.fit,type="response") # make predictions from the fitted model; provides vector of fitted probas
glm.probs[1:5]  # Look at first 5 probabilities; very close to 50% which makes sense (binary result)
# provides proba for each combinaison of values between variables 
# (lag1 = bla and lag2 = bli and lag3 =... then lag1 = ble and lag2 = bli and lag3 =...)
# This is a prediction of whether the market's going to be up or down based on the lags and other predictors
glm.pred=ifelse(glm.probs>0.5,"Up","Down") # if prob > 50% then "Up" else "Down" i.e. turn proba into classification
                                           # by thresholding at 0.5 

 # Look at the performance 
attach(Smarket)
table(glm.pred,Direction) # table of ups and downs from our prediction vs. trye direction
# on diagonal = correct classifications 
# off diagonal = mistakes (a lot of those in this case)
mean(glm.pred==Direction) # mean classification performance (= (145+507)/1250 )
 # mean = 0.52 i.e on training data we do slightly better than chance

# we may have overfit on the training data so we are going to divide our data up into a training and test set.   

# Make training and test set

train = Year<2005 # vector of logicals (all data with year < 2005 will be "True")
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train) #will only use observations for which train is true
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") # predict on test data using model create on
                                                                    # train data
glm.pred=ifelse(glm.probs >0.5,"Up","Down") # create classification
Direction.2005=Smarket$Direction[!train] # subset of response variable (direction) for test data.
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # mean = 0.48 (doing worse on test data). Might be overfitting. 

#Fit smaller model (use only lag1 and lag2 and leave out all the other variables)

glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train) #will only use observations for which train is true
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") # predict on test data using model create on
                                                                    # train data
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # 0.56 (correct classification of 56% - not too bad)
106/(76+106) # true positive/(true positive + false positive) = precision of the model

require(MASS)

## Linear Discriminant Analysis

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit # "Coefficients" are the regressional weights to compute the LDs 
        #  helpful in deciding which variable affects more in classification.
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,] #posterior.Down/Up = proba that down or up
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest Neighbors

library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
