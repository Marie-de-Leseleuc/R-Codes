

## Define work directory and read the documents

setwd("C:\\Users\\mdeleseleuc\\Documents")

train <- read.csv(file = "train.csv", head = TRUE, sep = ";")

test <- read.csv(file = "test.csv", head = TRUE, sep = ";")

head(train)

---------------------------------------------------------------------------------------------------------------------------------------

## DECISION TREE

library(rpart) 

 # The rpart programs build classification or regression models of a very general structure
 # using a two stage procedure; the resulting models can be represented as binary trees

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

## Let's examine tree

plot(fit)
text(fit)

 ## Let's get something better 

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

## Make a prediction

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

head(submit)

## Let's see if we can use the text fields that we excluded from the decision tree

train$Name[1]

## Extraction of the titles

test$Survived <- NA
combi <- rbind(train, test) 

head(combi)

 ## Text fields have been saved as factors instead of string. Need to be converted. 

combi$Name <- as.character(combi$Name)
combi$Name[1]

 ## Need to split the string to extract the title

strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

 ## Get rid of space at the beginning
 ## sub() function replaces the first match of a string, if the parameter is a string vector, replaces the first match of all elements

combi$Title <- sub(' ', '', combi$Title)

 ## How many people per title?

table(combi$Title)

 ## Some titles are very rare and won't give the model much to work with. Let's combine a few of the most unusuale ones
 ## To each title that contains 'Mme' or 'Mlle', assign the title 'Mlle' (%in% c(,) <=> where in (,))

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

 ## Looking for redundancy 
 ## Let's regroup the rich in two groups 

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

## change the variable type back to a factor, as these are essentially categories that we have created

combi$Title <- factor(combi$Title)

## Now let's combine the variables related to the passenger family into one dimension
 ## add the number of siblings, spouses, parents and children the passenger had with them, and plus one for their own existence

combi$FamilySize <- combi$SibSp + combi$Parch + 1

 ## What if specific families had more trouble than others? Could we use their surname?

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

 ## Pb: some surname are more common than others i.e. possible than some of the people that wear a common name are not related 
 ## to the others i.e. need to create an idea per family 

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

 ## Let's give to any family of siz <= 2 the ID "small"

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

 ## Seems like families with one or two members stil appear

famIDs <- data.frame(table(combi$FamilyID))

 ## Cleaning of the data. Extract families of 1 or 2 person and assign them an ID of "small"

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID) # conversion to a factor 

## So let's break them apart and do some predictions on our new fancy engineered variables:
## (need to combine both datasets before breaking them else the factoring wouldn't have worked)
  
  train <- combi[1:891,]
  test <- combi[892:1309,]

## Prediction time

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

fancyRpartPlot(fit)

---------------------------------------------------------------------------------------------------------------------------------------

## RANDOM FORESTS 
  
 ## Ensemble models: the avg results of a collection of imperfect models can sometimes find a superior model from their combination 
 ## than any of the individual parts (grow a lot of different models, and let their outcomes be averaged or voted across the group).

summary(combi$Age) 

 # 20% of missing values. Let's use a decision tree to approx. them. Continuous var. -> anova instead of class
 # Family size and IDs not considered because probably not a huge impact on age 
  
  Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                  data=combi[!is.na(combi$Age),], method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

 # Let see if find other issues

summary(combi)

 # Embarked and Fare both are lacking value 
 
summary(combi$Embarked) #2 blank values 

 # Since a large majority was from Southhampton, let's replace the blanks with an S. 
 
 which(combi$Embarked == '') # find the index of the missing values

 combi$Embarked[c(62,830)] = "S" # 
 combi$Embarked <- factor(combi$Embarked)

 # Then Fare 

summary(combi$Fare) # 1 NA

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)


## Random Forest can only handle factors of 32 levels or less. 2 solutions: unclass or reduce manually the number of levels

  #Let's take the second approach. To do this we'll copy the FamilyID column to a new variable, FamilyID2, and then convert it 
  #from a factor back into a character string with as.character(). We can then increase our cut-off to be a "Small" family 
  #from 2 to 3 people. Then we just convert it back to a factor and we're done:
  
  
  combi$FamilyID2 <- combi$FamilyID
  combi$FamilyID2 <- as.character(combi$FamilyID2)
  combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
  combi$FamilyID2 <- factor(combi$FamilyID2)

## Only 22 levels now. Time to split the test and train and grow a reandom forest <

train <- combi[1:891,]
test <- combi[892:1309,]

install.packages('randomForest')
library(randomForest)

## to get the same classifications each run

set.seed(415) # number inside is not important (just make sure you use the same seed # each time)

## Classification

 # Instead of specifying method="class" as with rpart, we force the model to predict our classification 
 # by temporarily changing our target variable to a factor with only two levels using as.factor(). 
 # The importance=TRUE argument allows us to inspect variable importance as we'll see, and the ntree argument specifies 
 # how many trees we want to grow.

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)

  # ntree = number of decision trees (reduce it for larger dataset or restrict complexity using nodesize)
  # importance = allow to inspect variable importance

## let's look at what variables were important

varImpPlot(fit)

 # 37% if variables not used (Out-of-bag or OOB) are used by the model to see how well each tree performs on unseen data
 # Accuracy: tests to see how worse the model performs without each var. (high decrease expected for very predictive var.)
 # Gini: measure how pure the nodes are at the end of the tree (high score = variable is important)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondforest.csv", row.names = FALSE)

---------------------------------------------------------------------------------------------------------------------------------------
  
  ## FOREST OF CONDITIONAL INFERENCE TREES
  
  # use statistical test instead of purity measure 
  
  install.packages('party')
  library(party)
  
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, type = "class") # which type to use? Class?
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mythirdtree.csv", row.names = FALSE)



