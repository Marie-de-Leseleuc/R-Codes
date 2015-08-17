-- Set Directory

setwd("C:\\Users\\mdeleseleuc\\Documents")

-- Read File

 ## Si pas de transformation du doc
train <- read.csv("train.csv", stringsAsFactors=FALSE)

 ## Si données converties dans le doc excel préalablement à l'export 
Train <- read.csv(file = "train.csv", head = TRUE, sep = ";")
head(Train)
attach(Train)

 ## Petit essai: À quelle classe appartiennent les noyés? 
Deads <- subset(Train,Survived ==0 ,select=c(Name, Pclass))
FreqD <- table(Deads$Pclass) 
PercentFreqD <- round(prop.table(FreqD,)*100, digit = 0) 
  #68% were 3rd class;86% were 2nd & 3rd class

 ##Then what about survivors
Survivors <- subset(Train,Survived ==1 ,select=c(Name, Pclass))
FreqS <- table(Survivors$Pclass) 
PercentFreqS <- round(prop.table(FreqS,)*100, digit = 0) 
 # 40% were first class 

 ## Total: how many survivors?
round(prop.table(table(Train$Survived))*100, digit = 0) 


