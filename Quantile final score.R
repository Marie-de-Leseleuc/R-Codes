
## Load the data

Scoredata <- read.csv(file = "ScorePerLevel2.csv", head = TRUE, sep = ";")

summary(Scoredata$finalscore) 

## Plot the final score per map

boxplot(finalscore~map,data = Scoredata, main="Score Data by Map",
        xlab="Map", ylab="Final Score") 

## Calculate the quantiles

FinalScoreW25 <- ddply(Scoredata, "map", summarise, WQ25 = quantile(finalscore, .25))
FinalScoreW50 <- ddply(Scoredata, "map", summarise, WQ50 = quantile(finalscore, .50))
FinalScoreW60 <- ddply(Scoredata, "map", summarise, WQ60 = quantile(finalscore, .60))
FinalScoreW70 <- ddply(Scoredata, "map", summarise, WQ70 = quantile(finalscore, .70))
FinalScoreW75 <- ddply(Scoredata, "map", summarise, WQ75 = quantile(finalscore, .75))
FinalScoreW80 <- ddply(Scoredata, "map", summarise, WQ80 = quantile(finalscore, .80))
FinalScoreW90 <- ddply(Scoredata, "map", summarise, WQ90 = quantile(finalscore, .90))
FinalScoreW95 <- ddply(Scoredata, "map", summarise, WQ95 = quantile(finalscore, .95))

  ## Exercice: Try to create a function that calculate each of these quantiles and return a column for each 

 Quantile_Function <- function(data,var1,var2) {
   
   result <- data.frame()
   
   FinalScoreW25 <- ddply(data, var1, summarise, WQ25 = quantile(var2, .25))
   FinalScoreW50 <- ddply(data, var1, summarise, WQ50 = quantile(var2, .50))
   FinalScoreW75 <- ddply(data, var1, summarise, WQ75 = quantile(var2, .75))
   FinalScoreW95 <- ddply(data, var1, summarise, WQ95 = quantile(var2, .95))
   
   result <- cbind(FinalScoreW25,FinalScoreW50$WQ50,FinalScoreW75$WQ75,FinalScoreW95$WQ95)
   
  }

Quantile_Function(Scoredata,Scoredata$map,Scoredata$finalscore)

## Bind the result

FinalScoreWQ <- cbind(FinalScoreW25,FinalScoreW50$WQ50,FinalScoreW60$WQ60,FinalScoreW70$WQ70,FinalScoreW75$WQ75,
                      FinalScoreW80$WQ80,FinalScoreW90$WQ90,FinalScoreW95$WQ95)


attach(FinalScoreW70)
FinalScoreW70[which(map == 'Map 1'),]

## Export to Excel

library(xlsx)
write.xlsx(FinalScoreWQ, "c:/FinalScoreWQ.xlsx") 


# Time vs. ParTime

## Load the data

Timemap <- read.csv(file = "TimePerMap.csv", head = TRUE, sep = ";")

attach(Timemap)
summary(Timemap) 

 # Median

MedianTime <- ddply(Timemap, "map", summarise, WQ50 = quantile(TimeMs, .50))


finalresult <- cbind(Timemap[,c(5,8)], MedianTime)

library(xlsx)
write.xlsx(MedianTime, "C:\\Users\\mdeleseleuc\\Documents\\MedianTime.xlsx") 
write.xlsx(Timemap[,c(5,8)], "C:\\Users\\mdeleseleuc\\Documents\\ParTime.xlsx") 


