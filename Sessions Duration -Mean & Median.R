
# Read the csv file 
SessionsDurations <- read.csv(file = "SessionsDurations.csv", head = TRUE, sep = ",")

# Obtain the session durations above 1 minute
Duration <- subset(SessionsDurations[,4], SessionsDurations[,4] > 1)

# Obtain the mean and the median (exclude NA results)
median(Duration,na.rm = TRUE)
mean(Duration,na.rm = TRUE)

  ## Events per Day

# Read the csv file 
EventsByDay <- read.csv(file = "EventsByDay.csv", head = TRUE, sep = ";")

# Obtain the number of sessions per day and player
EventsPerDay <- EventsByDay$Events

# Obtain the mean and the median (exclude NA results)
median(EventsPerDay,na.rm = TRUE)
mean(EventsPerDay,na.rm = TRUE)

#Create a box plot

boxplot(EventsPerDay,data=EventsByDay, xlab="Players", ylab="Events") 


# Read the csv file 
DaysHeld <- read.csv(file = "DaysHeld.csv", head = TRUE, sep = ";")

head(DaysHeld)

# Obtain the session durations above 1 minute
Days <- DaysHeld$Days

# Obtain the mean and the median (exclude NA results)
median(Days,na.rm = TRUE)
mean(Days,na.rm = TRUE)

#Create a box plot

boxplot(Days,data=DaysHeld, xlab="Players", ylab="Days Held") 



