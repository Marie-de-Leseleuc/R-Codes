
## Create an example

ID <- seq(1:177)
Age <- sample(c("0-15", "16-29", "30-44", "45-64", "65+"), 177, replace = TRUE)
Sex <- sample(c("Male", "Female"), 177, replace = TRUE)
Country <- sample(c("England", "Wales", "Scotland", "N. Ireland"), 177, replace = TRUE)
Health <- sample(c("Poor", "Average", "Good"), 177, replace = TRUE)
Survey <- data.frame(Age, Sex, Country, Health)
head(Survey)

## The basic table types supported by crosstab() are:

frequency - frequency count
row.pct - proprotion within row
col.pct - proportion within column
joint.pct - proportion within final 2 dimensions of table
total.pct - proportion of entire table
