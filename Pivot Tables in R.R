http://www.magesblog.com/2015/03/pivot-tables-with-r.html

I love interactive pivot tables. That is the number one reason why I keep using spreadsheet software. 
The ability to look at data quickly in lots of different ways, without a single line of code helps me to get 
an understanding of the data really fast.

Perhaps I can do the same now in R as well. At yesterday's LondonR meeting Enzo Martoglio presented briefly 
his rpivotTable package. Enzo builds on Nicolas Kruchten's PivotTable.js JavaScript library that provides drag'n'drop 
functionality and wraps it with htmlwidget into R. 

The result is an interactive pivot table rendered in either your default browser or the viewer pane of RStudio with one 
line of code:

## Install packages
library(devtools)
install_github("ramnathv/htmlwidgets") 
install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable
library(rpivotTable)
data(mtcars)
## One line to create pivot table
rpivotTable(mtcars, rows="gear", col="cyl", aggregatorName="Average", 
vals="mpg", rendererName="Treemap")
