library("reshape")
library("ggplot2")

## Graphs on Environment-related R&D Governmental spending in US & EU

US <- read.csv("U:/Global Product/Innovation/Data/US - condensed.csv")
EU <- read.csv("U:/Global Product/Innovation/Data/EU-15 - condensed.csv")

colnames(US)[1] <- "Years"
colnames(EU)[1] <- "Years"

US <- melt(US, id="Years")
EU <- melt(EU, id="Years")

categories <- c(rep("Industry", 31), rep("Buildings", 31*5), rep("Transport", 31*2), rep("Ag and Forestry", 31),
                rep("Renewables", 31*8), rep("Nuclear", 31), rep("Hydrogen Power", 31), rep("Load Management", 31))

EU$Category <- categories
US$Category <- categories

GraphFn <- function(df) ggplot(df, aes(x=Years, y=value, color=variable)) + geom_line() 

US.Graph <- GraphFn(US)
EU.Graph <- GraphFn(EU)

Gen.Public.Spending.Graph <- function(df, cat) return(GraphFn(subset(df, Category==cat)))
