library("ggplot2")
library("reshape")

deflator <- read.table('U:/Global Product/Innovation/Data/CPI.txt', header=T, quote='\"')

Deflator.Series <- deflator$Avg.[68:98]
Multiplier <- Deflator.Series[31] / Deflator.Series ## everything in 2010 dollars

US.Private.Energy.Industry <- read.csv("U:/Global Product/Innovation/Data/US_Private_Energy_Industry.csv")

US.Private.Energy.Energy <- read.csv("U:/Global Product/Innovation/Data/US_Private_Energy_Energy.csv")


Deflate <- function(df) {
  cbind(df[, 1],  sapply(2:ncol(df), 
                         function(x) {df[, x] * Multiplier[1:28]}))
}

Transform.Data <- function(df) {
  newdf <- Deflate(df)
  colnames(newdf) <- colnames(df)
  newdf <- melt(as.data.frame(newdf), id="Year")
  c1 <- ggplot(na.omit(newdf), aes(x=Year, y=value, colour=variable)) + geom_line()
  return(c1)
}

By.Industry <- Transform.Data(US.Private.Energy.Industry)
By.Fuel.Source <- Transform.Data(US.Private.Energy.Energy)


