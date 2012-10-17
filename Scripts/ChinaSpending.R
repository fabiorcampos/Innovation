## OECD R&D Spending Data for China

library("reshape")
library("ggplot2")

China <- read.csv("U:/Global Product/Innovation/OECD Spending/China.csv", skip=4, stringsAsFactors=F)

Propagate.Down <- function(df, colnum) {
  rowstodel <- c()
  
  #pull newcolumn out
  newcolumn <- df[, colnum]
  
  #alter newcolum
  j <- newcolumn[1]
  for (i in 1:(length(newcolumn)-1)) {
    k <- newcolumn[i]
    nextrow <- newcolumn[(i+1)]
    if (k == "") {
      newcolumn[i] <- j
    } else if (k == nextrow) {
      rowstodel <- append(rowstodel, -i)
      j <- k
    } 
  }
  
  #put nuewcolumn back in
  df[, colnum] <- newcolumn
  df <- df[rowstodel, ]
  return(df)
}

China <- Propagate.Down(China, 2)

China <- China[1:10, 2:ncol(China)]
China <- China[, -3]

colnames(China) <- c("Cat1", "Cat2", China[1, 3:ncol(China)])
China <- China[-c(1, 2), ]
China[1, 1] <- "Total"

for (i in 1981:2010) China[[as.character(i)]] <- as.numeric(China[[as.character(i)]])

China <- melt(China, id=c("Cat1", "Cat2"))

#View(China)

ChinaGraph <- ggplot(na.omit(China), aes(x=variable, y=value, color=Cat1)) + geom_bar() + facet_wrap(~ Cat2)
