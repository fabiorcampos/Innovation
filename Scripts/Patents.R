library("ggplot2")
library("reshape")
library("hash")

# Graphs on patent applications

Import.Country <- function(country) {
  directory <- paste("U:/Global Product/Innovation/Patents/", country, "_patents.csv", sep="")
  df <- read.csv(directory, skip=5, header=T, stringsAsFactors=F)
  df[, c(-1, -2, -6)]
}

Add.Colnames <- function(df) {
  for (i in 1:3) df[, i] <- as.character(df[, i])
  df[1, 1] <- "Cat1"
  df[1, 2] <- "Cat2"
  df[1, 3] <- "Cat3"
  colnames(df) <- df[1, ]
  df <- df[c(-1, -2), ]
  df[1, c(2, 3)] <- "Total"
  return(df)
}

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

Convert.To.Numeric <- function(df) {
  for (i in 1980:2011) {
    df[[as.character(i)]] <- as.numeric(df[[as.character(i)]]) 
  }
  df
}

Import.Patent.Dataset <- function(country) {
  
  df <- Add.Colnames(Import.Country(country))
  df <- Propagate.Down(df, 1)
  df <- Propagate.Down(df, 2)
  df <- Convert.To.Numeric(df)
  df[1, 1] <- "Total"
  df <- df[c(-2, -nrow(df)), ]
  df <- melt(df, id=c("Cat1", "Cat2", "Cat3"))
  df
}

categories <- hash()
.set(categories, 
     "Total" = "Total",
     "General"="General Environmental Management (air, water, waste)",
     "Generation"= "Energy generation from renewable and non-fossil sources",
     "Combustion"= "Combustion technologies with mitigation potential (e.g. using fossil fuels, biomass, waste, etc.)",
     "Capture"= "Technologies specific to climate change mitigation",
     "Storage"= "Technologies with potential or indirect contribution to emissions mitigation",
      "Transport"= "Emissions abatement and fuel efficiency in transportation",
      "Buildings"= "Energy efficiency in buildings and lighting")

Gen.Patent.Graph <- function(country, category) {
  df <- Import.Patent.Dataset(country)
  if (category != "Generation") {
    c1 <- ggplot(na.omit(subset(df, Cat1==categories[[category]])), 
                aes(x=variable, y=value, fill=Cat2)) + geom_bar()
  } else {
    c1 <- ggplot(na.omit(subset(df, Cat1==categories[[category]])), 
                 aes(x=variable, y=value, fill=Cat3)) + geom_bar()    
  }
  c1
}

