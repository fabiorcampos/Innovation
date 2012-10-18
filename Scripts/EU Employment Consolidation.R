## Script to consolidate European employment data
## Consolidated Data in EU_15.csv

Emp.Import.Data <- function(dir) {  
  df <- read.csv(dir, skip=5, stringsAsFactors=F)
  df <- df[1:52, ]
  df[3, 1] <- "Total"
  df <- df[, -c(1, 3, 6)]
  df
}

Emp.Add.Colnames <- function(df) {
  
  df[3, c(1, 2, 3)] <- "Total"
  df[1, c(1, 2, 3)] <- c("Sector", "Field", "Discipline")
  colnames(df) <- df[1, ]
  df <- df[-c(1, 2), ]
  
}

Emp.Propagate.Down <- function(df, colnum) {

  rowstodel <- c(-1)
  newcol <- df[, colnum]
  
  replace.val <- newcol[1]
  
  for (i in 2:length(newcol)) {
    
    # if row is the first of it's type, it gets deleted.
    # to be first it has to be: nonempty & not equal to the sequence above
    if (newcol[i] != "" && newcol[i] != replace.val) {
      rowstodel <- append(rowstodel, -i)
      replace.val <- newcol[i]
    }
    
    # if the row is empty, replace it with the replacement value
    if (newcol[i] == ""){
      newcol[i] <- replace.val      
    }
  }
  
  df[, colnum] <- newcol
  df <- df[rowstodel, ]
  df
  
}

Emp.Convert.Numeric <- function(df) 
{ 
  for (i in 1981:2010) 
  {
    df[[as.character(i)]] <- as.numeric(df[[as.character(i)]])
    df[[as.character(i)]] <- sapply(df[[as.character(i)]], function(x) if(is.na(x)) x<-0 else x)
  }
  df
}

Import.EU.Employment.Data <- function(dir){
  
  df  <- Emp.Import.Data(dir)
  df  <- Emp.Add.Colnames(df)
  df  <- Emp.Propagate.Down(df, 1)
  df  <- Emp.Propagate.Down(df, 2)
  df  <- Emp.Convert.Numeric(df)
  df
  
}

country <- "Austria"
ausdir <- paste("U:/Global Product/Innovation/OECD Employment/EU_15/", country,".csv", sep="")
Austria <- Import.EU.Employment.Data(ausdir)
#init <- Austria[1, ]

Import.All.EU.Emp.Data <- function()
{
  olddir <- getwd()
  setwd("U:/Global Product/Innovation/OECD Employment/EU_15/")
  for (i in dir(pattern="*.csv"))
  {
    
    df <- Import.EU.Employment.Data(i)
    if (!exists("compiled")) 
    {
      compiled <- df
    } else 
    {
      compiled <- cbind(compiled[, c(1, 2, 3)], compiled[, -c(1, 2, 3)] + df[, -c(1, 2, 3)])
    }
    print("success")
  }
  setwd(olddir)
  compiled  
}

replacewithzero <- function(x)
{
  x[x==0] <- NA
  x
}

Total <- as.data.frame(lapply(Import.All.EU.Emp.Data(), replacewithzero))
names(Total) <- c(names(Total)[1:3], 1981:2010)

View(Total)

setwd("U:/Global Product/Innovation/OECD Employment/")

write.csv(Total, "EU_15_Total.csv")



