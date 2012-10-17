library("reshape")
library("ggplot2")

setwd("U:\\Global Product\\Innovation")

## Deprecated Helper Functions
Convert.Numeric <- function (vector) {
  new <- sapply(vector, function(x) {as.numeric(as.character(x))})
  return(new) 
}

Load.and.Convert <- function (dir_path) {
  df <- read.csv(dir_path, skip=1, quote="\"")
  length <- ncol(df)
  
  for (i in 4:length) {
    df[, i] <- Convert.Numeric(df[, i])   
  }
  
  df[is.na(df)] <- 0
  
  print("Ignore the errors, they have to do with NA conversion, which is something we want.")
  return(df)  
}

Return.All.EU.Data <- function(dir_path) {
  
  olddir <- getwd()
  setwd(dir_path)
  
  dummydf <- Load.and.Convert(dir()[1])
  initcols <- dummydf[, 1:3]
  ncol <- ncol(dummydf)
  nrow <- nrow(dummydf)
  
  initdf <- as.data.frame(matrix(0, nrow, ncol))[, 4:ncol]
  for (i in dir()) {
    df <- Load.and.Convert(i)[, 4:ncol]
    initdf <- initdf + df
  }
  
  setwd(olddir)
  
  initdf <- cbind(initcols[, -1], initdf)
  
  colnames(initdf) <- c("Product", "FLOW", 1980:2010)
  
  return(initdf)
  
}

## Load the real data

# US data in Data/US_RD.csv
# EU-15 Data in Data/EU_RD.csv

US <- read.csv("Data/USA_RD.csv")
EU <- read.csv("Data/EU_15_RD.csv")

US.no.label <- US[, 5:ncol(US)]
colnames(US.no.label) <- 1980:2010
EU.no.label <- EU[, 4:ncol(EU)]
colnames(EU.no.label) <- 1980:2010

## helper function to exctract the columns listed below into a single helpful data frame
## result datasets in: "Data/EU-15 - condensed.csv" and "Data/US - condensed.csv"

Extract.Data <- function(df) {
  
  newdf <- data.frame(
    
    "Industry" = t(df)[, 2],
    "Buildings Total" = t(df)[, 7],
    "Design and Envelope" = t(df)[, 8],
    "Equipment and Operation" = t(df)[, 12],
    "Appliances" = t(df)[, 18],
    "Other" = t(df)[, 23],
    "Transport EE" = t(df)[, 24],
    "Transport Liquid Biofuels" = t(df)[, 84],
    "Ag Forestry EE" = t(df)[, 40], 
    "Renewable Energy Total" = t(df)[, 66],
    "Solar" = t(df)[, 67],
    "Wind" = t(df)[, 72],
    "Ocean" = t(df)[, 77],
    "Geothermal" = t(df)[, 99], 
    "Hydro Total" = t(df)[, 105], 
    "Small Hydro" = t(df)[, 107], 
    "All Biofuels" = t(df)[, 83],
    "Nuclear" = t(df)[, 111],
    "Hydrogen Power" = t(df)[, 138],
    "Transmission Storage Load Management" = t(df)[, 152]
    
  )
  
  return(newdf)
}

# energy-efficiency
#   Total: 1
# industry: 
#   Total: 2
#   Other: 3-6
# buildings
#   Total: 7
#   Other: 8-23
#   Building Design & Envelope: 8
#   Equipment & Operation: 12
#   Appliances: 18
#   Other: 23
# transport
#   Total: 24
#   Other: 25-36
# ag & forestry: 40

# renewable energy
#   Total: 66
#   Renewable Electricity:
#     Solar: 67
#     Wind: 72
#     Ocean: 77
#     Geothermal: 99
#     Hydro: 105
#     Small Hydro: 107
#   All biofuels: 83
#     liquid biofuels: 84  

# nuclear
#   Total: 111

# hydrogen power
#   Total: 138

# transmission / storage / load management
#   Total: 152



