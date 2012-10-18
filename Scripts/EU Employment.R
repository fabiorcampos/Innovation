library("reshape")
library("ggplot2")
olddir <- getwd()

## from R-D personnel by sector of employment and field of science
## OECD.StatExtracts

# Data Characteristics
# Date last updated
# February 2012; forthcoming update February 2013.
# Other data characteristics
# Sources and Methods Databases
# Reference period
# 1981 onward.
# Unit of measure used
# Full-time equivalent on R&D (FTE) and/or Headcounts
# Variables collected
# These tables present research and development (R&D) personnel statistics for :
#   - Total R&D personnel by sector of employment and field of science, in full-time equivalent on R&D;
# - Researchers by sex, sector of employment and field of science, in full-time equivant on R&D;
# - Researchers by sex, sector of employment and field of science, in headcounts.
# Sectors of employment are business enterprise, government, higher education, private non-profit and total.
# Breakdown by field of science includes natural sciences, engineering, medical sciences, agricultural sciences, social sciences, and humanities.

## script using the results of the EU Employment Consolidation Script

setwd("U:/Global Product/Innovation/OECD Employment")
EU.Total <- read.csv("EU_15_Total.csv")
names(EU.Total) <- c(names(EU.Total)[1:4], 1981:2010)

EU.Total <- subset(EU.Total, Sector != "Total")

Melted.EU.Total <- melt(EU.Total[, -1], id=c("Sector", "Field", "Discipline"))

c1 <- ggplot(na.omit(Melted.EU.Total), aes(x=variable, y=value, fill=Discipline)) + 
  geom_bar() + facet_wrap(~Sector)


setwd(olddir)