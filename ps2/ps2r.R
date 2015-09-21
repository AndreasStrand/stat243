########### R code #############
# From the preparations in bash 10000 random lines were selected in addition to the header. Now the relevant columns are extracted.
# Comparing runtime of read.csv() and read.lines()
cols = c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP", "FPARC", "HHL", "NOC", "MV", "VEH", "YBL")
setwd("~/Documents/ps2")
readcsvTime = system.time(hhdata <- read.csv(file="smpl.csv", header=TRUE, sep=","))
hhCols = hhdata[, cols]
readlinesTime = system.time(hhdata <- readLines("smpl.csv"))

# Checking unique values in each column
sapply(hhCols, function(x) length(unique(x)))
# Checking for elements that are not NA
sapply(hhCols, function(x) sum(!is.na(x)))

