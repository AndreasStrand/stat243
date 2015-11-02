# ps6
data = read.csv(bzfile("1987.csv.bz2"))
######## Problem 1 ########
setwd("~/Documents/stat243")
# The folder contains the zipped airline data

# Running this code bunch in the EC2
library(RSQLite)
year = seq(1987,2008, by =1)
airNames = paste("http://www.stat.berkeley.edu/share/paciorek/",year, ".csv.bz2",sep = "")
filename = "airline.db"
db <- dbConnect(dbDriver("SQLite"), dbname = filename)
for (i in 1:length(year)){
dbWriteTable(conn = db, name = "airlines",
             value = read.csv(bzfile(airNames[i]), header=T, sep = ","),
             row.names = FALSE, header = TRUE, append = T)
}
dbDisconnect(db)


