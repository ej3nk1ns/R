###########################################################################
# Script octo01.R  Plot octopus gas readings
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readType, fn_readNumber
# reads: gas-readings.txt
# plots: gas meter readings (interactively)
# writes: 
###########################################################################
# clear environment 
rm(list = ls())

cat("\n*** Running script octo01 to plot gas energy readings:\n")

library(viridisLite)            # for an accessible colour palette
colour4 <- viridis(n = 4)       # define a colour scale

setwd("~/R/Working/Octopus")
gas_file <- "gas-readings.txt"

# https://stackoverflow.com/questions/14582422/how-to-read-#multiple-lines-
#of-a-file-into-one-row-of-a-dataframe
# scan the file into a list (data is on successive lines)?
#gas_raw <- scan(gas_file, what = character())
#summary(gas_raw)

# alternative approach: readLines can retrieve one line at a time
# https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-
#by-line

# define column vectors; the date one is a bit funny
gDate <- as.Date(x = integer(0), origin = "1970-01-01")
gType <- character()
gNumber <- integer()

###########################################################################
# define functions to process each data type
###########################################################################
fn_readDate <- function(field){
  # replace the st/nd/rd/th and convert to date type
  field <- as.Date(sub("th|st|rd|nd", "", field), format = "%d %b %Y")
  #cat(lineIn, "date", field, "\n")
  return(field)
}
###########################################################################
fn_readType <- function(field){
  #cat(lineIn, "type", field, "\n")
  return(field)
}
###########################################################################
fn_readNumber <- function(field){
  # convert to integer
  field <- as.integer(field)
  #cat(lineIn, "number", field, "\n")
  return(field)
}
###########################################################################
# need to open the connection with file() to keep position as we read
con = file(gas_file, "r")

# count the lines we are reading and writing
lineIn <- 0
lineOut <- 1

# break out of the loop at EOF
while (TRUE) {
  line = readLines(con = con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  # line in count = 1,5,9,13... are the date fields
  if(lineIn %% 4 == 0){
    # add to date vector
    gDate[lineOut] <- fn_readDate(line)
  }
  
  # line in count = 2,6,10,14 are the meter reading type fields
  if(lineIn %% 4 == 1){
    gType[lineOut] <- fn_readType(line)
  }
  
  # line in count = 3,7,11,15 are the meter readings
  if(lineIn %% 4 == 2){
    gNumber[lineOut] <- fn_readNumber(line)
    #cat(gDate[lineOut], gType[lineOut], gNumber[lineOut], "\n")
    
    # now increment the line out count
    lineOut <- lineOut + 1
  }
  
  # if modulo lineIn is 3, this is a blank line, do nothing 
  lineIn <- lineIn + 1
}

# tidy up
close(con = con)

###########################################################################
# create the data frame, export and plot!
###########################################################################
gType <- as.factor(gType)
gas_df <- data.frame(gDate, gType, gNumber)

write.csv(gas_df, 
          file = "gas.csv",
          row.names = FALSE)

# first plot, estimated readings
cat("*** Plotting estimated readings\n")
plot(x = gas_df[gas_df$gType == "Estimated reading", ]$gDate, 
     y = gas_df[gas_df$gType == "Estimated reading", ]$gNumber, 
     #col = gas_df$gType,
     main = "Gas meter readings (estimated) April 2023 to July 2025",
     xlab = "Date of estimation",
     ylab = "Meter reading")

# now bars for actual readings
your_df <- gas_df[gas_df$gType == "Your reading", ]

cat("*** Plotting actual ('Your') readings\n")
barplot(height = your_df[order(your_df$gNumber), ]$gNumber, 
        col = colour4[3],
        space = 0.5,
        cex.names = 0.8,
        names.arg = your_df[order(your_df$gNumber), ]$gDate,
        main = "Gas meter readings (actual) April 2023 to July 2025",
        axis.lty = 1,
        ylim = c(2000, 7000),
        xpd = FALSE,               # truncate bars at y limits
        xlab = "Date of reading",
        ylab = "Meter reading"
)

# plot both reading types, denoted by colours, with a legend
palette(colour4[3:4])

cat("*** Plotting combined gas readings\n")
barplot(height = gas_df[order(gas_df$gDate), ]$gNumber, 
        col = gas_df[order(gas_df$gDate), ]$gType,   
        space = 0.4,
        cex.names = 0.75,
        names.arg = gas_df[order(gas_df$gDate), ]$gDate,
        main = "Gas meter readings April 2023 to July 2025",
        axis.lty = 1,
        las = 2,                   # rotated labels
        ylim = c(2000, 7000),
        xpd = FALSE,               # truncate bars at y limits
        ylab = "Meter reading",
)
legend("topleft", legend = gas_df[2:3, ]$gType, fill = colour4[3:4])
