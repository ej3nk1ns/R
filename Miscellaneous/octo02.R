###########################################################################
# Script octo02.R  Plot octopus electricity readings
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readType, fn_readNumbers
# reads: elec-readings.txt
# plots: ...
# writes: 
###########################################################################
# clear environment 
rm(list = ls())

cat("\n*** Running script octo02 to plot electricity energy readings:\n")

setwd("~/R/Working/Octopus")
elec_file <- "elec-readings.txt"

# alternative approach: readLines can retrieve one line at a time
# https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-
#by-line

# define column vectors; the date one is a bit funny
eDate <- as.Date(x = integer(0), origin = "1970-01-01")
eType <- character()
eNight <- integer()
eDay <- integer()

###########################################################################
# define functions to process each data type
###########################################################################
fn_readDate <- function(field){
  
  # replace the st/nd/rd/th and convert to date type
  field <- as.Date(sub("th|st|rd|nd", "", field), format = "%d %b %Y")
  return(field)
}
###########################################################################
fn_readType <- function(field){
  
  return(field)
}
###########################################################################
fn_readNumbers <- function(field){
  
  # separate numbers and return as an unnested list
  field <- sub("Night", "", field)
  readings <- unlist(strsplit(field, split = "Day", fixed = TRUE))
  #cat(lineIn, "number", field, "\n")
  return(readings)
}
###########################################################################
# need to open the connection with file() to keep position as we read
con = file(elec_file, "r")

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
    eDate[lineOut] <- fn_readDate(line)
  }
  
  # line in count = 2,6,10,14 are the meter reading type fields
  if(lineIn %% 4 == 1){
    eType[lineOut] <- fn_readType(line)
  }
  
  # line in count = 3,7,11,15 are the meter readings
  if(lineIn %% 4 == 2){
    readings <- fn_readNumbers(line)
    #  cat(eDate[lineOut], eType[lineOut], line, "\n")
   # print(readings)
#    str(readings)
    eNight[lineOut] <- as.integer(readings[1])
    eDay[lineOut] <- as.integer(readings[2])
    
    # now increment the line out count
    lineOut <- lineOut + 1
  }
  
  # if modulo lineIn is 3, this is a blank line, do nothing 
  lineIn <- lineIn + 1
}

# tidy up
close(con = con)

###########################################################################
# create the data frame, and plot!
###########################################################################
eType <- as.factor(eType)

elec_df <- data.frame(eDate, eType, eNight, eDay)

# first plot
plot(x = elec_df$eDate, 
     y = elec_df$eNight, 
     col = elec_df$eType,
     main = "Electricity meter NIGHT readings 202x to 202y",
     xlab = "Date of reading",
     ylab = "Meter reading")

plot(x = elec_df$eDate, 
     y = elec_df$eDay, 
     col = elec_df$eType,
     main = "Electricity meter DAY readings 202x to 202y",
     xlab = "Date of reading",
     ylab = "Meter reading"
  #   add = TRUE
  )

# now bars for actual readings, points for estimates

#your_df <- gas_df[gas_df$gType == "Your reading", ]

#barplot(height = your_df[order(your_df$gNumber), ]$gNumber, 
#     col = "palegreen",
#     space = 0.5,
#     cex.names = 0.8,
#     names.arg = gas_df[gas_df$gType == "Your reading", ]$gDate,
#  main = "Gas meter readings 202x to 202y",
#   xlab = "Date of reading",
#   ylab = "Meter reading"
#   add = TRUE)

