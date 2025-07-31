###########################################################################
# Script octo03.R  Plot octopus credits and charges
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readType, fn_readNumbers
# reads: transactions-no-workings.txt
# plots: ...
# writes: 
###########################################################################
# clear environment 
rm(list = ls())

cat("\n*** Running script octo03 to plot credits and charges:\n")

setwd("~/R/Working/Octopus")
cc_file <- "transactions-no-workings.txt"

library(viridisLite)            # for an accessible colour palette
colour9 <- viridis(n = 9)       # define a colour scale

# define column vectors; the dates are a bit funny
fuel <- character()
dateFrom <- as.Date(x = integer(0), origin = "1970-01-01")
dateTo <- as.Date(x = integer(0), origin = "1970-01-01")
kWh <- numeric()
cc <- character()
amount <- numeric()
balance <- numeric()

###########################################################################
# define functions to process each data type
###########################################################################
fn_readDate <- function(field){
  
  cat(lineIn, "date", field, "\n")
  
  # convert to date type
  field <- as.Date(field, format = "%d %b %Y")
  return(field)
}
###########################################################################
#fn_readType <- function(field){
#  
#  return(field)
#}
###########################################################################
fn_readNumbers <- function(field){
  
  # separate numbers and return as an unnested list
  #  field <- sub("Night", "", field)
  #  readings <- unlist(strsplit(field, split = "Day", fixed = TRUE))
  cat(lineIn, "number", field, "\n")
  #  return(readings)
}
###########################################################################
fn_previous <- function(line, previous){
  # rotate the previous lines read to use for bank date
  print(nchar(line))
  if(nchar(line) != 0){
    cat("length of previous", length(previous), "\n")
    if(length(previous) == 0){
      previous[1] <- line
    }
  }
  return(previous)
}
###########################################################################
# need to open the connection with file() to keep position as we read
con = file(cc_file, "r")

# count the lines we are reading and writing
lineIn <- 0
lineOut <- 1

# save previous lines to retrieve date of bank payment
previous <- character()

# break out of the loop at EOF
while (TRUE) {
  line = readLines(con = con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  ### this works, just not in fn above
  #previous[1] <- line
  
  # check the type of record
  if (substr(line, 1,3) == "Ban"){
    # this is a bank payment record
    ### bank transfer date is only in the titles! Separate year and day!
    ### need to save the three previous lines as we go, and if it is a bank record,
    ### retrieve the day month and year from them
    cat("Bank", line, "\n")
    
    # now increment the line out count for bank
    lineOut <- lineOut + 1
    
  } else {
    # this is a gas or electricity record, or titles, blank...
    # here we can drop the title lines, all shorter than 15 chars
    if (nchar(line) > 15){
      cat("Fuel", line, "\n")
      
      # now parse the line
      if (substr(line, 1,3) == "Gas"){
        # parse gas
        fuel[lineOut] <- "Gas"
        line <- sub("Gas", "", line)
        #print(line)
      } else {
        # parse electricity
        fuel[lineOut] <- substr(line, 1,11)
        line <- sub("Electricity", "", line)
        #print(line)
      } # end check for gas or electricity, both the same format after
      
      # extract the from date #############################################
      splitLine <- strsplit(line, " - ", fixed = TRUE)
      #print(unlist(splitLine)[1])
      dateFrom[lineOut] <- as.Date(unlist(splitLine)[1], 
                                   format = "%d %b %Y")
      
      # remove from date from line
      line <- unlist(splitLine)[2]
      print(line)
      
      # extract the to date, using the emoji as separator #################
      if (fuel[lineOut] == "Gas"){
        splitLine <- strsplit(line, "🔥", fixed = TRUE)
      } else {
        splitLine <- strsplit(line, "⚡", fixed = TRUE)
      }
      dateTo[lineOut] <- as.Date(unlist(splitLine)[1], 
                                 format = "%d %b %Y")
      
      # remove to date from line
      line <- unlist(splitLine)[2]
      #print(line)
      
      # extract the kWh used in that time period ##########################
      splitLine <- strsplit(line, " ", fixed = TRUE)
      #print(unlist(splitLine)[1])
      kWh[lineOut] <- as.numeric(unlist(splitLine)[1])

      # remove number of kWh from line
#print(length(unlist(splitLine)))
      line <- unlist(splitLine)[2:length(unlist(splitLine))]
      print(line)
      
      # extract charge or credit ##########################################
      ### actually this parsing is only for charge, need check earlier
      
      # extract amount of charge or credit ################################
      
      # extract balance ###################################################
      
      
      # now increment the line out count for fuel
      lineOut <- lineOut + 1
      
    } # end check for bank or fuel
    
    #}else{
    #cat("drop", line, "\n")
    
  } # end check of line length
  
  #  fn_previous(line, previous)
  
  #print("🔥")
  
  # if modulo lineIn is 3, this is a blank line, do nothing 
  lineIn <- lineIn + 1
}

# tidy up
close(con = con)

###########################################################################
# create the data frame, and plot separate day and night
###########################################################################
#eType <- as.factor(eType)
#elec_df <- data.frame(eDate, eType, eNight, eDay)


