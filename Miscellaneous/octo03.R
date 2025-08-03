###########################################################################
# Script octo03.R  Plot octopus credits and charges
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readNumbers
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
# main script
###########################################################################
# need to open the connection with file() to keep position as we read
con = file(cc_file, "r")

# count the lines we are reading and writing
lineIn <- 0
lineOut <- 1

# save and count previous lines to retrieve some dates 
previous <- character()
i <- 1

# read the input file line by line, and process accordingly
while (TRUE) {
  
  # break out of the loop at EOF
  line = readLines(con = con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  # first, skip blank lines
  if(nchar(line) == 0){
    # no further action
    #cat("blank line\n")
  } else {
    
    # next, save any short (date) lines to our 'previous' list
    if (nchar(line) <= 15){
      previous[i] <- line
      
      # insert a space between day and month
      # https://stackoverflow.com/questions/26896971/add-space-between-
      #two-letters-in-a-string-in-r#26897007
      previous[i] <- gsub("([0-9])([A-Z])", "\\1 \\2", previous[i])
      
      #print(previous[i])
      
      # save the year separately
      if(grepl("202", previous[i], fixed = TRUE)){
        # this is a year field, save it until it changes
        
        # extract the year only
        # https://stackoverflow.com/questions/40744463/how-to-extract-
        #numbers-from-text
        #print(as.numeric(strsplit(previous[i], "\\D+")[[1]][-1]))
        year <- as.numeric(strsplit(previous[i], "\\D+")[[1]][-1])
        print(year)
      }
      i <- i + 1
    } else {
      
      # check the type of record
      if (substr(line, 1,3) == "Ban"){
        
        # this is a bank payment record
        #cat("Bank", line, "\n")
        fuel[lineOut] <- "Bank"
        
        # extract bank transfer date from the titles, year and day ########

        print(paste(previous[i-1], year))
        print(as.Date(paste(previous[i-1], year), format = "%d %b %Y"))
       # print(paste(previous[i-1], year))
        
        # set both dates the same
        dateFrom[lineOut] <- as.Date(paste(previous[i-1], year), 
                                     format = "%d %b %Y")
        dateTo[lineOut] <- as.Date(paste(previous[i-1], year), 
                                     format = "%d %b %Y")
        
        # extract amount paid #############################################
        splitLine <- strsplit(line, "£", fixed = TRUE)
        split2 <- strsplit(unlist(splitLine)[2], " ", fixed = TRUE)
        
        # remove any commas
        amount[lineOut] <- as.numeric(sub(",", "", unlist(split2)[1]))
        
        # extract balance #################################################
        balance[lineOut] <- as.numeric(unlist(splitLine)[3])
        
        # initialise other fields
        kWh[lineOut] <- 0
        cc[lineOut] <- "credit"
        
      } else {
        
        # this is a gas or electricity record
        #cat("Fuel", line, "\n")
        
        # now parse the line
        if (substr(line, 1,3) == "Gas"){
          # parse gas
          fuel[lineOut] <- "Gas"
          line <- sub("Gas", "", line)
          
        } else {
          # parse electricity
          fuel[lineOut] <- substr(line, 1,11)
          line <- sub("Electricity", "", line)
          
        } # end check for gas or electricity, both the same format after
        
        # check for charge or credit ######################################
        
        creditText <- "We credited your account "
        check <- grepl(creditText, line, fixed = TRUE)
        
        if (check == TRUE){
          # the credit line is shorter than charge line, but date is messy
          
          # first deal with special case credit line with -ve kWh
          checko1o <- grepl("-58.05 kWh", line, fixed = TRUE)
          # deal with odd one out
          if(checko1o){
            # set fields by position
            dateFrom[lineOut] <- as.Date(substr(line, 1, 11), 
                                         format = "%d %b %Y")
            dateTo[lineOut] <- as.Date(substr(line, 15, 25), 
                                       format = "%d %b %Y")
            kWh[lineOut] <- as.numeric(substr(line, 27, 32))
            cc[lineOut] <- "credit"
            amount[lineOut] <- as.numeric(substr(line, 63, 67))
            balance[lineOut] <- as.numeric(sub("£", "", 
                                               substr(line, 68, 75)))
          } else{
            # not the special case credit line
            
            # extract date ################################################
            cat(unlist(previous[i-1]), unlist(previous[i-2]), "\n")
            
            # extract amount of credit ####################################
            # first check if the balance is +ve or -ve
            minus <- grepl("-", line, fixed = TRUE)
            
            # then remove the minus to separate the amount and balance
            if(minus){
              line <- sub("-", "", line)
            }
            splitLine <- strsplit(line, "£", fixed = TRUE)
            amount[lineOut] <- as.numeric(sub("£", "", 
                                              unlist(splitLine)[2]))
            
            # extract balance #############################################
            balance[lineOut] <- as.numeric(sub(",", "", 
                                               unlist(splitLine)[3]))
            if (minus){
              # reapply the minus sign
              balance[lineOut] <- balance[lineOut] * -1
            }
            
            # set credit and initialise other fields
            cc[lineOut] <- "credit"
            kWh[lineOut] <- 0
            
          } # end special case check 
        } else {
          # this is a charge, not a credit ################################
          
          # extract the from date #########################################
          splitLine <- strsplit(line, " - ", fixed = TRUE)
          dateFrom[lineOut] <- as.Date(unlist(splitLine)[1], 
                                       format = "%d %b %Y")
          # remove from date from line
          line <- unlist(splitLine)[2]
          #print(line)
          
          # extract the to date, using the emoji as separator #############
          if (fuel[lineOut] == "Gas"){
            splitLine <- strsplit(line, "🔥", fixed = TRUE)
          } else {
            splitLine <- strsplit(line, "⚡", fixed = TRUE)
          }
          
          dateTo[lineOut] <- as.Date(unlist(splitLine)[1], 
                                     format = "%d %b %Y")
          # remove to date from line
          line <- unlist(splitLine)[2]
          
          # extract the kWh used in that time period ######################
          splitLine <- strsplit(line, " ", fixed = TRUE)
          kWh[lineOut] <- as.numeric(unlist(splitLine)[1])
          
          # remove number of kWh from line
          line <- unlist(splitLine)[2:length(unlist(splitLine))]
          
          # extract amount of charge or credit ############################
          # first check if the balance is +ve or -ve
          minus <- grepl("-", line[5], fixed = TRUE)
          
          # then remove the minus to separate the amount and balance
          if(minus){
            line[5] <- sub("-", "", line[5])
          }
          splitLine <- strsplit(line[5], "£", fixed = TRUE)
          
          amount[lineOut] <- as.numeric(sub("£", "", 
                                            unlist(splitLine)[2]))
          
          # extract balance ###############################################
          #remove any commas
          balance[lineOut] <- as.numeric(sub(",", "", 
                                             unlist(splitLine)[3]))
          # reapply the minus sign, if there was one
          if(minus){
            balance[lineOut] <- balance[lineOut] * -1
          }
          
          # set charge ####################################################
          cc[lineOut] <- "charge"
          
        } # end of charge or credit check
      } # end check for Bank
      
      # now increment the line out count 
      lineOut <- lineOut + 1
      
    } # end of line length check
    # increment line count for file being read
    lineIn <- lineIn + 1
    
  } # end check for blank lines
} # end read loop

#print(previous)
# tidy up
close(con = con)

###########################################################################
# create the data frame, and plots
###########################################################################
fuel <- as.factor(fuel)
cc <- as.factor(cc)
tranx_df <- data.frame(fuel, dateFrom, dateTo, kWh, cc, amount, balance)
