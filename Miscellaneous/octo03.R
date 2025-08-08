###########################################################################
# Script octo03.R  Plot octopus credits and charges
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readNumbers <- are these used?
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
# define functions to process different data types
###########################################################################
#fn_readDate <- function(field){
#  
#  cat(lineIn, "readDate date", field, "\n")
#  
#  # convert to date type
#  field <- as.Date(field, format = "%d %b %Y")
#  return(field)
#}
###########################################################################
fn_saveDate <- function(dateLine){
  # save the date information in a useful format
  
  # insert a space between day and month
  # https://stackoverflow.com/questions/26896971/add-space-between-two-
  #letters-in-a-string-in-r#26897007
  dateLine <- gsub("([0-9])([A-Z])", "\\1 \\2", dateLine)
  #cat("Saved date:", dateLine, "\n")
  
  return(dateLine)
}

###########################################################################
fn_saveYear <- function(dateLine){
  # extract the year only
  # https://stackoverflow.com/questions/40744463/how-to-extract-#numbers-
  #from-text
  savedYear <- as.numeric(strsplit(dateLine, "\\D+")[[1]][-1])
  #cat("Saved year:", savedYear, "\n")
  return(savedYear)
}
###########################################################################
fn_setDate <- function(string){
  # convert to a date field
  stringAsDate <- as.Date(string, format = "%d %b %Y")
  return(stringAsDate)
}
###########################################################################
fn_numeric <- function(string){
  # convert a string, maybe with commas, to a number
  number <- as.numeric(sub(",", "", string))
  return(number)
}
###########################################################################
fn_plotBalance <- function(df, main){
  # plot the balance after each transaction
  
  plot(x = df$dateTo, 
       y = df$balance,
       type = "n",
       main = main,
       ylab = "Account balance (£)",
       xlab = "",
       ylim = c(-3500, 1000),
       cex.axis = 0.8,
       las = 2)
  
  # add lines in specified colour
  lines(x = df$dateTo, 
        y = df$balance,
        col = colour9[1])
  
  # add points coloured for credit or charge
  points(x = df$dateTo, 
         y = df$balance,
         pch = 19,
         col =  df$cc, ##sign,
         bg = df$cc)  ##sign)
  
  abline(h = 0,
         col = "darkgrey",
         lty = "dashed")
  
  legend("topleft", 
         legend = #sort(
           unique(df$cc), 
         #decreasing = TRUE),
         fill = colscheme)
  
  # label the points with the first letter of fuel, offset in x
  text(x = df$dateTo + 10, 
       y = df$balance,  
       label =  substr(df$fuel, 1, 1),
       cex = 0.75)     
  
  mtext("Charges, credits and payments", side = 3)
}

###########################################################################
# main script
###########################################################################
# need to open the connection with file() to keep position as we read
con = file(cc_file, "r")

# count the lines we are reading and writing
lineIn <- 0
lineOut <- 1

# define vbles to save and count previous lines, to retrieve some dates 
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
      previous[i] <- fn_saveDate(line)
      
      # save the year separately
      if(grepl("202", previous[i], fixed = TRUE)){
        # this is a year field, save it until it changes
        year <- fn_saveYear(previous[i])
      }
      i <- i + 1
    } else {
      
      # now check the type of record, is it Bank or Fuel?
      if (substr(line, 1,3) == "Ban"){
        
        ###################################################################
        # this is a bank payment record
        ###################################################################
        #cat("Bank", line, "\n")
        fuel[lineOut] <- "Bank"
        
        # extract bank transfer date from the 'previous' list #############
        dateFrom[lineOut] <- fn_setDate(paste(previous[i-1], year))
        # set both dates the same
        dateTo[lineOut] <- fn_setDate(paste(previous[i-1], year))
        
        # extract amount paid #############################################
        splitLine <- strsplit(line, "£", fixed = TRUE)
        print(splitLine)
        split2 <- strsplit(unlist(splitLine)[2], " ", fixed = TRUE)
        #print(split2)
        
        # remove any commas
        amount[lineOut] <- fn_numeric(unlist(split2)[1])
        
        # extract balance #################################################
        balance[lineOut] <- as.numeric(unlist(splitLine)[3])
        
        ## we have not checked for a negative balance!
        
        
        # initialise other fields
        kWh[lineOut] <- 0
        cc[lineOut] <- "credit"
        
      } else {
        
        ###################################################################
        # this is a gas or electricity record
        ###################################################################
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
            # credit, but not the special case credit line
            
            # extract date ################################################
            dateFrom[lineOut] <- fn_setDate(paste(previous[i-1], year))
            # set both dates the same
            dateTo[lineOut] <- fn_setDate(paste(previous[i-1], year))
            
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
            balance[lineOut] <- fn_numeric(unlist(splitLine)[3])
            if (minus){
              # reapply the minus sign
              balance[lineOut] <- balance[lineOut] * -1
            }
            
            # set credit and initialise other fields ######################
            cc[lineOut] <- "credit"
            kWh[lineOut] <- 0
            
          } # end special case check 
        } else {
          
          #################################################################
          # this is a charge, not a credit 
          #################################################################
          
          # extract the from date #########################################
          splitLine <- strsplit(line, " - ", fixed = TRUE)
          dateFrom[lineOut] <- fn_setDate(unlist(splitLine)[1])
          
          # remove from date from line
          line <- unlist(splitLine)[2]
          
          # extract the to date, using the emoji as separator #############
          if (fuel[lineOut] == "Gas"){
            splitLine <- strsplit(line, "🔥", fixed = TRUE)
          } else {
            splitLine <- strsplit(line, "⚡", fixed = TRUE)
          }
          
          dateTo[lineOut] <- fn_setDate(unlist(splitLine)[1])
          
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
          balance[lineOut] <- fn_numeric(unlist(splitLine)[3])
          
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

# tidy up
close(con = con)

###########################################################################
# create the data frame, and plots
###########################################################################
fuel <- as.factor(fuel)
cc <- as.factor(cc)

tranx_df <- data.frame(fuel, dateFrom, dateTo, kWh, cc, amount, balance)

# add a column for +ve or -ve balance
tranx_df$sign <- ifelse(tranx_df$balance >= 0, 1, 2)

# define colours
colscheme <- c("red", "black")
#colscheme <- c("black", "red")

# set the colour order
palette(colscheme)

# set up the plot - some entries are post-dated ha ha
cat("*** Plotting balance of energy account in transaction order\n")

mn <- "Energy account balance in transaction order"
fn_plotBalance(tranx_df, mn)

text(x = as.Date("2023-08-01"), 
     y = -2500, 
     label = "Look what Octopus hath wrought!")

###########################################################################

# now sort and plot again
cat("*** Plotting balance of energy account in date order\n")

mn <- "Energy account balance in date order"
# sort by balance as well for line continuity on the plot
fn_plotBalance(tranx_df[order(tranx_df$dateTo, -tranx_df$balance), ], mn)

text(x = as.Date("2023-12-14"), 
     y = -2500, 
     label = "Notes\n\nMeters were read April, May, June, November 2023,
     January, February, July 2024 and March, April, May 2025.
     This generally triggers credits, due to over-estimating.\n\n
     B = Bank transfer,  E = Electricity charge/credit,  G = Gas charge/credit.")

