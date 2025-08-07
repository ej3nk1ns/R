###########################################################################
# Script octo02.R  Plot octopus electricity readings
# July 2025 EAJ
# calls: 
# contains: fn_readDate, fn_readType, fn_readNumbers
# reads: elec-readings.txt
# plots: electricity readings, interactively
# writes: 
###########################################################################
# clear environment 
rm(list = ls())

cat("\n*** Running script octo02 to plot electricity energy readings:\n")

setwd("~/R/Working/Octopus")
elec_file <- "elec-readings.txt"

library(viridisLite)            # for an accessible colour palette
colour9 <- viridis(n = 9)       # define a colour scale

# readLines can retrieve one line at a time
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
    #cat(eDate[lineOut], eType[lineOut], line, "\n")
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
# create the data frame, and plot separate day and night
###########################################################################
eType <- as.factor(eType)
elec_df <- data.frame(eDate, eType, eNight, eDay)

# define colours
colscheme <- c(colour9[7], colour9[1], colour9[5], colour9[3])
palette(colscheme)

# night readings
cat("*** Plotting Night meter readings\n")
barplot(height = elec_df[order(elec_df$eDate), ]$eNight, 
        col = elec_df[order(elec_df$eDate), ]$eType,
        main = "Electricity meter NIGHT readings April 2023 to July 2025",
        space = 0.4,
        cex.names = 0.75,
        cex.axis = 0.9,
        names.arg = elec_df[order(elec_df$eDate), ]$eDate,
        ylim = c(39000, 48500),
        xpd = FALSE,                    # truncate bars at y limits
        ylab = "Meter reading",
        axis.lty = 1,                   # show x axis
        las = 2
)

legend("topleft", legend = sort(unique(elec_df$eType)), fill = colscheme)

# day readings
cat("*** Plotting Day meter readings\n")
barplot(height = elec_df[order(elec_df$eDate), ]$eDay, 
        col = elec_df[order(elec_df$eDate), ]$eType,
        main = "Electricity meter DAY readings April 2023 to July 2025",
        space = 0.4,
        cex.names = 0.75,
        cex.axis = 0.9,
        names.arg = elec_df[order(elec_df$eDate), ]$eDate,
        ylim = c(39000, 48500),
        xpd = FALSE,                    # truncate bars at y limits
        ylab = "Meter readings",
        axis.lty = 1,                   # show x axis
        las = 2
)

legend("topleft", legend = sort(unique(elec_df$eType)), fill = colscheme)

###########################################################################
# plot all readings side by side
###########################################################################
# https://stackoverflow.com/questions/68947230/showing-bar-plot-side-
#by-side-for-two-variables-in-dataframe

# create a matrix of the readings to plot side by side
tpose <- t("row.names<-"(as.matrix(elec_df[3:4]), 
                         elec_df$eDate))
main <- "Electricity readings (Night and Day) from April 2023 to July 2025"

# https://stackoverflow.com/questions/18339725/sorting-matrix-by-column-
#names

# convert column names back to date and plot
cat("*** Plotting Day and Night meter readings together\n")
barplot(tpose[ , order(as.integer(colnames(tpose)))],
        # sort by integer not character
        names.arg = as.Date(as.integer(colnames(
          tpose[ , order(as.integer(colnames(tpose)))])), 
          format = "%Y %m %d"),
        las = 2,
        main = main,
        ylab = "Meter reading",
        ylim = c(39000, 49500),
        xpd = FALSE,            # truncate bars at y limits
        cex.names = 0.8,
        cex.axis = 0.9,
        col = c(colour9[2], colour9[5]),
        legend = TRUE, 
        beside = TRUE)
