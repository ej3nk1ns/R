#####################################################################################################################
### udv_final_vis1.R call the different scripts to plot various visualisations
#####################################################################################################################
### Code adapted from https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html
###
### Data from a file provided to me, but mental health data originally from
### https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-bulletin/2018-19-annual-report
### and deprivation data originally from
### https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
### see also the research report at
### https://www.gov.uk/government/publications/english-indices-of-deprivation-2019-research-report
### for explanation of how the data is calculated.
###
### Borderline UK map data from
### https://www.ordnancesurvey.co.uk/opendatadownload/products.html
###
### Feb 2020
### EAJ + 2
#####################################################################################################################

# clear the environment
rm(list=ls())
setwd("~/R/Working/UDV2020")

# also needs (on Debian) sudo apt-get install libudunits2-dev
# and sudo apt-get install libgdal-dev

# install packages one time only

#install.packages("rgeos")       # done
#install.packages("units")       # done
#install.packages("sf")          # done
#install.packages("cartography") # done
#install.packages("viridis")     # done

# load the packages
library(sf)                      # simple features (for maps)
library(cartography)             # maps package
library(viridis)                 # nice colours

# set some parameters...
plot_width <- 768                # 768 pixels wide by 1024 high = A4 portrait
plot_height <- 1024              # vv for A4 landscape
# set the character expansion factor for text on the plots
char_size <- 0.7
# how many LADs we are interested in: 12 or 19?
LAD_focus <- 19
# set the limit where we start being interested in these LADs
LAD_lim <- 6                                                      # these two are not independent!

# set the path to the geopackage file saved in cartography package directory
path_to_gpkg <- system.file("gpkg/bdline_gb.gpkg", package="cartography")

# show all layers
#print(st_layers(dsn = path_to_gpkg))
#cat("\n")

# read in the boundary layer we want
gb <- st_read(dsn = path_to_gpkg, 
              layer = "district_borough_unitary")

cat("\n")
cat("## Local authority district boundaries read into data frame", "\n")

# choose shorter names for the MH/deprivation columns
cols <- c("LADcode","LADname","Popn","No1st","Percent1st","No2nd","Percent2nd",
          "AveRank","RofAveRank","AveScore","RofAveScore","Prop10","RofProp10")

# read the provided Mental Health/Deprivation data in 
csvfile <- read.csv("Conjoined data CSV.csv", 
                    header = TRUE, 
                    col.names = cols)

cat("## Mental health & deprivation data read into data frame", "\n")

# calcuate the mean of the mental health data over the least deprived 50% of LADs
# we will call this the 'assumed base rate' for MH, before deprivation increases this figure in our 15 focus LADs

# but first... there are 14 LADs with missing values for RofAveScore; these will be excluded from the calculation in work file 1
csvfile_work1 <- csvfile[order(csvfile$RofAveScore, na.last = NA), ]       

# now data is sorted, select the least deprived 50% of the LADs into work file 2 (+ 1 for rounding up)
csvfile_work2 <- csvfile_work1[(round(nrow(csvfile_work1)/2) + 1) : nrow(csvfile_work1),] 

# calculate the variables we need, excluding any additional mising values
mean_MH <- round(mean(csvfile_work2$Percent1st, na.rm = TRUE), digits = 3)      # mean of least deprived 50%
sd_MH <- round(sd(csvfile_work2$Percent1st, na.rm = TRUE), digits = 3)          # standard deviation, likewise

## decided not to use these scales in favour of one map accentuating the most-deprived LADs
#colour_depr <- viridis(9, direction = 1)
#colour_mh <- viridis(9, direction = -1)

# define a colour scale that is measured in standard deviations between the max and min values of MH
max_MH <- max(csvfile$Percent1st, na.rm = TRUE)                                 # find the largest mh value
min_MH <- min(csvfile$Percent1st, na.rm = TRUE)                                 # find the smallest MH value
range_MH <- max_MH - min_MH                                                     # distance between max and min
bucket_no <- as.integer(round(range_MH / sd_MH) + 1)                            # number of standard devs that cover the range

# colour the plots according to where in the range they fall, in standard deviations 
colour_scatter <- viridis(bucket_no, direction = -1)
colour_scatter2 <- viridis(4, direction = -1)                                   # different grouping, yellow plot

# build a vector of breaks between buckets to map colours to points (values are the lower limit of the bucket)
buckets_MH <- c(0.0)                                                            # initialise vector

# calculate from 3 SDs below the mean to an extra bucket above the highest value (needed for the comparison below)
for (i in -3:(bucket_no - 4 + 1)) {
  buckets_MH[i + 4] <- mean_MH + (i * sd_MH)                                    # lower limit of buckets defined below and above the mean
} # end for

# check values
##print(buckets_MH)   ### [1]  3.830  6.268  8.706 11.144 13.582 16.020 18.458 20.896 23.334 25.772 28.210 30.648 33.086 35.524

# sort the data frame by the MH access rate (keeping missing values)...
csvfile <- csvfile[order(csvfile$Percent1st, na.last = TRUE, decreasing = TRUE), ]

# ... so that we can add a ranking of MH access rate to use as a key to the most-signicficant LADs
csvfile$MHrank <- seq.int(nrow(csvfile))  

# need to compare the MH value with the vector of bucket limits and find where it first is greater
for (j in 1:nrow(csvfile)){
  
# check our assignment of colour is correct by looking at the top 20 LADs
#    if(csvfile$MHrank[j] < 20) {
#      print(which(csvfile$Percent1st[j] < buckets_MH)[1])   # what is the index of the first bucket that is bigger than this Percent1st?
#    } # end if
  
  # assign colour to this LAD based on the bucket below the one that is bigger
  csvfile$colour[j] <- (which(csvfile$Percent1st[j] < buckets_MH)[1]) - 1
} # end for

cat("## Colour buckets assigned\n")

############################################################################################################################
# the plots are called by the 'source' statement. comment #/uncomment according to the ones that you want
############################################################################################################################

# call the code for the scatter plot #######################################################################################
source("udv_final_vis2.R")

# Select only English LADs to match our data (if gb.Census_Code starts with "E" then this is England)
selected <- startsWith(as.vector(gb$Census_Code), "E")

#print(selected)
#print(gb$Census_Code)

# extract these records into a new sf object for England
england <- st_sf(gb[selected,])   

# call the code for the boundary map #######################################################################################
#source("udv_final_vis3.R")

# use merge to join the MH/deprivation data frame with the geographic sf object
merged <- merge(csvfile, 
                england, 
                all.x = TRUE, 
                by.x = "LADcode", 
                by.y = "Census_Code")

# turn the merged data frame into a geographic sf object again
merged <- st_as_sf(merged)

# show some merged data before plotting
cat("## Data merge complete", "\n")

#print(merged$LADcode, merged$LADname, merged$Percent1st, merged$RofAveScore)
#print(head(merged, 10))

# call the code for the separate deprivation map ###########################################################################
#source("udv_final_vis4.R")

# call the code for the mental health map ##################################################################################
#source("udv_final_vis5.R")

# call the code for the map focusing on only the most significant LADs #####################################################
source("udv_final_vis6.R")

# call the code for maps considered but rejected ###########################################################################
#source("udv_final_vis7.R")

cat("## End of source")