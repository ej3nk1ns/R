#####################################################################################################################
### udv_draft_vis1.R
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

# load the packages
library(sf)
library(cartography)

# set the path to the geopackage file saved in cartography package directory
path_to_gpkg <- system.file("gpkg/bdline_gb.gpkg", package="cartography")

# show all layers
print(st_layers(dsn = path_to_gpkg))
cat("\n")

# read in the boundary layer we want
gb <- st_read(dsn = path_to_gpkg, layer = "district_borough_unitary")

cat("\n")
cat("## Local authority district boundaries read into data frame", "\n")

# choose shorter names for the MH/deprivation columns
cols <- c("LADcode","LADname","Popn","No1st","Percent1st","No2nd","Percent2nd",
          "AveRank","RofAveRank","AveScore","RofAveScore","Prop10","RofProp10")

# read the provided Mental Health/Deprivation data in 
csvfile <- read.csv("Conjoined data CSV.csv", header = TRUE, col.names = cols)

cat("## Mental health & deprivation data read into data frame", "\n")
cat("## Generating scatter plot...")
# scatter plot of mental health and deprivation data to look for correlation
# flip the y scale! So deprivation increases as we go up
plot(csvfile$Percent1st, csvfile$RofAveScore,
               main = "Deprivation and Mental Health Issues in Young People\nEach point represents a Local Authority District in England",
               xlab = "Rate of access to Mental Health services as a % of young people",
               ylab = "Local Deprivation measured as a rank",    #:\nsmaller values represent higher deprivation",
               ylim = c(333,0),                                  # set limits just outside the data region, in reverse
               pch = 23, col = "sienna1", bg = "steelblue3")     # point type, colour and fill

cat(" complete\n")

# Select only English LADs to match our data (if gb.Census_Code starts with "E" then this is England)
selected <- startsWith(as.vector(gb$Census_Code), "E")

#print(selected)
#print(gb$Census_Code)

# extract these records into a new sf object for England
england <- st_sf(gb[selected,])   

cat("## Generating boundary plot...")

# plot the England LAD boundaries for confirmation
plot(st_geometry(england),
    col = "darkgrey",                       # fill colour
    lwd = 0.5,                              # line width
    main = "Local Authority District (LAD) boundaries in England")

cat(" complete\n")

# use merge to join the MH/deprivation data frame with the geographic sf object
merged <- merge(csvfile, england, all.x = TRUE, by.x = "LADcode", by.y = "Census_Code")

# turn the merged data frame into a geographic sf object again
merged <- st_as_sf(merged)

# show some merged data before plotting
cat("## Data merged", "\n\n")
#print(merged$LADcode, merged$LADname, merged$Percent1st, merged$RofAveScore)
print(head(merged, 10))

# if no output device is specified, the default is the output window on screen...

# start the map plotting
cat("\n")
cat("## Now generating the choropleth plots...")

# plot the choropleth of deprivation from the merged data object
choroLayer(
  x = merged, 
  var = "RofAveScore",
#  method = "geom",                               # geometric? default is quantile
  nclass=9,                                       # no. of classes, chosen by formula 1+ 3.3 *log10(N) 
  col = carto.pal(pal1 = "blue.pal", n1 = 9),     # colour palette and number of divisions
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rank of Average Score from\nthe Index of Deprivation 2019\nLower numbers mean\nHigher Deprivation",
  add = FALSE)                                    # this is a new plot

# presentation layer
layoutLayer(title = "A Deprivation map of England as it affects Young People", 
            col = "skyblue",
            sources = "Sources: x, y, and z",
            author = paste0("cartography ", 
                            packageVersion("cartography")), 
            frame = FALSE, 
            north = TRUE, 
            scale = 100)             

# now plot mental health data from merged data object
# first, plot empty borders to show the cells with no data
plot(st_geometry(merged),
#     lwd = 0.5)
     border = "darkgrey")

# now plot the choropleth layer of mental health
choroLayer(
  x = merged, 
  var = "Percent1st",
#  method = "geom",                               # default method is quantiles
  nclass=9,                                       # no. of classes
  col = carto.pal(pal1 = "orange.pal", n1 = 9),   # warm colours to contrast with cool 
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rate of contact with\nmental health services\nas a % of ...",
  add = TRUE)                                     # add to the blank border plot 

# presentation layer
layoutLayer(title = "A Mental Health map of England for Young People (age 0-19)", 
            col = "sienna1",
            sources = "Sources: x, y, and z",
            author = paste0("cartography ", 
                            packageVersion("cartography")), 
            frame = FALSE, 
            north = TRUE, 
            scale = 100)             

cat(" complete")