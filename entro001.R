##################################################################################
# Playing with entropy for information gain
# RHUL again 
# 
# EAJ July 2023
##################################################################################

# uncomment these if needed
#install.packages('readxl')     # read data from a Microsoft spreadsheet
#install.packages("entropy")
# load packages
library(readxl)
library(entropy)

# clear environment and set working directory
rm(list = ls())
wdir <- "~/Documents/Jobs/RHUL again/"
idir <- "../../../projectPrep2023/data/"

setwd(wdir)

### import sheet of Excel file into R
ccri_df <- read_excel(paste0(idir, "CCRI_Model_Draft20210420.xlsx"), 
                      sheet = "CCRI",
                      na = c("x")    )

# shorten column names
colShortNames <- c("ISO1", "UNSubRegion", "UNICEFRegion", "UNICEFCountry", 
                   "ISO5", "WaterScarcity", "RiverineFloods", "CoastalFloods", 
                   "TropicalCyclones", "Malaria", "Zika", "Dengue", "Aedes", 
                   "VectorBorneDs", "TempAnomaly", "AmbientAirPoll", "LeadPoll", 
                   "EnvironPoll", "ClimateShocks", "ChildHealth", "Nutrition", 
                   "MaternalHealth", "Education", "ChildProtection", 
                   "SocialProtection", "WASH", "PovAndInequality", 
                   "Communication", "Governance", "Displacement", 
                   "ChildVulnbility", "CCRIndex", "Rank")
ccri_df <- setNames(ccri_df, colShortNames)

# split off CCRIndex (what we will try to predict) and other fields not needed
CCRIndex <- ccri_df$CCRIndex
ccri_df <- subset(ccri_df, select = -c(1:5,32:33))

# create a vector to hold the classes
CCRIclass <- vector(mode="character", length=length(CCRIndex))
counts <- vector(mode = "numeric", length = 4)

# bin size = range / 4.0  => 4 bins
#cat("Max value CCRI = ", max(CCRIndex), "\n")
#cat("Min value CCRI = ", min(CCRIndex), "\n")
#binSize <- (max(CCRIndex) - min(CCRIndex)) / 4.0
#cat("Bin size CCRI = ", binSize, "\n")

# bin the index into 4 categories so that we have a classification problem
# not using bin size. Changed from < to <= to get same results as discretize
for (i in 1:length(CCRIndex)){
  if (CCRIndex[i] <= 2.5) {
#    cat(CCRIndex[i], "low", "\n")
    CCRIclass[i] <- "low"
    counts[1] <- counts[1] + 1
    
  } else if (CCRIndex[i] <= 5) {
#    cat(CCRIndex[i], "lowish", "\n")
    CCRIclass[i] <- "lowish"
    counts[2] <- counts[2] + 1
    
  } else if (CCRIndex[i] <= 7.5) {
#    cat(CCRIndex[i], "highish", "\n")
    CCRIclass[i] <- "highish"
    counts[3] <- counts[3] + 1
    
  } else {
#    cat(CCRIndex[i], "high", "\n")
    CCRIclass[i] <- "high"
    counts[4] <- counts[4] + 1
  } 
}

# 4 bins = 5 boundary points
png("CCRIHist.png", width = 720, height = 720)
hist(CCRIndex, breaks = c(0, 2.5, 5.0, 7.5, 10))
dev.off()

# why don't these match? Sorted.
cat("Count in each bin = ", counts, "\n")                    # 25 84 80 13
dis_CCRI <- discretize(CCRIndex, numBins = 4, r = c(0,10))   # 30 85 76 11
cat("Discretised counts (range 0 - 10) =", dis_CCRI, "\n")

# theoretical entropy with 4 bins
cat("Theoretical entropy with 4 bins = ", log2(4), "\n")

# estimate of empirical entropy
entCCRI <- entropy(dis_CCRI, unit = "log2")
cat("CCRI entropy in bits = ", entCCRI, "\n")

# discretise an indicator
dis_WS <- discretize(ccri_df$WaterScarcity, numBins = 4) # 77 24 35 66
ent_WS <- entropy(dis_WS, unit = "log2")
cat("Water Scarcity entropy in bits = ", ent_WS, "\n")
png("WS_Hist.png", width = 720, height = 720)
hist(ccri_df$WaterScarcity, breaks = c(0, 2.5, 5.0, 7.5, 10))
dev.off()

# let's have a function
calcEnt <- function(indic, numBins = 4){
  # bin the indicator and calculate its entropy
  dis_indic <- discretize(indic, numBins) 
  cat("Bin counts", dis_indic, "Total ", sum(dis_indic), "\n")
  ent_indic <- entropy(dis_indic, unit = "log2")
  cat("Calculated entropy (no NAs), with", numBins, "bins = ", ent_indic, "\n")
  return(ent_indic)
}
# apply the function to check results - data includes NAs
#cat(calcEnt(ccri_df$WaterScarcity), "\n")

# apply the function to all columns, after brutally dropping NAs
ccri_df <- ccri_df[complete.cases(ccri_df), ]
res <- apply(X = ccri_df, MARGIN = 2, FUN = calcEnt)

# maybe 4 bins was ambitious, trying with only 2
res <- apply(X = ccri_df, MARGIN = 2, FUN = calcEnt, numBins = 2)

# and the index (still includes data from dropped NA rows)
cat("Binning the index: ")
Index <- calcEnt(CCRIndex, numBins = 2)
png("IndexHist.png", width = 720, height = 720)
hist(CCRIndex, breaks = c(0, 5.0, 10))
dev.off()

# how much information do we gain if we split the data set on indicator x?
# for WaterScarcity:
# Bin counts 31 83 Total  114 Calculated entropy (no NAs), with 2 bins =  0.8442202 
