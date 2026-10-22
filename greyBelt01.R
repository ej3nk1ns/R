###########################################################################
# greyBelt01.R
# Fetch data from the UK parliamentary petitions site
# EAJ 
# October 2026
###########################################################################
library(jsonlite)

# clear environment 
rm(list = ls())

cat("\n*** Running script greyBelt01.R for Parliamentary petitions\n")

# reverse the slashes if copied from Windows!
wdir <- "C:/Users/ej3nk/R/greyBelt/"
setwd(wdir)

# eg. https://petition.parliament.uk/petitions/766093.json

# set up a list of the required petition numbers
petitionNo <- list(769589, 767687, 766093, 777301, 780361, 767016, 776551,
                   725558, 759587, 762640, 764785, 762214, 762304)

# initialise the dataframes
combiOpen_df <- data.frame()
combiClosed_df <- data.frame()

# today's date
today <- Sys.Date()

###########################################################################
# read and process each petition in the list
###########################################################################
for (i in 1:length(petitionNo)) {
  petNo <- petitionNo[i]
  
  # build the petition URL and show it
  petitionURL <- paste0("https://petition.parliament.uk/petitions/", 
                        petNo, ".json")
  print(petitionURL)
  
  # retrieve the dataset for this petition, restructured
  json_data <- read_json(path = petitionURL, 
                         simplifyVector = TRUE)
  
  # build the required data into a dataframe
  temp_df <- data.frame(json_data$data$attributes$signatures_by_constituency)
  
  # add the petition number 
  temp_df["petition"] <- petitionNo[i]
  
  # add petition title from the header fields 
  temp_df["title"] <- json_data$data$attributes$action
  print(json_data$data$attributes$action)

  cat("*** Petition is", json_data$data$attributes$state, "\n\n")
  
  # actions depending on whether petition is open or closed
  if(json_data$data$attributes$state == "open"){
    
    # add header fields
    temp_df["creator"] <- json_data$data$attributes$creator_name
    temp_df["closingDate"] <- json_data$data$attributes$closing_date
    
    # combine data into the appropriate dataframe
    combiOpen_df <- rbind(combiOpen_df, temp_df)
    
  } else{
    
    # petition is closed (check in creators.csv for name?)
    temp_df["creator"] <- "-"   # or NA
    temp_df["closingDate"] <- substr(json_data$data$attributes$closed_at,
                                     1, 10)
    # combine data into the appropriate dataframe
    combiClosed_df <- rbind(combiClosed_df, temp_df)
  }
}

###########################################################################
# add regional boundaries from 2025, downloaded manually as CSV from 
# https://pages.mysociety.org/2025-constituencies/downloads/parliament-con-2025-parl-constituencies-2025-csv/latest
###########################################################################
regions_df <- data.frame(read.csv("parl_constituencies_2025.csv"))

# we only need a couple of columns from this data
regions_df <- regions_df[ , c(5, 8)]

# merge regions with the open petitions data frame
combiOpen_df <- merge(combiOpen_df, regions_df, 
                      by.x = "ons_code",
                      by.y = "gss_code",
                      all.x = TRUE,
                      sort = FALSE)

# sort columns in open petitions dataframe again
combiOpen_df <- combiOpen_df[ , c(2,1,9,3:8)]

# merge regions with the closed petitions data frame
combiClosed_df <- merge(combiClosed_df, regions_df, 
                        by.x = "ons_code",
                        by.y = "gss_code",
                        all.x = TRUE,
                        sort = FALSE)

# sort columns in the closed petitions dataframe again
combiClosed_df <- combiClosed_df[ , c(2,1,9,3:8)]

###########################################################################
# write the data for open and closed petitions to CSV
###########################################################################
# write open petitions, filename includes run date
filename <- paste0("open-", today, ".csv")
write.csv(combiOpen_df, 
          file = filename, 
          row.names = FALSE)
cat("\n*** Open petitions written to file", filename, "\n")

# write closed petitions
filename <- paste0("closed-", today, ".csv")
write.csv(combiClosed_df, 
          file = filename, 
          row.names = FALSE)
cat("*** Closed petitions written to file", filename, "\n")

# retrieve the unique creators for open petitions
creators_df <- data.frame(unique(combiOpen_df[ , c(6, 8, 9)]))

# and save to file, for reference on future runs
filename <- paste0("creators-", today, ".csv")
write.csv(creators_df,
          file = filename,
          row.names = FALSE)
cat("*** Petition creators written to file", filename, "\n")
