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
petitionNo <- list(780361, 766093)

# initialise the dataframe
combined_df <- data.frame()

# loop over petitions in the list
for (i in 1:length(petitionNo)) {
  petNo <- petitionNo[i]
  
  # build the petition URL and show it
  petitionURL <- paste0("https://petition.parliament.uk/petitions/", 
                        petNo, ".json")
  print(petitionURL)
  
  # retrieve the dataset for a particular petition, restructured
  json_data <- read_json(path = petitionURL, 
                         simplifyVector = TRUE)
  
  # build the required data into a dataframe
  temp_df <- data.frame(json_data$data$attributes$signatures_by_constituency)
  
  # add the petition number (at the front?)
  #temp_df <- cbind("petition" = petitionNo[i], temp_df)
  temp_df["petition"] <- petitionNo[i]
  
  # combine data in one dataframe
  combined_df <- rbind(combined_df, temp_df)
}

###########################################################################
# write the data to CSV
###########################################################################
write.csv(combined_df, 
          file = "greyPetitions.csv", row.names = FALSE)
