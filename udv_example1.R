#####################################################################################################################
### udv_example1.R plot a simple bar chart to show length is easier to perceive than area
###
### Apr 2020
### EAJ
#####################################################################################################################

# clear the environment
rm(list=ls())
setwd("~/R/Working/UDV2020")

cat("## Generating bar plot...")

# add the data from 09/04/2020 to a data frame 
cov_df <- data.frame(name = c("England", "Scotland", "Wales", "N.I."), cases = c(54554, 4957, 4089, 1477))

# set some parameters...
plot_width <- 256                # 768 pixels wide by 1024 high = A4 portrait. 768/2 = 350+30+4 = 384. 768/3 = 256 
plot_height <- 1024              # vv for A4 landscape

# open a plot device for the PNG format
png(file = "cov%d.png", 
    width = plot_width, 
    height = plot_height)  

# create a bar plot
barplot(cov_df$cases,
        names.arg = cov_df$name,
        col = "aquamarine3",
        border = NA,
        main = "Total UK COVID-19 cases\nas of Thursday 9th April",
        ylab = "Number of cases to date")


# close plot device
dev.off()

# report plot complete
cat(" complete\n")
