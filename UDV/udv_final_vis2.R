#####################################################################################################################
### udv_final_vis2.R  Produce the correlation scatter plot
#####################################################################################################################
###
### Feb 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating scatter plot...")

# open a plot device for the PNG format
png(file = "scatter%d.png", 
    width = plot_width, 
    height = plot_height)  

# start a blank plot based on our variables so lines will be behind the data points
plot(csvfile$Percent1st, csvfile$RofAveScore,
##     main = "1. scatter title\n2. scatter subtitle",
     xlab = "Proportion of children and young people accessing mental health services (2018-19), as a % of this age group",
     ylab = "Local Authority Districts in England, ranked by Deprivation score (1 = highest)",
     ylim = c(340,0),                                    # set limits just outside the data region, in reverse (to flip the y scale)
     type = "n")

# add a line to the plot for the mean of the least deprived 50%, as the assumed 'base rate' line
abline(v = mean_MH, col = "lightgrey")

# add a text label to the mean
text(x = mean_MH - 0.2, y = 348,                         # offset from the line in x, position below data in y 
     "'Base rate'",                                      # text to add
     cex = char_size,    
     col = colour_scatter[3],                            # the mean is the boundary between colours [3] and [4]
     adj = c(0,0),                                       # left justify in x, bottom in y
     srt = 90)                                           # vertical text

# plot the lines for multiples of the standard deviation from the mean
for(k in 1:13){
  abline(v = buckets_MH[k],                              # mean plus or minus whole multiples of sd
         col = "lightgrey", 
         lty = 3)
} # end for

# now build up a key to the numbers assigned to the LADs
# xt = position of text, xn = posn of number box
xt <- 28.5
xn <- 27

# initialise the vectors y_key, key
y_key <- c(0)
key <- c(0)

# define the y positions of text strings in our key (actually superposed to allow different colours)
for(l in 1:LAD_focus){                                               
  y_key[l] <- 200 + l * 7.5                               # start at y = ... and offset each by ...
} # end for

# build a vector of the MH rank of the top LADs for the key
for(l in 1:LAD_focus){
  key[l] <- paste(csvfile$MHrank[csvfile$MHrank[l]],"          ")
} # end for

# can we draw a box on the plot with legend?
legend(x = xn,                                            # x of top left corner
       y = y_key[1] - 7,                                  # y of top left corner
       legend = key,                                      # just the numbers
       cex = char_size,
       text.col = "sienna1",
       text.width = 6,
#       title = "Key",
       box.col = "lightgrey",                             # colour of surrounding box
       box.lwd = 0.5,                                     # line width of box
       y.intersp = 1.8)                                   # vertical spacing

# plot the text name of each LAD
text(xt, y_key, 
     labels = csvfile$LADname[csvfile$MHrank[1:LAD_focus]], 
     adj = c(0,0),                                        # left and bottom justify the text about x,y
     col = colour_scatter[csvfile$colour],                # colour to match the point displayed
     cex = char_size,
     font = 2)                                            # bold

# finally plot the scatter plot of mental health and deprivation data to show where there is correlation
points(csvfile$Percent1st, csvfile$RofAveScore,
       pch = 23,                                           # point type is diamond
       col = colour_scatter[csvfile$colour],               # use same colour for outline and fill
       bg =  colour_scatter[csvfile$colour])

# add some names/numbers, if colour is greater than the limit we have specified
text(csvfile$Percent1st[csvfile$colour > LAD_lim], 
     csvfile$RofAveScore[csvfile$colour > LAD_lim],
     csvfile$MHrank[csvfile$colour > LAD_lim],
     col = "sienna1",                                   # colour to match the map numbers
     cex = char_size,                                   # character multiplier
     font = 2,                                          # bold
     pos = 4,                                           # pos = 1,2,3,4 for below left above right, or a vector if some overlap
     offset = 0.3)                                      # snuggle up to the point (default = 0.5)

# close plot device
dev.off()

# report plot complete
cat(" complete\n")
