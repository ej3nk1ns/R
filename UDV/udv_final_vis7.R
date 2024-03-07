#####################################################################################################################
### udv_final_vis7.R  Produce the Focus 15 plots...
#####################################################################################################################
###
### Mar 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating alternative Focus maps...")

# open a plot device for the PNG format
png(file = "grey%d.png", 
    width = plot_width, 
    height = plot_height)

## plot 1 shows only the areas we are focussed on in colour: overall it is dull
# plot all the England LADs in grey and white...
plot(st_geometry(england),
     col = "lightgrey",                       # fill colour
     lwd = 0.5,                               # line width
     border = "white")

# presentation layer
layoutLayer(title = "Focus:  the most deprived areas in England show excess mental health issues", 
            col = colour_scatter[10],
            sources = "Sources: x, y, and z",
            author = paste0("cartography ", 
                            packageVersion("cartography")), 
            frame = FALSE, 
            north = TRUE, 
            scale = 100)             

# now add the 'focus' areas to the grey plot, almost correct colours
choroLayer(
  x = merged[merged$MHrank <= LAD_focus, ],
  var = "Percent1st",
  nclass=7,                                       # no. of classes (from 4 to 10 only in this selection)
  col = colour_scatter[4:10],                     # match up the colours with the scatterplot? this is still quantiles
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rate of contact with\nmental health services\nas a % of ...",
  legend.title.cex = char_size,                         # text size
  add = TRUE)        

# close plot device
dev.off()

## copy current plot to a (large) PNG file
#dev.print(png, file = "grey%d.png", width = 768, height = 1024)  # A4 portrait

################################################################################################################################

# open a plot device for the second plot, PNG format
png(file = "yellow%d.png", 
    width = plot_width, 
    height = plot_height)

## plot 3 makes the colour breaks broader, to include areas below the mean in the same colour => hideous yellow map
col_breaks <- c(0, mean_MH + 3 * sd_MH, mean_MH + 6 * sd_MH, mean_MH + 9 * sd_MH, mean_MH + 12 * sd_MH)

choroLayer(
  x = merged, 
  var = "Percent1st",
#  nclass=11,                                     # don't specify nclass with breaks
  breaks = col_breaks,
  col = colour_scatter2,                          # ? match up the colours with the scatterplot
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rate of contact with\nmental health services\nas a % of ...",
  legend.title.cex = char_size,                         # text size
  add = FALSE)       

# presentation layer
layoutLayer(title = "Focus:  the most deprived areas in England show excess mental health issues", 
            col = colour_scatter2[1],
            sources = "Sources: x, y, and z",
            author = paste0("cartography ", 
                            packageVersion("cartography")), 
            frame = FALSE, 
            north = TRUE, 
            scale = 100)  

# close second plot device
dev.off()

# report finished
cat(" complete\n")

## copy current plot to a (large) PNG file
#dev.print(png, file = "yellow%d.png", width = 768, height = 1024)  # A4 portrait
