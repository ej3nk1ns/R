#####################################################################################################################
### udv_final_vis4.R  Produce the deprivation choropleth map
#####################################################################################################################
###
### Feb 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating deprivation choropleth plot...")

# open a plot device for the PNG format
png(file = "deprivation%d.png", 
    width = plot_width, 
    height = plot_height)

# plot the choropleth of deprivation from the merged data object
choroLayer(
  x = merged, 
  var = "RofAveScore",
#  method = "geom",                                # geometric? default is quantile
  nclass=9,                                       # no. of classes, chosen by formula 1+ 3.3 *log10(N) 
#  col = colour_depr,                              # our colour palette for deprivation...
  col = carto.pal(pal1 = "blue.pal", n1 = 9),     # colour palette and number of divisions
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rank of Average Score from\nthe Index of Deprivation 2019\nLower numbers mean\nHigher Deprivation",
  legend.title.cex = char_size,                         # text size
  add = FALSE)                                    # this is a new plot

# presentation layer
layoutLayer(title = "A Deprivation map of England as it affects Young People (colour scale to be reversed)", 
     #       col = colour_depr[1],              # pick the end colour
            col = "skyblue",
            sources = "Sources: x, y, and z",
            author = paste0("cartography ", 
                            packageVersion("cartography")), 
            frame = FALSE, 
            north = TRUE, 
            scale = 100)          

# close plot device
dev.off()

cat(" complete\n")

## copy current plot to a (large) PNG file
#dev.print(png, file = "deprivation%d.png", width = 768, height = 1024)   # A4 portrait