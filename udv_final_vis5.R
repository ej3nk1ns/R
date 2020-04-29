#####################################################################################################################
### udv_final_vis5.R  Produce the mental health choropleth map
#####################################################################################################################
###
### Feb 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating mental health choropleth plot...")

# open a plot device for the PNG format
png(file = "m_health%d.png", 
    width = plot_width, 
    height = plot_height)

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
#  col = colour_mh,
  col = carto.pal(pal1 = "orange.pal", n1 = 9),   # warm colours to contrast with cool 
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Rate of contact with\nmental health services\nas a % of ...",
  legend.title.cex = char_size,                         # text size
  add = TRUE)                                     # add to the blank border plot 

# presentation layer
layoutLayer(title = "A Mental Health map of England for Young People (age 0-19)", 
            col = "sienna1",                               ##colour_mh[9],
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
#dev.print(png, file = "m_health%d.png", width = 768, height = 1024)    # A4 portrait