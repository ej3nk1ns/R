#####################################################################################################################
### udv_final_vis6.R  Produce the focus plot, with colours aligned to the scatter plot
#####################################################################################################################
###
### Mar 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating the focus map...")

# open a plot device for the PNG format
png(file = "focus%d.png", 
    width = plot_width, 
    height = plot_height)

# first, plot empty borders to show the cells with no data
plot(st_geometry(england),                           # just the borders (why do we lose somerset in merged?)
     border = "darkgrey",
     lwd = 0.5)

# plot all LADs, using our own defined colour scale (buckets by sd)
choroLayer(
  x = merged, 
##  nclass=13,                                      # can't use classes with breaks 
  breaks = buckets_MH,                            # breaks defines the exact colour buckets
  var = "Percent1st",
  col = colour_scatter,                           # match up the colours with the scatterplot
  border = "white",                               # line colour
  lwd = 0.5,                                      # line width
  legend.pos = "topleft", 
  legend.title.txt = "Colour scale mapped to the standard deviation (2.4) multiples above and below the mean mental health access rate (11.1).\n(the mean and standard deviation were calculated using the access rates of the least deprived 50% of LADs.)",
  legend.title.cex = char_size,                   # text size
  add = TRUE)                                     # add to the current plot

# add text labels to the plot, for the LADs we are interested in
labelLayer(
  x = merged[merged$MHrank <= LAD_focus, ],       # choose only the rows where the rank is within our focus
  txt = "MHrank",                                 # show the MH rank on the geographical area   
  col= "sienna1",    
  cex = char_size,                                # character expansion
  font = 2,                                       # bold
  halo = TRUE,                                    # show an outline round the text
  bg = "white",                                   # outline colour
  r = 0.05,                                       # outline radius, default 0.1
  overlap = FALSE, 
  show.lines = TRUE)                              # draw lines linking the label and its point, if it is offset

# presentation layer - titles, footnotes, scale and compass (must be after the labelLayer?)
layoutLayer(title = "",          ##"6. map title", 
            col = colour_scatter[bucket_no],      # use the most extreme colour
            sources = "See scatter plot for key to Local Authority District numbering",           
            frame = TRUE, 
            north = TRUE, 
            scale = 100)             

# close plot device
dev.off()

# report completion
cat(" complete\n")
