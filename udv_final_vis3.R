#####################################################################################################################
### udv_final_vis3.R  Produce the LAD boundary plot
#####################################################################################################################
###
### Feb 2020
### EAJ + 2
#####################################################################################################################

cat("## Generating boundary plot...")

# open a plot device for the PNG format
png(file = "boundary%d.png", 
    width = plot_width, 
    height = plot_height)                    # default resolution is 72 ppi

# plot the England LAD boundaries for confirmation
plot(st_geometry(england),
     col = "darkgrey",                       # fill colour
     lwd = 0.5,                              # line width
     main = "Local Authority District (LAD) boundaries in England")

# close the plot device
dev.off()

cat(" complete\n")
