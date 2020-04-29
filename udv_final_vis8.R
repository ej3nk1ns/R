#####################################################################################################################
### udv_final_vis8.R  A grid map, based on cartography example
#####################################################################################################################
###
### Apr 2020
### EAJ
#####################################################################################################################
# for mtq, use gb...


library(sf)
library(cartography)

# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")

# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)

# reset the path to the geopackage file saved in cartography package directory
path_to_gpkg <- system.file("gpkg/bdline_gb.gpkg", package="cartography")

# read in the boundary layer we want
gb <- st_read(dsn = path_to_gpkg, 
              layer = "district_borough_unitary")

# Select only English LADs to match our data (if gb.Census_Code starts with "E" then this is England)
selected <- startsWith(as.vector(gb$Census_Code), "E")

# extract these records into a new sf object for England
england <- st_sf(gb[selected,]) 

# Create a grid layer, cell size area match the median municipality area 
#mygrid <- getGridLayer(
#  x = mtq, 
#  cellsize = median(as.numeric(st_area(mtq))),   # st_area computes the area of a set of geometries
#  var = "POP",
#  type = "hexagonal")

# Create a grid layer, cell size area match the median municipality area 
enggrid <- getGridLayer(
  x = england, 
  cellsize = median(as.numeric(st_area(england))),   # st_area computes the area of a set of geometries
  var = "Hectares",                             # only geographic values in england so approximate with hectares
  type = "hexagonal")

# Compute population density in people per km2
#mygrid$POPDENS <- 1e6 * mygrid$POP / mygrid$gridarea

# plot municipalities (only the backgroung color is plotted)
#plot(st_geometry(mtq), 
#     col = NA, 
#     border = NA, 
#     bg = "#deffff")

# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(england), 
     col = NA, 
     border = NA, 
     bg = "#deffff")

# Plot the population density
choroLayer(x = enggrid, 
           var = "Hectares", 
           method = "geom", 
           nclass=5, 
           col = carto.pal(pal1 = "turquoise.pal", n1 = 5), 
           border = "grey80", 
           lwd = 0.5, 
           legend.pos = "topright", 
           add = TRUE,
           legend.title.txt = "Population Density\n(people per km2)") 

layoutLayer(title = "Population Distribution in Martinique", 
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE,
            theme = "turquoise.pal")

# north arrow
north(pos = "topleft")
