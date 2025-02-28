# THis script generates the main maps of NIgeria and Africa


rm(list=ls()) #cleaning the environment


# installing necessary packages
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
remove.packages("lwgeom")
install.packages("lwgeom", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
install.packages("tmap", dependencies = TRUE)
install.packages("tmaptools")
library(sf)
library(tmap)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmaptools)
library(dplyr)


library(lwgeom)

################################################################################
#loading Nigeria Map

nigeria_shp <- st_read("C:/Users/luisf/Downloads/gadm41_NGA_2.shp")  
ggplot(data = nigeria_shp) +
  geom_sf(fill = "darkred", color = "black") +
  ggtitle("Nigeria - Administrative Boundaries") +
  theme_minimal()

print(nigeria_shp)
colnames(nigeria_shp)

sokoto_shp <- nigeria_shp[nigeria_shp$NAME_1 == "Sokoto", ]
print(unique(sokoto_shp$NAME_2))  # List LGAs in Sokoto

lga_column <- "NAME_2"  

# Filter for Tureta and Tangaza
highlighted_lgas <- c("Tureta", "Tangaza")
highlighted_shp <- nigeria_shp[nigeria_shp[[lga_column]] %in% highlighted_lgas, ]


################################################################################



# Set working directory
setwd('D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/maps_graphs/')

# Generate the map
ggplot() +
  # Base Nigeria map
  geom_sf(data = nigeria_shp, fill = "gray", color = "black") +
  
  # Highlighted LGAs with different colors
  geom_sf(data = highlighted_shp, aes(fill = .data[[lga_column]]), color = "black", size = 1.2) +
  
  # Labels moved outside the map
  annotate("text", x = 9.15, y = 5.5, label = "Tureta\nSample: 305 (IDP: 155 ; Host:150)", 
           size = 4, color = "black", fontface = "bold", hjust = 0) +
  annotate("text", x = 9.15, y = 4.45, label = "Tangaza\nSample: 303 (IDP: 148 ; Host: 155)", 
           size = 4, color = "black", fontface = "bold", hjust = 0) +
  
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  ggtitle("Nigeria - Tureta & Tangaza (Sample Size)") +
  
  # Remove background lines
  theme_void() +  # Completely removes all background elements
  theme(
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    axis.text = element_blank(),  # Removes axis text
    axis.ticks = element_blank(),  # Removes axis ticks
    legend.title = element_blank()
  )

# Save the map as a JPG
ggsave("Nigeria_Tureta_Tangaza.jpg", width = 10, height = 6, dpi = 300)

################################

# Set working directory
setwd('D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/maps_graphs/')

# Generate the map
ggplot() +
  # Base Nigeria map
  geom_sf(data = nigeria_shp, fill = "gray", color = "black") +
  
  # Highlighted LGAs with different colors
  geom_sf(data = highlighted_shp, aes(fill = .data[[lga_column]]), color = "black", size = 1.2) +
  
  # Labels moved outside the map
  annotate("text", x = 9.15, y = 5.5, label = "Tureta\nSample: 305 (IDP: 155 ; Host:150)", 
           size = 4, color = "black", fontface = "bold", hjust = 0) +
  annotate("text", x = 9.15, y = 4.45, label = "Tangaza\nSample: 303 (IDP: 148 ; Host: 155)", 
           size = 4, color = "black", fontface = "bold", hjust = 0) +
  
  # Define fill colors for LGAs
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  ggtitle("Nigeria - Tureta & Tangaza (Sample Size)") +
  
  # Remove background lines and change title color
  theme_void() +  
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold"),  # Change title color
    legend.title = element_blank()
  )

# Save the map as a PNG
ggsave("Nigeria_Tureta_Tangaza.png", width = 10, height = 6, dpi = 300, bg = "white")


################################################################################

# Map with google maps like view

# Load Nigeria shapefile
nigeria_shp <- st_read("C:/Users/luisf/Downloads/gadm41_NGA_2.shp")


print(colnames(nigeria_shp))  # Identify the correct column for LGAs
lga_column <- "NAME_2"  

# Filter for Tureta & Tangaza
highlighted_shp <- nigeria_shp %>%
  filter(get(lga_column) %in% c("Tureta", "Tangaza"))

highlighted_centroids <- st_centroid(highlighted_shp)

coords <- st_coordinates(highlighted_centroids)

highlighted_centroids <- highlighted_centroids %>%
  mutate(
    X = coords[, 1] + 0.2,  # Move right (+) or left (-)
    Y = coords[, 2] + 0.2   # Move up (+) or down (-)
  )

# Assign Colors to Tureta & Tangaza
highlighted_shp <- highlighted_shp %>%
  mutate(color = ifelse(get(lga_column) == "Tureta", "darkred", "darkgreen"))

leaflet() %>%
  addTiles() %>%  
  
  # Highlight Tureta (blue) and Tangaza (red)
  addPolygons(
    data = highlighted_shp, color = "black", 
    fillColor = ~color, weight = 2, opacity = 1, fillOpacity = 0.7, group = "LGAs"
  ) %>%
  
    addLabelOnlyMarkers(
    lng = highlighted_centroids$X, lat = highlighted_centroids$Y,  # Adjusted positions
    label = highlighted_centroids[[lga_column]],  
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE, 
      style = list("color" = "black", "font-weight" = "bold", "font-size" = "14px")
    )
  )

# this map need to be saved manually. Exported using the Plot window on the rigth side of the RStudio screen.


################################################################################

# another option for the maps of Nigeria


install.packages("leaflet")

library(leaflet)

leaflet() %>%
  addTiles() %>%  
  addPolygons(data = nigeria_shp, color = "black", fillColor = "yellow", weight = 2)



################################################################################

# map where is located Nigeria


install.packages(c("sf", "leaflet", "rnaturalearth", "rnaturalearthdata"))

library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Africa shapefile
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Check available column names
print(colnames(africa))

# Filter Nigeria from the Africa dataset
nigeria <- africa[africa$admin == "Nigeria", ]  # Ensure correct column name

# Leaflet Map: Highlighting Nigeria
leaflet() %>%
  addTiles() %>%  # Base map
  
  # Africa map (light grey)
  addPolygons(data = africa, color = "black", fillColor = "lightgrey", weight = 1) %>%
  
  # Highlight Nigeria (Red)
  addPolygons(data = nigeria, color = "black", fillColor = "red", weight = 2) %>%
  
  # Add a label for Nigeria
  addLabelOnlyMarkers(
    lng = st_coordinates(st_centroid(nigeria))[1], 
    lat = st_coordinates(st_centroid(nigeria))[2], 
    label = "",
    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
  )


# this map need to be saved manually. Exported using the Plot window on the rigth side of the RStudio screen.

################################################################################
# map of africa with mini map

leaflet() %>%
  addTiles() %>%  # Adds a Google Maps-like tile layer
  addPolygons(data = africa, color = "black", fillColor = "lightgray", weight = 1) %>%
  addMiniMap(toggleDisplay = TRUE)  # Adds a small mini-map for navigation


