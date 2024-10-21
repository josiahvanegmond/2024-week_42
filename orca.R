### TIDY TUESDAY WEEK 42 ###
# Josiah Van Egmond #


### LOAD LIBRARIES ###==========================================================
library(tidytuesdayR)
library(tmap)
library(tidyverse)
library(sf)
library(terra) #raster
library(marmap) #bathymetry data
library(geodata) #detailed cost data
library(patchwork) #Placing insets
library(ggimage) #placing image


##### LOAD AND CLEAN DATA ##### ================================================

tt_data <- tidytuesdayR::tt_load(2024, week=42) #load in tidytuesday data

orca_original <- tt_data$orcas #extract

orca <- orca_original %>% 
  drop_na(c(begin_latitude, begin_longitude)) %>% 
  select(year,
         date,
         pods_or_ecotype,
         latitude = begin_latitude, 
         longitude = begin_longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% #as simple features object
  mutate(pod = ifelse(grepl("Bigg|T", pods_or_ecotype, ignore.case = TRUE), "transient", "resident")) %>% 
  select(-pods_or_ecotype)


#load bathymetry data from NOAA

lat_range <- c(47.1176, 51.0367)  # Southern and northern latitude limits
lon_range <- c(-129.1448, -121.9098)  # Western and eastern longitude limits

# Get bathymetric data
bathy_data <- getNOAA.bathy(lon_range[1], lon_range[2], lat_range[1], lat_range[2], resolution = .1)

bath <- marmap::as.raster(bathy_data) #convert to raster

bath[bath > 0] <- 0 #dem data above 0 elevation set to 0

log_bath <- -log(abs(bath) + 1) #log to adjust color ramp

log_bath_df <- terra::as.data.frame(log_bath, xy = TRUE) #make dataframe


### CREATE BOX HIGHLIGHT

bbox_coords_df <- data.frame(
  lon = c(-123.508301, -123.508301, -122.873840, -122.873840, -123.508301),
  lat = c(48.390570, 48.788650, 48.788650, 48.390570, 48.390570)
)

# Create the bounding box as an sf object
bbox_sf <- st_as_sf(bbox_coords_df, coords = c("lon", "lat"), crs = 4326) %>%
  st_combine() %>%
  st_cast("POLYGON")

##### EXTENT FOR MAP BASED ON RASTER #####

raster_extent <- ext(log_bath)  # Get the extent of the raster

# Extract extent boundaries
xmin <- raster_extent[1]
xmax <- raster_extent[2]
ymin <- raster_extent[3]
ymax <- raster_extent[4]

##### GADM LAND DATA #####

land <- gadm(country = c("Canada", "United States of America"), level = 0, path = "C:/Users/josia/Documents/TidyTuesday", resolution = 1)

land <- land %>% st_as_sf()

land <- land %>% st_crop(xmin = -129.1, ymin = 47, xmax = -121.9, ymax = 51.1, crs= st_crs(land))


### MAIN MAP ### ===============================================================

map <- ggplot() +
  # Add bathymetry raster
  geom_raster(data = log_bath_df, aes(x = x, y = y, fill = layer), interpolate = TRUE) + #bathymetry data
  scale_fill_gradient(name = "Bathymetry", low = "#00072D", high = "#ADE1FB") +  #custom color palette for bathymetry
  geom_sf(data = orca, size = .5, alpha = 0.4, color = "#FFFFE0") + #orca points
  geom_sf(data = land, fill = "#04041c", color = NA) +  #land polygon
  geom_sf(data = bbox_sf, color = "white", fill = NA, lwd = .5) +  #inset map highlight
  coord_sf(xlim = c(xmin+.5, xmax-.5), ylim = c(ymin+.5, ymax-.3)) + #coord limmits
  geom_image(aes(image = "https://images.vexels.com/media/users/3/232110/isolated/preview/1c4be58c031ebc2919a280ea488c317d-filled-stroke-killer-whale.png",
                 x = xmin + 1.1, y = ymin + 2.4),size = .3) +
  theme_void() + #remove plot gridlines, legend
  theme(legend.position = "none",
        plot.title = element_text(family = "mono", face = "bold", size = 22, color = "#ADE1FB",
                                  hjust = 0.5, vjust = 4.5),
        plot.subtitle = element_text(family = "mono", margin = margin(5, 0, 10, 0), colour = "#ADE1FB",
                                     hjust = 0.5, vjust = 8, size = 10),
        plot.background = element_rect(fill = "#04041c"),
        plot.margin = margin(r = 30, l = 30, t = 20, b = 10),
        panel.border = element_blank()) + 
  labs(
    title = "Orca sightings in the Salish Sea",
    subtitle = "This map shows observations of southern resident orcas \n and transient individuals in the Salish Sea from 2017-2024
                        \n Source: Center for Whale Research"
  )

map


### SEASONAL CHART ### =========================================================

orca$month <- format(orca$date, "%m")
orca$year_month <- format(orca$date, "%Y-%m")

orca_count <- orca %>% group_by(month) %>% 
  summarise(monthly = n()) %>% 
  drop_na()

orca_count$month <- factor(orca_count$month, 
                           levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

month_plot <- ggplot(data = orca_count, aes(x = month, y = monthly)) + 
  geom_col(aes(fill = monthly)) +
  coord_radial() + 
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "#ece7f2", size = 12, family = "mono"),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "#151836", color = "gray"),
    legend.position = "none",
    panel.grid = element_line(color = "#4c5078"),
    plot.title = element_text(hjust = 0.5, color = "#ece7f2", size = 10, family = "mono", face = "bold")) +
  scale_fill_gradientn(colors = c("#5f90ad", "#c4e9ff")) + 
  labs(title = "Seasonality of orca \n observations")

### MAP INSET ### ==============================================================

#higher quality land polygons
gadm_land <- gadm(country = c("Canada", "United States of America"), level = 0, path = "C:/Users/josia/Documents/TidyTuesday", resolution = 1)

#crop land polygons to bbox
gadm_sf <- gadm_land %>% st_as_sf() %>% st_crop(bbox_sf)

orca$year_month <- as.Date(paste0(orca$year_month, "-01"))

orca_inset <- orca %>% st_crop(bbox_sf)

map_inset <- ggplot() +
  geom_sf(data = bbox_sf, fill = "#0c4161") +
  geom_sf(data = orca, alpha = 0.5, color = "#FFFFE0")  + 
  geom_sf(data = gadm_sf, fill = "#04041c", color = NA) +  # No outline for land, black fill
  coord_sf(xlim = c(-122.8738, -123.5083), ylim = c(48.39057, 48.78865), expand = FALSE) +
  theme_void() + 
  theme(legend.position = "top",
        panel.border = element_rect(color = "gray90", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, color = "#ece7f2", size = 13, family = "mono", face = "bold"))+ 
  labs(title = "Observations in Haro Strait")


##### COMBINE MAP AND INSETS ##### =============================================

map + inset_element(map_inset, left = 0, right = .4, top = .4, bottom = 0.02, align_to = "full") +
  inset_element(month_plot, left = .7, bottom = .6, right = 1, top = .85, align_to = "full")

ggsave("orca_map.jpg", dpi = 150, units = "px", width = 1500, height = 1500, type = "cairo")


