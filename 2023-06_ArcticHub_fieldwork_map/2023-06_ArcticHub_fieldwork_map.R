# Map for fieldwork activities across the ArcticHub network during summer 2023
# code by Jonathan von Oppen, vonoppenj@gmail.com


# 0) Dependencies ----
pacman::p_load(readxl,
               tidyverse,
               sf,
               rnaturalearth,
               ggthemes)

setwd("2023-06_ArcticHub_fieldwork_map")

# 1) Prepare data ----

# >> mapping settings ----

# define centre zoom point
zoom_to <- c(-52, 78)

# define target CRS
target_crs <- "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=-52 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs"

# define zoom level and ranges
zoom_level <- 2.1

C <- 40075016.686   # ~ circumference of Earth in meters
x_span <- C / 2^zoom_level
y_span <- C / 2^(zoom_level)

# convert central point to target CRS
zoom_to_xy <- st_sfc(st_point(zoom_to), crs = 4326) %>% 
  st_transform(crs = target_crs)

# calculate lon and lat ranges for display window by subtracting/adding the half of the above ranges from/to the zoom center coordinates respectively.
disp_window <- st_sfc(
  st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
  st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
  crs = target_crs)


# >> fieldwork data ----
fieldwork_plans <- readxl::read_xlsx("ArcticHub_2023_fieldwork_map_data.xlsx") %>%
  
  # drop unused cols & rows
  select(-contact) %>%
  filter(!name == "Laura")

# place names & coordinates
coords <- read_delim(file.path("..", "data", "geonames", "cities1000.txt"), 
                     col_names = c("geonameid", "name", "asciiname", "alternatenames", 
                                   "latitude", "longitude",  # WGS84
                                   "feature class", "feature code", 
                                   "country code", "cc2", 
                                   "admin1 code", "admin2 code", "admin3 code", "admin4 code",      # codes for 1st/2nd/3rd/4th administrative division
                                   "population", 
                                   "elevation", "dem",  # srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer.
                                   "timezone",        # iana timezone id
                                   "modification date")) %>%
  # select relevant columns
  select(name,
         lat = latitude,
         lon = longitude) %>%
  # filter for fieldwork places with coordinates available
  filter(name %in% fieldwork_plans$place)

# add to fieldwork plans df
fieldwork_map_data <- fieldwork_plans %>%
  left_join(coords, by = c("place" = "name"))

# get coordinates for other places
coords_add <- fieldwork_map_data %>%
  # filter for places without coords
  filter(is.na(lat)) %>%
  select(-c(lat, lon)) %>%
  left_join(bind_rows(
    # Abisko
    c(place = "Abisko", lat = 68.319714, lon = 18.733436),
    # Qeqertarsuaq
    c(place = "Qeqertarsuaq", lat = 69.262834, lon = -53.475332),
    # Latnjajaure
    c(place = "Latnjajaure", lat = 68.358237, lon = 18.495037),
    # Tväråklumparna
    c(place = "Tväråklumparna", lat = 63.199396, lon = 12.332765),
    # Kangerlussuaq
    c(place = "Kangerlussuaq", lat = 67.030663, lon = -50.722457),
    # Qikiqtaruk
    c(place = "Qikiqtaruk", lat = 69.571417, lon = -138.901712),
    # Kluane
    c(place = "Kluane", lat = 61.251591, lon = -138.711384),
    # Kaffeklubben Island
    c(place = "Kaffeklubben Island", lat = 83.629436, lon = -31.199370),
    # Ymer Island / Ella Island
    c(place = "Ymer Island / Ella Island", lat = 72.979719, lon = -24.861646)),
    by = "place") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

# merge data frames & make spatial
fieldwork_map_data <- fieldwork_map_data %>% 
  # filter for places with coordinates
  filter(!is.na(lat)) %>%
  # bind with additional places
  bind_rows(coords_add) %>%
  
  # make sf object
  sf::st_as_sf(coords = c("lat", "lon"),
               crs = "epsg:4326") %>%
  
  # transform coordinates to polar stereographic
  st_transform(target_crs)


# >> map data ----
background_map <- rnaturalearth::ne_countries(scale = "large",
                                              returnclass = "sf") %>% 
  #retain only name and geometry
  dplyr::select(continent,
                name,
                geometry) %>% 
  # filter for relevant countries
  filter(continent %in% c("Asia", "Europe", "North America")) %>%
  
  # transform coordinates to polar stereographic, https://epsg.io/5938
  st_transform(target_crs)


# 2) Make map ----
ggplot() +
  geom_sf(data = background_map) +
  geom_sf(data = fieldwork_map_data,
          aes(colour = lab,
              fill = name),
          pch = 21,
          size = 4) +
  # no legend
  guides(fill = "none") +
  # set extent
  coord_sf(xlim = st_coordinates(disp_window)[,'X'],
           ylim = st_coordinates(disp_window)[,'Y'],
           crs = target_crs) +
  # plain map theme with decent grid
  theme_map() +
  theme(panel.grid.major = element_line(colour = "grey90"))

  # limit coordinates: check BES datavis workshop
  # make interactive: ggplotly