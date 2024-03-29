# Map for fieldwork activities across the ArcticHub network during summer 2023
# code by Jonathan von Oppen, vonoppenj@gmail.com


# 0) Dependencies ----
pacman::p_load(readxl,
               tidyverse,
               sf,
               rnaturalearth,
               ggthemes,
               ggforce,
               ggrepel,
               plotly,
               cowplot)

if(getwd() == "C:/Users/au630524/Documents/R/dataviz_playground")
  setwd("2023-06_ArcticHub_fieldwork_map")

# Colour schemes
palette_labs <- c("#e67e22", "#2e86c1", "#cb4335", "#27ae60", "#f1c40f", "#884ea0")
# palette_names <- 

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
  st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 4))),
  st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 4))),
  crs = target_crs)


# >> fieldwork data ----
fieldwork_plans <- readxl::read_xlsx("ArcticHub_2023_fieldwork_map_data.xlsx") %>%
  
  # drop unused cols & rows
  select(-contact) %>%
  filter(!is.na(place))

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
  mutate(
    # make coordinates numeric
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    # adjust Ymer/Ella label
    place = case_when(place == "Ymer Island / Ella Island" ~ "Ymer Island /\nElla Island",
                      TRUE ~ place))

# merge data frames & make spatial
fieldwork_map_data <- fieldwork_map_data %>% 
  # filter for places with coordinates
  filter(!is.na(lat)) %>%
  # bind with additional places
  bind_rows(coords_add) %>%
  
  # make sf object
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "epsg:4326") %>%
  
  # jitter points with multiple occurrences
    # make place_region var to account for close-by locations (Abisko/Latnja, Ilulissat/Qeqertarsuaq)
    mutate(place_region = case_when(place %in% c("Abisko", "Latnjajaure") ~ "Sapmi",
                                    place %in% c("Ilulissat", "Qeqertarsuaq") ~ "Disko Bay",
                                    TRUE ~ place)) %>%
  group_by(place_region) %>%
  group_split() %>%
  map_if(~ nrow(.x) > 1,
         ~ st_jitter(.x, amount = 3)) %>%
  bind_rows() %>%
  
  # make label column for interactive plots
  mutate(label = case_when(!is.na(date_range) ~ paste(name, place, date_range, method,
                                                      sep = " -- "),
                           is.na(method) ~ paste(name, place,
                                                 sep = " -- "),
                           TRUE ~ paste(name, place, method,
                                        sep = " -- "))) %>%
  
  # transform coordinates to polar stereographic
  st_transform(target_crs)

# make convex hull
map_hull_data <- fieldwork_map_data %>% st_buffer(2e5) %>%
  
  # Aggregate - which unions the points into MULTIPOINTS (default do_union=TRUE)
  # and uses FUN to assign the first value in other fields
    # source: https://gis.stackexchange.com/a/404071
  aggregate(.,
      by=list(lab=.$lab),
      FUN=function(vals){vals[1]}) %>% 
  st_sf()

# Cast to convex hull
st_geometry(map_hull_data) <- st_convex_hull(map_hull_data$geometry)


# >> map data ----
background_map <- rnaturalearth::ne_countries(scale = "large",
                                              returnclass = "sf") %>% 
  #retain only name and geometry
  dplyr::select(continent,
                name,
                geometry) %>% 
  # filter for relevant countries
  filter(continent %in% c("Asia", "Europe", "North America")) %>%
  
  # transform coordinates to Greenland polar stereographic, https://epsg.io/5938
  st_transform(target_crs)


# 2) Make map ----

# >> map ----
(fieldwork_map <- ggplot() +
   geom_sf(data = background_map,
           colour = "white",
           inherit.aes = FALSE) +
   geom_sf(data = fieldwork_map_data,
           aes(fill = label,
               colour = lab),
           pch = 21,
           size = 4,
           stroke = 2,
           alpha = .6) +
   # no legend
   guides(fill = "none",
          colour = "none") +
   # colour schemes
   scale_colour_manual(values = palette_labs) +
   scale_fill_manual(values = rep("grey30", length(fieldwork_map_data$name))) +
   # set extent
   coord_sf(xlim = st_coordinates(disp_window)[,'X'],
            ylim = st_coordinates(disp_window)[,'Y'],
            crs = target_crs) +
   # plain map theme with decent grid
   theme_map() +
   theme(panel.grid.major = element_line(colour = "grey90")))


# make dummy plot for legend
map_legend_labels <- fieldwork_map_data %>% arrange(lab) %>% distinct(lab) %>% pull(lab) %>% str_replace("\\/ ", "\\/\n")
map_legend <- get_legend(
  ggplot(fieldwork_map_data) +
    geom_sf(aes(colour = lab,
                fill = lab),
            pch = 21,
            alpha = 0.5) +
    scale_colour_manual(values = palette_labs,
                        labels = map_legend_labels,
                        guide = guide_legend(direction = "horizontal", 
                                             byrow = T,
                                             nrow = 2,
                                             override.aes = list(stroke = 2))) +
    scale_fill_manual(values = palette_labs,
                      labels = map_legend_labels,
                      guide = guide_legend(direction = "horizontal", 
                                           nrow = 2,
                                           override.aes = list(size = 5))) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.spacing.y = unit(3, "mm"),
          legend.text = element_text(face = "bold")))


# combine base map and legend
fieldwork_map_legend <- plot_grid(plotlist = list(fieldwork_map,
                                                  map_legend),
                                  ncol = 1,
                                  rel_heights = c(1, .18),
                                  rel_widths = c(1, 0.6))


# >> add place labels ----
fieldwork_map_place_labels <- fieldwork_map +
  
  # add labels
  ggrepel::geom_label_repel(data = fieldwork_map_data %>% distinct(place, .keep_all = T),
                            aes(label = place,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 2,
                            max.overlaps = 10)


# combine name map and legend
fieldwork_map_places_legend <- plot_grid(plotlist = list(fieldwork_map_place_labels,
                                                         map_legend),
                                         ncol = 1,
                                         rel_heights = c(1, .18),
                                         rel_widths = c(1, 0.6))


# >> add name labels ----
fieldwork_map_name_labels <- fieldwork_map +
  
  # add labels
  ggrepel::geom_label_repel(data = fieldwork_map_data %>% distinct(name, .keep_all = T),
                            aes(label = name,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 2,
                            max.overlaps = 10)


# combine name map and legend
fieldwork_map_names_legend <- plot_grid(plotlist = list(fieldwork_map_name_labels,
                                                         map_legend),
                                         ncol = 1,
                                         rel_heights = c(1, .18),
                                        rel_widths = c(1, 0.6))


# >> add lab hulls and labels ----
(fieldwork_map_lab_labels <- ggplot() +
   # add background map
   geom_sf(data = background_map,
           colour = "white",
           inherit.aes = FALSE) +
  # add hulls
  geom_sf(data = map_hull_data,
          aes(colour = lab,
              fill = lab),
          alpha = .4) +
   # add points
   geom_sf(data = fieldwork_map_data,
           aes(colour = lab),
           fill = "grey30",
           pch = 21,
           size = 4,
           stroke = 2,
           alpha = .6) +
   # add labels
   ggrepel::geom_label_repel(data = map_hull_data %>% distinct(lab, .keep_all = T),
                             aes(label = lab,
                                 geometry = geometry),
                             stat = "sf_coordinates",
                             min.segment.length = 2,
                             max.overlaps = 10) +
   # no legend
   guides(fill = "none",
          colour = "none") +
   # colour schemes
   scale_colour_manual(values = palette_labs) +
   scale_fill_manual(values = palette_labs) +
   # set extent
   coord_sf(xlim = st_coordinates(disp_window)[,'X'],
            ylim = st_coordinates(disp_window)[,'Y'],
            crs = target_crs) +
   # plain map theme with decent grid
   theme_map() +
   theme(panel.grid.major = element_line(colour = "grey90")))


# combine lab map and legend
fieldwork_map_labs_legend <- plot_grid(plotlist = list(fieldwork_map_lab_labels,
                                                       map_legend),
                                       ncol = 1,
                                       rel_heights = c(1, .18),
                                       rel_widths = c(1, 0.4))


# >> make interactive ----
fieldwork_map_plotly <- ggplotly(fieldwork_map,
                                 tooltip = c("name", "place", "date_range", "method")) # not working yet, neither displaying "label" col


#' ideas:
#' - make markdown with all maps displayed