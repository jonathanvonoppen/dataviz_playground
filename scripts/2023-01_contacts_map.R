#' Darts scores from the 2022/2023 PDC World Championships
#' 
#' code by Jonathan von Oppen  |  https://www.github.com/jonathanvonoppen/
#' Dataviz playground project


#' TO DO ----
#' 
#' 


# Dependencies ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(sf,
               rnaturalearth,
               terra,
               geonames,
               tidyverse,  # for data wrangling and visualisation
               ggnewscale, # for multiple colour scales in ggplots
               ggthemes,   # for extra themes
               MetBrewer,  # for nice colour palettes
               polite,     # for polite web scraping
               rvest       # for web scraping functions
)

# Set colours and plot visuals

# define colours

# File paths
geonames_path <- file.path("data", "geonames")
if(!dir.exists(geonames_path)) dir.create(geonames_path)

# country outlines from Eurostat
europe_map_df <- read_sf("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035_LEVL_1.geojson") |> 
  # filter out overseas territories & Cyprus & Turkey
  filter(!NUTS_ID %in% c("FRY", "PT2", "PT3", "ES7") & CNTR_CODE != "CY" & CNTR_CODE != "TR") |> 
  # aggregate at country level
  count(CNTR_CODE) |> 
  # # transform to elevation data CRS
  # st_transform(map_crs) |> 
  # crop to area below 75°N
  st_crop(y = c(xmin = 2635973, xmax = 6084621, ymin = 1386017, ymax = 5700000))


# elevation data
europe_elev <- elevatr::get_elev_raster(locations = europe_map_df, 
                                        z = 3, # zoom level
                                        clip = "locations"
) |> 
  # transform to data frame
  as.data.frame(xy = TRUE) |> 
  
  rename("elev" = 3)


# Address book ----
addresses <- rbind(
  c("Achim von Oppen & Christine Wedhorn", "Familie", "Berlin, DE"),
  c("Alex Dombrowski & Mela Padilla", "Schule, Stuttgart", "Stuttgart, DE"),
  c("Alina Brand & Julian Wortmann", "Wohnprojekt", "Leipzig, DE"),
  c("Angela Prendin", "Phd, Aarhus", "Padova, IT"),
  c("Angelika & Eberhard von Oppen", "Familie", "Stuttgart, DE"),
  c("Anne Bjorkman", "PhD", "Göteborg, SE"),
  c("Benjamin Richter", "Familie", "Marburg an der Lahn, DE"),
  c("Caroline Saam", "Ultimate", "Köln, DE"),
  c("Chiara Esposito", "WG, Aarhus", "Århus, DK"),
  c("Dagmar Egelkraut", "PhD", "Bergen, NO"),
  c("Dominik Jockers", "Ultimate", "Heidelberg, DE"),
  c("Elena Pearce & Charlie Davison", "PhD, Aarhus", "Århus, DK"),
  c("Elin Lindén", "Auslandssemester, Abisko", "Umeå, SE"),
  c("Elisabeth Seitz", "Familie", "Köln, DE"),
  c("Ellen & Chris Sommer", "Ultimate, Tübingen", "Bonn, DE"),
  c("Elli, Paul & Lukas Fehling", "Ultimate, Tübingen", "Tübingen, DE"),
  c("Emily Pickering Pedersen", "Auslandssemester, Abisko", "Abisko, SE"),
  c("Flo Krüger", "Ultimate", "Marburg an der Lahn, DE"),
  c("Giulia Mazzotti", "Master, Davos", "Grenoble, FR"),
  c("Hanna Frick", "Studium, Tübingen", "Zürich, CH"),
  c("Heidi & Hartmut Richter", "Familie", "Neu-Anspach, DE"),
  c("Ida Dumon", "Ultimate", "Brandenburg an der Havel, DE"),
  c("Jakob Assmann & Nina Moffat", "PhD", "Zürich, CH"),
  c("Jakob de Maeyer", "Ultimate", "Münster, DE"),
  c("Jakob Dürrwächter", "Schule, Stuttgart", "Berlin, DE"),
  c("Johannes Domeier", "Auslandssemester, Umeå", "Malmö, SE"),
  c("Jule Tesch & Flo Auferoth", "Ultimate", "Berlin, DE"),
  c("Juli Ballmann", "Ultimate", "Berlin, DE"),
  c("Kathrin Bross & Moritz Koch", "Studium, Tübingen", "Heidelberg, DE"),
  c("Katja Weigl", "Ultimate", "Freiburg, DE"),
  c("Keno Franke & Theresa Purschke", "Ultimate", "Berlin, DE"),
  c("Linnéa Weitkamp", "WG, Tübingen", "Leipzig, DE"),
  c("Lisa Andresen", "Auslandssemester, Abisko", "Stralsund, DE"),
  c("Liz Hell", "Ultimate", "Konstanz, DE"),
  c("Mariana García Criado", "PhD", "Edinburgh, GB"),
  c("Max Schön & Lisa Emerich", "WG, Tübingen", "Heidelberg, DE"),
  c("Muriel Ehrbar & Nick Solenthaler", "Master, Riederfurka", "Winterthur, CH"),
  c("Nacho Montero Requena", "WG, Aarhus", "Århus, DK"),
  c("Nina Fahs", "PhD, Aarhus", "Brno, CZ"),
  c("Ollie Baines", "PhD, Aarhus", "Århus, DK"),
  c("Patrick Möhl", "Master, Davos", "Basel, CH"),
  c("Ragnhild Gya & Lars-Olav Hammer", "PhD", "Bergen, NO"),
  c("Ronja Wedegärtner & Eivind Bering", "Studium, Tübingen", "Oslo, NO"),
  c("Rosa Witty", "Studium, Tübingen", "Tübingen, DE"),
  c("Rosel von Oppen", "Familie", "Marburg an der Lahn, DE"),
  c("Sara ten Brinke & Jonas Bothe", "Wohnprojekt", "Alfter, DE"),
  c("Signe Lemcke", "WG, Aarhus", "Århus, DK"),
  c("Sonja Lorenz", "Musik, Tübingen", "Munich, DE"),
  c("Sonja Wipf & Christian Rixen", "Master, Davos", "Davos, CH"),
  c("Sophie Monsarrat & Adam Custock", "PhD", "Nijmegen, NL"),
  c("Tofu (Markus) Starr", "Ultimate", "Hamburg, DE"),
  c("Ulrich Schaffert", "Familie", "Frankfurt am Main, DE"),
  c("Vera Blaschke & Flo Schunck", "Ultimate", "Leipzig, DE"),
  c("Vera Middendorf & Behrend Dellwisch", "WG, Tübingen", "Münster, DE")
) |> 
  
  # transform to tibble
  as_tibble(.name_repair = "unique") |> 
  
  # name columns
  rename(name = 1,
         connection = 2,
         place = 3) |> 
  
  # separate place and country
  separate(place,
           sep = ", ",
           into = c("place", "country"))


# load city location data
if(!file.exists(file.path(geonames_path, "cities1000.txt"))) {
  download.file("http://download.geonames.org/export/dump/cities1000.zip", 
                destfile = geonames_path)
  zip::unzip(exdir = "data/geonames")
  }

# join locations to places
addresses_locations <- read_delim(file = file.path("data", "geonames", "cities1000.txt"), 
                                  col_names = c("geonameid", "name", "asciiname", "alternatenames", 
                                                "latitude", "longitude",  # WGS84
                                                "feature class", "feature code", 
                                                "country code", "cc2", 
                                                "admin1 code", "admin2 code", "admin3 code", "admin4 code",      # codes for 1st/2nd/3rd/4th administrative division
                                                "population", 
                                                "elevation", "dem",  # srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer.
                                                "timezone",        # iana timezone id
                                                "modification date")) %>% 
  
  # get coordinates through Geonames
  left_join(x = addresses,
            y = .,
            by = c("place" = "name", "country" = "country code")) |> 
  
  # fix multiple matches for "Münster"
  filter(!(place == "Münster" & population < 1e5)) |> 
  
  # fill in lat and lon manually for Abisko 
  mutate(latitude = case_when(place == "Abisko" ~ 68.349347,
                              TRUE ~ latitude),
         longitude = case_when(place == "Abisko" ~ 18.831289,
                               TRUE ~ longitude)) |> 
  
  # select relevant columns
  select(name, connection, 
         place, country, 
         latitude, longitude) |> 
  
  # split connection column
  separate(connection,
           sep = ", ",
           into = c("connection_background", "connection_place"),
           extra = "merge", 
           fill = "right") |> 
  
  # make numeric country col for making colour palette work
  mutate(country_num = factor(country),
         country_num = as.numeric(fct_shuffle(country_num))) |> 
  
  # transform to sf object
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)


# Plot map ----
(friends_map <- ggplot() +
   # # add elevation raster
   # geom_tile(data = europe_elev,
   #           aes(x = x, y = y,
   #               fill = elev),
   #           show.legend = NA) +
   # # adjust colour scheme
   # scale_fill_viridis_c(option = "mako") +
   # ggnewscale::new_scale_fill() +
   # add locations
   geom_sf(data = europe_map_df,
           # aes(fill = loc),
           fill = "darkgrey",
           colour = "grey80",
           pch = 22,
           size = 3) +
   # add points
   geom_sf(data = addresses_locations,
           aes(fill = country_num),
           pch = 21,
           size = 4,
           colour = "grey30") +
   # # add labels
   # geom_sf_label(data = maple_loc,
   #               aes(label = loc,
   #                   colour = loc),
   #               nudge_x = 180000,
   #               nudge_y = -120000,
   #               size = 3) +
   # adjust colour schemes for points and labels
   scale_fill_met_c("Austria") +
   # scale_fill_manual(values = c("darkgreen", "steelblue3", "firebrick4", "goldenrod")) +
   # use empty map theme
   theme_map() +
   # remove legends
   theme(legend.position = "none")
)
