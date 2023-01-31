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
               polite,     # for polite web scraping
               rvest       # for web scraping functions
)

# Set colours and plot visuals

# define colours

# File paths
if(!dir.exists(file.path("data", "geonames"))) dir.create(file.path("data", "geonames"))

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


# Address book
addresses <- rbind(
  c("Achim von Oppen & Christine Wedhorn", "Familie", "Berlin, DE"),
  c("Alex Dombrowski & Mela Padilla", "Schule, Stuttgart", "Stuttgart, DE"),
  c("Alina Brand & Julian Wortmann", "Wohnprojekt", "Leipzig, DE"),
  c("Angela Prendin", "Phd, Aarhus", "Padova, IT"),
  c("Angelika & Eberhard von Oppen", "Familie", "Stuttgart, DE"),
  c("Anne Bjorkman", "PhD", "Göteborg, SE"),
  c("Benjamin Richter", "Familie", "Marburg, DE"),
  c("Caroline Saam", "Ultimate", "Köln, DE"),
  c("Dagmar Egelkraut", "PhD", "Bergen, NO"),
  c("Dominik Jockers", "Ultimate", "Heidelberg, DE"),
  c("Elena Pearce & Charlie Davison", "PhD, Aarhus", "Aarhus, DK"),
  c("Elin Lindén", "Auslandssemester, Abisko", "Umeå, SE"),
  c("Elisabeth Seitz", "Familie", "Köln, DE"),
  c("Ellen & Chris Sommer", "Ultimate, Tübingen", "Bonn, DE"),
  c("Elli, Paul & Lukas Fehling", "Ultimate, Tübingen", "Tübingen, DE"),
  c("Emily Pickering Pedersen", "Auslandssemester, Abisko", "Abisko, SE"),
  c("Flo Krüger", "Ultimate", "Marburg, DE"),
  c("Giulia Mazzotti", "Master, Davos", "Grenoble, FR"),
  c("Hanna Frick", "Studium, Tübingen", "Zürich, CH"),
  c("Heidi & Hartmut Richter", "Familie", "Neu-Anspach, DE"),
  c("Ida Dumon", "Ultimate", "Brandenburg, DE"),
  c("Jakob Assmann & Nina Moffat", "PhD", "Zürich, DE"),
  c("Jakob de Maeyer", "Ultimate", "Münster, DE"),
  c("Jakob Dürrwächter", "Schule, Stuttgart", "Berlin, DE"),
  c("Johannes Domeier", "Auslandssemester, Umeå", "Malmö, SE"),
  c("Juli Ballmann", "Ultimate", "Berlin, DE"),
  c("Kathrin Bross & Moritz Koch", "Studium, Tübingen", "Heidelberg, DE"),
  c("Katja Weigl", "Ultimate", "Freiburg, DE"),
  c("Keno Franke & Theresa Purschke", "Ultimate", "Berlin, DE"),
  c("Linnéa Weitkamp", "WG, Tübingen", "Leipzig, DE"),
  c("Lisa Andresen", "Auslandssemester, Abisko", "Greifswald"),
  c("Mariana García Criado", "PhD", "Edinburgh, UK"),
  c("Max Schön & Lisa Emerich", "WG, Tübingen", "Heidelberg, DE"),
  c("Muriel Ehrbar & Nick Solenthaler", "Master, Riederfurka", "Winterthur, CH"),
  c("Ollie Baines", "PhD, Aarhus", "Aarhus, DK"),
  c("Patrick Möhl", "Master, Davos", "Basel, CH"),
  c("Ragnhild Gya & Lars-Olav Hammer", "PhD", "Bergen, NO"),
  c("Ronja Wedegärtner", "Studium, Tübingen", "Oslo, NO"),
  c("Rosa Witty", "Studium, Tübingen", "Tübingen, DE"),
  c("Rosel von Oppen", "Familie", "Marburg, DE"),
  c("Sara ten Brinke & Jonas Bothe", "Wohnprojekt", "Alfter, DE"),
  c("Signe Lemcke", "WG, Aarhus", "Aarhus, DK"),
  c("Sonja Lorenz", "Musik, Tübingen", "München, DE"),
  c("Sonja Wipf & Christian Rixen", "Master, Davos", "Davos, CH"),
  c("Sophie Monsarrat & Adam Custock", "PhD", "Nijmegen, NL"),
  c("Tofu (Markus) Starr", "Ultimate", "Hamburg, DE"),
  c("Ulrich Schaffert", "Familie", "Frankfurt, DE"),
  c("Vera Blaschke & Flo Schunck", "Ultimate", "Leipzig, DE"),
  c("Vera Middendorf & Behrend Dellwisch", "WG, Tübingen", "Münster, DE")
) |> 
  
  # transform to tibble
  as_tibble(.name_repair = "unique") |> 
  
  # name columns
  rename(name = 1,
         connection = 2,
         town = 3)

addresses_locations <- download.file("http://download.geonames.org/export/dump/cities1000.zip") |> 
  # unzip into data
  zip::unzip(exdir = "data/geonames") #|> 
  
  # get coordinates through Geonames
  right_join(addresses |> 
              pull(town) |> 
              GNsearch() |> 
              bind_rows(),
            by = c("town" = "city"))


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
   # # add labels
   # geom_sf_label(data = maple_loc,
   #               aes(label = loc,
   #                   colour = loc),
   #               nudge_x = 180000,
   #               nudge_y = -120000,
   #               size = 3) +
   # # adjust colour schemes for points and labels
   # scale_colour_manual(values = c("darkgreen", "steelblue3", "firebrick4", "goldenrod")) +
   # scale_fill_manual(values = c("darkgreen", "steelblue3", "firebrick4", "goldenrod")) +
   # use empty map theme
   theme_map() +
   # remove legends
   theme(legend.position = "none")
)
