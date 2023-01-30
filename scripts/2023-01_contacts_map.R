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
  c("Achim von Oppen & Christine Wedhorn", "Familie", "Berlin"),
  c("Alex Dombrowski & Mela Padilla", "Schule, Stuttgart", "Stuttgart"),
  c("Alina Brand & Julian Wortmann", "Wohnprojekt", "Leipzig"),
  c("Angela Prendin", "Phd, Aarhus", "Padova"),
  c("Angelika & Eberhard von Oppen", "Familie", "Stuttgart"),
  c("Anne Bjorkman", "PhD", "Göteborg"),
  c("Benjamin Richter", "Familie", "Marburg"),
  c("Caroline Saam", "Ultimate", "Köln"),
  c("Dagmar Egelkraut", "PhD", "Bergen"),
  c("Dominik Jockers", "Ultimate", "Heidelberg"),
  c("Elena Pearce & Charlie Davison", "PhD, Aarhus", "Aarhus"),
  c("Elin Lindén", "Auslandssemester, Abisko", "Umeå"),
  c("Elisabeth Seitz", "Familie", "Köln"),
  c("Ellen & Chris Sommer", "Ultimate, Tübingen", "Bonn"),
  c("Elli, Paul & Lukas Fehling", "Ultimate, Tübingen", "Tübingen"),
  c("Flo Krüger", "Ultimate", "Marburg"),
  c("Giulia Mazzotti", "Master, Davos", "Grenoble"),
  c("Hanna Frick", "Studium, Tübingen", "Zürich"),
  c("Heidi & Hartmut Richter", "Familie", "Neu-Anspach"),
  c("Ida Dumon", "Ultimate", "Brandenburg"),
  c("Jakob Assmann & Nina Moffat", "PhD", "Zürich"),
  c("Jakob de Maeyer", "Ultimate", "Münster"),
  c("Jakob Dürrwächter", "Schule, Stuttgart", "Berlin"),
  c("Johannes Domeier", "Auslandssemester, Umeå", "Malmö"),
  c("Juli Ballmann", "Ultimate", "Berlin"),
  c("Kathrin Bross & Moritz Koch", "Studium, Tübingen", "Heidelberg"),
  c("Katja Weigl", "Ultimate", "Freiburg"),
  c("Keno Franke & Theresa Purschke", "Ultimate", "Berlin"),
  c("Linnéa Weitkamp", "WG, Tübingen", "Leipzig"),
  c("Mariana García Criado", "PhD", "Edinburgh"),
  c("Max Schön & Lisa Emerich", "WG, Tübingen", "Heidelberg"),
  c("Ollie Baines", "PhD, Aarhus", "Aarhus"),
  c("Patrick Möhl", "Master, Davos", "Basel"),
  c("Ragnhild Gya & Lars-Olav Hammer", "PhD", "Bergen"),
  c("Ronja Wedegärtner", "Studium, Tübingen", "Oslo"),
  c("Rosa Witty", "Studium, Tübingen", "Tübingen"),
  c("Rosel von Oppen", "Familie", "Marburg"),
  c("Sara ten Brinke & Jonas Bothe", "Wohnprojekt", "Alfter"),
  c("Signe Lemcke", "WG, Aarhus", "Aarhus"),
  c("Sonja Lorenz", "Musik, Tübingen", "München"),
  c("Sonja Wipf & Christian Rixen", "Master, Davos", "Davos"),
  c("Sophie Monsarrat & Adam Custock", "PhD", "Nijmegen"),
  c("Tofu (Markus) Starr", "Ultimate", "Hamburg"),
  c("Ulrich Schaffert", "Familie", "Frankfurt"),
  c("Vera Blaschke & Flo Schunck", "Ultimate", "Leipzig"),
  c("Vera Middendorf & Behrend Dellwisch", "WG, Tübingen", "Münster")
) |> 
  
  # transform to tibble
  as_tibble(.name_repair = "unique") |> 
  
  # name columns
  rename(name = 1,
         connection = 2,
         town = 3)

addresses_locations <- addresses |> 
  
  # get coordinates through Geonames
  left_join(addresses |> 
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
