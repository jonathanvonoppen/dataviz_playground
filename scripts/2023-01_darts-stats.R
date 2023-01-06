#' Darts scores from the 2022/2023 PDC World Championships
#' 
#' code by Jonathan von Oppen  |  https://www.github.com/jonathanvonoppen/
#' Dataviz playground project


# Dependencies ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,  # for data wrangling and visualisation
               lubridate,  # for date/time functions
               polite,     # for polite web scraping
               rvest)      # for web scraping functions


# Set colours and plot visuals

# define segment colours
red <- "#D62213"
  green <- "#42862D"
    beige <- "#F5DFB1"
      black <- "#191B1D"

# define empty space radius
empty_space_radius_inner <- 0.6
empty_space_radius_outer <- 1.6

# Scrape & wrangle data ----

# set URL
scores_url <- "https://en.wikipedia.org/wiki/Nine-dart_finish"

# bow
url_bow <- polite::bow(scores_url)

# scrape
niners <- polite::scrape(bow = url_bow) |> 
  html_nodes("table.wikitable") |> 
  html_table()
  
# define today's date
today <- dmy("06-01-2023")

# grab table with 9-darters within last 20 years
niners_tv <- niners |> 
  # pluck first list (all televised 9-darters)
  pluck(1) |> 
  # convert date col to date format
  mutate(Date = dmy(Date)) |> 
  # filter for events within last 20 years
  filter(Date %within% interval(today - years(20), today)) |> 
  # make year column
  mutate(Year = year(Date),
  # clean player column (weird prefix for Johnny Clayton (2))
         Player = str_remove(Player, ".+(?=\\}\\s)"),
         Player = str_remove(Player, "^\\}\\s"),
  # make raw player column without count
         Player_name = str_remove(Player, "[:space:]+\\(\\d+\\)")) |> 
  # create factors
  mutate(across(c(Player, Player_name, Opponent, Tournament, Method, Referee, Year), as.factor))

niners_count <- niners_tv |> 
  # get count by player and year
  group_by(Player_name, Year) |> 
  summarise(n_niners = n(), .groups = "drop") |> 
  # sort by no. niners & alphabetic within year
  group_by(Year) |> 
  arrange(Year, desc(n_niners), Player_name) |> 
  # assign rank within Year for ordering bars
  mutate(rank = rank(desc(n_niners), ties.method = "first"),
  # make new year_rank column
         year_rank = paste(Year, rank, sep = "_"),
  
  # add empty space radius to all numbers to leave the centre empty)
         n_niners = n_niners + empty_space_radius_outer)

# Make plot

(niners_plot <- ggplot() +
    
    geom_segment(data = niners_count,
                 aes(x = year_rank,
                     xend = year_rank,
                     y = 0, 
                     yend = n_niners,
                     colour = Player_name),
                 position = position_dodge(width = 0.1)) +
    # add two inner rectangles which will become the inner empty space
    geom_rect(aes(xmin = 1, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = 0, ymax = empty_space_radius_outer),
              fill = green) +
    geom_rect(aes(xmin = 1, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = 0, ymax = empty_space_radius_inner),
              fill = red) +
    # set circular layout
    coord_polar() +
    # set theme
    theme_minimal())
