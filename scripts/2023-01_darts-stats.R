#' Darts scores from the 2022/2023 PDC World Championships
#' 
#' code by Jonathan von Oppen  |  https://www.github.com/jonathanvonoppen/
#' Dataviz playground project


#' TO DO ----
#' 
#' make colour scheme
#'  > create tighter col gradient for players w/ < 2 9ers
#' 
#' close gap
#'  > adjust rect widths
#' 
#' adjust point sizes
#'  > evt look up points left for opponent
#' 
#' make labels
#'  > first 9er
#'  > most 9ers (Phil Taylor)
#'  > most 9ers in one year
#'  > latest 9ers
#'  
#' make explanatory text
#'  > list all players w/ colour


# Dependencies ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,  # for data wrangling and visualisation
               lubridate,  # for date/time functions
               ggnewscale, # for multiple colour scales in ggplots
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
empty_space_radius_outer <- 1

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
  # make new year_rank column & numeric version for plotting
         year_rank = paste(Year, rank, sep = "_"),
         year_rank = factor(year_rank),
         year_rank_num = as.numeric(year_rank),
  # order players by no. niners
         Player_name = ordered(Player_name,
                               levels = niners_tv |> 
                                          group_by(Player_name) |> 
                                          summarise(n_niners = n()) |> 
                                          arrange(desc(n_niners)) |> 
                                          pull(Player_name)),
  # add empty space radius to all numbers to leave the centre empty)
         n_niners = n_niners + empty_space_radius_outer)


# data for coloured rings
rings <- niners_count |>
  
  # pick Year (group value) & year_rank columns
  select(starts_with("year_rank")) |> 
  ungroup() |> 
  # assign colours
  mutate(fill_col = case_when(
            # red for even years
            Year %in% as.character(seq(2004, 2022, by = 2)) ~ red,
            # green for odd years
            Year %in% as.character(seq(2005, 2023, by = 2)) ~ green),
         width = case_when(
           Year == 2023 ~ 1.5,
           TRUE ~ 1)
         )
  

# Make plot ----

(niners_plot <- ggplot() +
    
  # darts board design: coloured segments
    # inner beige ring
    geom_rect(aes(xmin = 0, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = empty_space_radius_outer, ymax = empty_space_radius_outer + 1),
              fill = beige) +
    # inner red/green ring
    geom_rect(data = rings,
              aes(xmin = year_rank_num - 1/2, xmax = year_rank_num + 1/2,
                  ymin = empty_space_radius_outer + 1, ymax = empty_space_radius_outer + 2,
                  fill = fill_col)) +
    # outer beige ring
    geom_rect(aes(xmin = 0, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = empty_space_radius_outer + 2, ymax = empty_space_radius_outer + 3),
              fill = beige) +
    # outer red/green ring
    geom_rect(data = rings,
              aes(xmin = year_rank_num - 1/2, xmax = year_rank_num + 1/2,
                  ymin = empty_space_radius_outer + 3, ymax = empty_space_radius_outer + 4,
                  fill = fill_col)) + scale_fill_identity(guide = "none") +
   # set up new fill scale for points 
   ggnewscale::new_scale_fill() +
   
  # 9-darter lines
    geom_segment(data = niners_count,
                 aes(x = year_rank_num,
                     xend = year_rank_num,
                     y = empty_space_radius_outer, 
                     yend = n_niners,
                     colour = Player_name),
                 position = position_dodge(width = 0.5)) +
    # add end points
    geom_point(data = niners_count,
               aes(x = year_rank_num,
                   y = n_niners,
                   fill = Player_name,
                   size = n_niners - empty_space_radius_outer), # to get original numbers
               pch = 21,
               colour = "grey",
               show.legend = FALSE) +
   
    # set colour scheme
    scale_fill_discrete()
    
    # add two inner rectangles which will become the inner empty space
    geom_rect(aes(xmin = 0, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = empty_space_radius_inner, ymax = empty_space_radius_outer),
              fill = green) +
    geom_rect(aes(xmin = 0, xmax = length(unique(niners_count$year_rank)) + 1,
                  ymin = 0, ymax = empty_space_radius_inner),
              fill = red) +
    # set circular layout
    coord_polar() +
    # set theme
    theme_void())
