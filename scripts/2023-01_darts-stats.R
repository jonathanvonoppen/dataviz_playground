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
#'  > two in one match & most 9ers overall (Phil Taylor)
#'  > two in one night & most 9ers in one year (Price)
#'  > latest 9ers: Smith v van Gerwen
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
        darkgrey <- "#686868"
          lightgrey <- "#c0c0c0"

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
df_niners_tv <- niners |> 
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

df_niners_plot <- df_niners_tv |> 
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
                               levels = df_niners_tv |> 
                                          group_by(Player_name) |> 
                                          summarise(n_niners = n()) |> 
                                          arrange(desc(n_niners)) |> 
                                          pull(Player_name)),
  # make player colour column
         player_col = case_when(n_niners > 1 & Player_name == "Phil Taylor" ~ "#57D0F4",
                                n_niners > 1 & Player_name == "Raymond van Barneveld" ~ "#E85E11",
                                n_niners > 1 & Player_name == "Adrian Lewis" ~ "#5541ce",
                                n_niners > 1 & Player_name == "Michael van Gerwen" ~ "#C2D411",
                                n_niners > 1 & Player_name == "Gerwyn Price" ~ "#2872c3",
                                Year == 2023 & Player_name == "Michael Smith" ~ "#D51E1A",
                                TRUE ~ lightgrey),
  # add empty space radius to all numbers to leave the centre empty)
         n_niners_plot = n_niners + empty_space_radius_outer)


# data for dots
df_niners_dots <- df_niners_tv |> 
  # make opponent score column
  mutate(opponent_avg_score = case_when(Date == "2007-05-08" & Player_name == "Phil Taylor" ~ mean(100, 85),
                                        Date == "2007-06-09" & Player_name == "Phil Taylor" ~ mean(61, 96 + 60),
                                        Date == "2009-01-02" & Player_name == "Raymond van Barneveld" ~ mean(100, 137),
                                        Date == "2009-12-28" & Player_name == "Raymond van Barneveld" ~ mean(85, 86),
                                        Date == "2010-05-24" & Player_name == "Phil Taylor" & No. == 20 ~ mean(59, 100, 100),
                                        Date == "2010-05-24" & Player_name == "Phil Taylor" & No. == 21 ~ mean(100, 180),
                                        Date == "2010-04-29" & Player_name == "Raymond van Barneveld" ~ mean(96, 100, 134),
                                        Date == "2010-07-17" & Player_name == "Raymond van Barneveld" ~ mean(53, 84, 59),
                                        Date == "2011-01-03" & Player_name == "Adrian Lewis" ~ mean(180, 38, 96),
                                        Date == "2011-07-31" & Player_name == "Adrian Lewis" ~ mean(58, 134),
                                        Date == "2012-07-25" & Player_name == "Michael van Gerwen" ~ mean(100, 81),
                                        Date == "2012-12-30" & Player_name == "Michael van Gerwen" ~ mean(94, 96, 41),
                                        Date == "2022-01-01" & Player_name == "Gerwyn Price" ~ mean(140, 96),
                                        Date == "2022-02-17" & Player_name == "Gerwyn Price" & Opponent == "Michael van Gerwen" ~ mean(123, 59),
                                        Date == "2022-02-17" & Player_name == "Gerwyn Price" & Opponent == "James Wade" ~ mean(60, 100),
                                        Date == "2022-07-23" & Player_name == "Gerwyn Price" ~ mean(58, 96),
                                        Date == "2023-01-03" & Player_name == "Michael Smith" ~ mean(180, 177, 120),
                                        TRUE ~ NA_real_)) |> 
  # join columns from plotting df
  left_join(df_niners_plot |> select(Player_name, Year, year_rank_num, player_col, n_niners_plot) |> mutate(Player_name = as.character(Player_name)),
            by = c("Player_name", "Year")) |> 
  # select relevant columns
  select(Player_name, Year, year_rank_num, player_col, n_niners_plot, Opponent, opponent_avg_score)


# data for coloured rings
df_rings <- df_niners_plot |>
  
  # pick Year (group value) & year_rank columns
  select(Year, 
         starts_with("year_rank")) |> 
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


        # # colour gradient for players n >=2
        # col_range <- 
        # 
        # # colour gradient for other players (n < 2)
        # greys.range <- colorRampPalette(c(lightgrey, darkgrey))
        # greys_range <- greys.range(length(df_niners_plot |> filter(n_niners < 2) |> pull(Player_name)))  

# Make plot ----

(niners_plot <- ggplot() +
   
  # darts board design: coloured segments
   # green ring for base
   geom_rect(aes(xmin = 0, xmax = length(unique(df_niners_plot$year_rank)) + 1,
                 ymin = empty_space_radius_inner, ymax = empty_space_radius_outer + 4),
             fill = green) +
   # inner beige ring
   geom_rect(aes(xmin = 0, xmax = length(unique(df_niners_plot$year_rank)) + 1,
                 ymin = empty_space_radius_outer, ymax = empty_space_radius_outer + 1),
             fill = beige) +
   # inner red/green ring
   geom_rect(data = df_rings,
             aes(xmin = year_rank_num - 1/2, xmax = year_rank_num + 1/2,
                 ymin = empty_space_radius_outer + 1, ymax = empty_space_radius_outer + 2,
                 fill = fill_col)) +
   # outer beige ring
   geom_rect(aes(xmin = 0, xmax = length(unique(df_niners_plot$year_rank)) + 1,
                 ymin = empty_space_radius_outer + 2, ymax = empty_space_radius_outer + 3),
             fill = beige) +
   # outer red/green ring
   geom_rect(data = df_rings,
             aes(xmin = year_rank_num - 1/2, xmax = year_rank_num + 1/2,
                 ymin = empty_space_radius_outer + 3, ymax = empty_space_radius_outer + 4,
                 fill = fill_col)) + scale_fill_identity(guide = "none") +
  # set up new fill scale for points 
   ggnewscale::new_scale_fill() +
   
  # start-of-time line
   geom_segment(aes(x = 0.5, xend = 0.5,
                    y = empty_space_radius_outer, yend = empty_space_radius_outer + 4.5),
                colour = "grey15",
                lineend = "round",
                linewidth = 2) +
   
  # 9-darter lines
   geom_segment(data = df_niners_plot,
                aes(x = year_rank_num,
                    xend = year_rank_num,
                    y = empty_space_radius_outer, 
                    yend = n_niners_plot,
                    colour = player_col),
                position = position_dodge(width = 0.5)) +
   # add end points
   geom_point(data = df_niners_dots,
              aes(x = year_rank_num,
                  y = n_niners_plot,
                  fill = player_col,
                  size = opponent_avg_score),
              pch = 21,
              colour = "grey",
              show.legend = FALSE) +
   
   # scale point size
   scale_size_continuous(range = c(1, 8),
                         guide = "none") +
   
  # set colour scheme
   scale_colour_identity(guide = "none") +
   scale_fill_identity(guide = "none") +
   
  # add two inner rectangles which will become the single bull / bulls eye
       # geom_rect(aes(xmin = 0, xmax = length(unique(df_niners_plot$year_rank)) + 1,
       #               ymin = empty_space_radius_inner, ymax = empty_space_radius_outer),
       #           fill = green) +
   geom_rect(aes(xmin = 0, xmax = length(unique(df_niners_plot$year_rank)) + 1,
                 ymin = 0, ymax = empty_space_radius_inner),
             fill = red) +
   # set circular layout
   coord_polar() +
   # set theme
   theme_void())
