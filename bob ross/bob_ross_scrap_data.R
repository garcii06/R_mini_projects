# Script for the web scraping of the program The Joy of Painting.
library(rvest)
library(lubridate)
library(tidyverse)

# Scrap All Seasons ---------------------------------------------------------------------------
# Get the individual elements of a single Season.
season_data <- function(season){
  base_season <- str_c("https://www.imdb.com/title/tt0383795/episodes?season=", season, "&ref_=ttep_ep_sn_nx")
  
  season_list_ep <- read_html(base_season) |> 
    html_elements(".list_item")
  
  season_info <- tibble(Season = season,
         episode_air_date = season_list_ep |> html_element(".airdate") |> html_text2(), 
         episode_name = season_list_ep |> html_element("strong") |> html_text2(),
         episode_rate = season_list_ep |> html_element(".ipl-rating-star__rating") |> html_text2(),
         episode_votes = season_list_ep |> html_element(".ipl-rating-star__total-votes") |> html_text2(),
         episode_description = season_list_ep |> html_element(".item_description") |> html_text2()
  )
  return(season_info)
}

# Loop through all the seasons, and append them into a tibble.
for (season in 1:31) {
  if(season == 1)
    all_seasons <- tibble()
  
  all_seasons <- bind_rows(all_seasons, season_data(season))
}

# View the result tibble.
all_seasons |> 
  view()

# Data Cleansing ------------------------------------------------------------------------------
all_seasons <- all_seasons |> 
  mutate(episode_air_date = dmy(episode_air_date),
         episode_rate = parse_number(episode_rate),
         episode_votes = parse_number(episode_votes)
         ) |> 
  group_by(Season) |> 
  mutate(episode = row_number()) |> 
  select(Season, episode, everything())

# Loading Data --------------------------------------------------------------------------------
write_csv(all_seasons, "SQL_mini_projects/Data/bob_ross/all_seasons.csv")

# Scrap Individual Elements -------------------------------------------------------------------
base_season <- "https://www.imdb.com/title/tt0383795/episodes?season=1&ref_=ttep_ep_sn_nx"

season_list_ep <- read_html(base_season) |> 
  html_elements(".list_item")

# Air date of the episode
 season_list_ep |> 
  html_element(".airdate") |> 
  html_text2()

# Name of the episode
 season_list_ep |> 
  html_element("strong") |> 
  html_text2()

# Rate of the episode
season_list_ep |> 
  html_element(".ipl-rating-star__rating") |> 
  html_text2()

# Votes of the episode
 season_list_ep |> 
  html_element(".ipl-rating-star__total-votes") |> 
  html_text2()

# Description of the episode
 season_list_ep |> 
  html_element(".item_description") |> 
  html_text2()
