library(tidyverse)
library(httr)
library(rvest)

fs::dir_info("code/functions/") %>% 
  filter(type == "file") %>% 
  pull(path) %>% 
  walk(~source(.x))

seasons = c(2019, 2018)

walk(seasons, function(season){

season_open_date = get_mlb_season_open_and_end_dates(season, game_type = 'R') %>% 
  ##open date is first regular season game ('R')
  summarise(first_game_date = min(first_game_date)) %>%
  pull(first_game_date)
  
season_end_date = get_mlb_season_open_and_end_dates(season, game_type = 'W') %>% 
            ##end date is last WS game ('W')
            summarise(last_game_date = max(last_game_date)) %>%
  pull(last_game_date)

mlb_date_api = glue::glue(
  'http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&season={season}&startDate={season_open_date}&endDate={season_end_date}'
)

season_games = GET(mlb_date_api) %>%
  content()

season_game_list = season_games[["dates"]]

games_processed = process_game_list(season_game_list) %>% 
  filter(!(seriesDescription %in%  c("Exhibition Game", "Spring Training")))

games_processed %>% write_rds(paste0("data/games_processed_", season, ".rds"))

}) %>% 
  bind_rows
# %>% 
#   write_rds("data/games_processed.rds")

## stack datasets together 
# fs::dir_info("data/") %>% 
#   filter(type == "file") %>% 
#   filter(str_detect(path, "data/games_processed_")) %>% 
#   pull(path) %>% 
#   map(~read_rds(.x)) %>% 
#   bind_rows %>% 
#   write_rds("data/games_processed.rds")
                              

