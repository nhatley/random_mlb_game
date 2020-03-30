get_mlb_season_open_and_end_dates <- function(season = '2019',
                                      game_type = 'R'){
  
  # mlb api game_type flags
  # string (required) Example: 'R'
  # 'R' - Regular Season
  # 'S' - Spring Training
  # 'E' - Exhibition
  # 'A' - All Star Game
  # 'D' - Division Series
  # 'F' - First Round (Wild Card)
  # 'L' - League Championship
  # 'W' - World Series
  
  
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(rvest))
  
  
  url_base = "http://lookup-service-prod.mlb.com/json/named.org_game_type_date_info.bam?current_sw='Y'&sport_code='mlb'"
  
  if(game_type == "R"){
    game_type_in = "&game_type='R'"  
  } else {
    game_type_in = paste0("&game_type='", game_type, "'")  
  }
  
  season_in = paste0("&season='", season, "'")  
  
  url_ping = paste0(url_base, game_type_in, season_in)
  
  season_game_info_url = GET(url_ping) %>% content()
  
  season_game_info = map_df(season_game_info_url[["org_game_type_date_info"]][["queryResults"]][["row"]],
                            ~enframe(.x) %>% 
                              mutate(value = as.character(value)) %>%
                              spread(name, value)
  )  %>% 
    filter(league_code != "") %>% 
    select(-one_of(
      "playoff_round", "playoffs_sw", "round_robin_sw", "sport_code"
    )
    ) %>% 
    mutate_at(vars(ends_with("_date")), ~lubridate::as_date(.x))
  
  
  return(season_game_info)
}