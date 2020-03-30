library(tidyverse)
library(httr)
library(rvest)


## boom: can get gamepacks by date (still need to clean it) for an entire season below
all_games = GET('http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&season=2019&startDate=2019-03-20&endDate=2019-09-29') %>%
  content()

## reminder I can get start and end date with my existing function 
## https://github.com/nhatley/MLB/blob/master/code/functions/get_mlb_league_game_dates.R

all_games_by_date = all_games[["dates"]] %>% 
  map(function(by_date_list){
    map(by_date_list[["games"]], ~{ 
      
      non_lst_items = .x %>% 
        discard(~class(.x) == "list") %>% 
        enframe(name = "column_name", value = "column_value") %>% 
        rowwise %>% 
        mutate(column_class = class(column_value)) %>% 
        ungroup 
          
      non_lst_column_classes = non_lst_items %>% 
        distinct(column_name, column_class) 
      
      non_lst_item_df = non_lst_items %>%
        select(-column_class) %>% 
        spread(column_name, column_value) %>% 
        ## temporary
        mutate(across(everything(), ~as.character(.x)))
      
      # %>% 
      #   mutate(across(everything()), function(col){
      #     out_class = non_lst_column_classes %>% 
      #       filter(column_name == col) %>% 
      #       pull(column_class)
      #   }
                 
      lst_items = .x %>% 
        keep(~class(.x) == "list")
      
      team_info = lst_items[["teams"]] %>% 
        imap(~.x %>% 
               enframe %>% 
               mutate(home_away = .y)
               )
      # %>% 
      #   enframe
      
    })
    
  })
    

test_date_list = all_games_by_date[[1]]



test_date_list$games




# mlb api game_type flags
# string (required) Example: 'R'
# The type of games you want career stats for.
# 'R' - Regular Season
# 'S' - Spring Training
# 'E' - Exhibition
# 'A' - All Star Game
# 'D' - Division Series
# 'F' - First Round (Wild Card)
# 'L' - League Championship
# 'W' - World Series


z = GET(url_ping) %>% content()

# 
z = GET("http://lookup-service-prod.mlb.com/json/named.org_game_type_date_info.bam?current_sw='Y'&sport_code='mlb'game_type='L'&season='2019'") %>% content


#get_mlb_league_game_dates <- function(){
  
  season = '2019'
  game_type = 'R'
  
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
  
  season_game_info = map_df(
    season_game_info_url[["org_game_type_date_info"]][["queryResults"]][["row"]],
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
  season_game_info
  
#   return(season_game_info)
# }


## first game id of 2019 = g566083
## last game id of 2019 = g565782
  
  
  
  
base_url <- "http://gd2.mlb.com/components/game/mlb/year_2019"
#year_2019/month_05/day_15
#"/components/game/mlb/year_2014/month_05/day_15/gid_2014_05_15_sdnmlb_cinmlb_1"
x = GET(base_url)
  
statcast_url <- 'https://baseballsavant.mlb.com/gamefeed?game_pk=567131&game_date=2019-9-9'
statcast_tables <- statcast_url %>% 
  read_html() %>% 
  html_table()