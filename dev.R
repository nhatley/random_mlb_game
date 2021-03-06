library(tidyverse)
library(httr)
library(rvest)


## boom: can get gamepacks by date (still need to clean it) for an entire season below
all_games = GET('http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&season=2019&startDate=2019-03-20&endDate=2019-09-29') %>%
  content()

## reminder I can get start and end date with my existing function 
## https://github.com/nhatley/MLB/blob/master/code/functions/get_mlb_league_game_dates.R

team_crosswalk = read_rds("data/team_crosswalk.rds") %>% 
  rename(name_snake = fg_url_name,
         name = full_name)
  

all_games_by_date = all_games[["dates"]][1] %>% 
  map(function(by_date_list){
    date_set = by_date_list[["date"]]
    date_set_number_of_games = by_date_list[["totalItems"]]
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
        mutate(across(everything(), ~as.character(.x)))
                 
      lst_items = .x %>% 
        keep(~class(.x) == "list")
      
      team_info = lst_items[["teams"]] %>% 
        imap(~{
          .x[["team"]] %>% 
            enframe %>% 
            spread(name, value) %>% 
            select(-link) %>% 
            mutate(across(everything(), as.character)) %>% 
            mutate(home_away = .y)
        }
          ) %>% 
        bind_rows() %>% 
        pivot_wider(names_from = home_away,
                    values_from = c(id,name)) %>% 
        set_names(~paste0("team_", .x))
      
      venue_info = lst_items[["venue"]] %>% 
        enframe %>% 
        spread(name, value) %>% 
        select(-link) %>% 
        mutate(across(everything(), as.character)) %>% 
        set_names(~paste0("venue_", .x)) 
      
      out = non_lst_item_df %>% 
        mutate(gameDate = as.Date(gameDate)) %>% 
        bind_cols(team_info) %>% 
        bind_cols(venue_info) %>% 
        select(one_of(
          "gamePk", "gameDate", #"gamedayType" idk what this is
          "gameNumber", "gamesInSeries" 
        ),
        starts_with("team_"),
        one_of(
          "season", "seasonDisplay", 
          "seriesDescription", "seriesGameNumber"
          ),
        starts_with("venue_")
        )
      return(out)  
    }) %>% 
      bind_rows %>% 
      mutate(
        date_set = date_set,
        date_set_number_of_games = date_set_number_of_games
      )
    
    
    
  }) %>% 
  bind_rows %>% 
  left_join(team_crosswalk %>% 
              set_names(~paste0("team_", .x, "_away"))
            ) %>% 
  left_join(team_crosswalk %>% 
              set_names(~paste0("team_", .x, "_home"))
  )





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

input = list()
input$choice = 1
input$season = 2019
input$team = 'Atlanta Braves'
input$home_away = ''

no_selection <- function(x) stringr::str_length(x) == 0

server <- function(input, output, session) {
  
  datasetArgs <- eventReactive(input$link, {
    list(
      choice = input$link,
      season = input$in_season,
      team = input$in_team,
      home_away = input$in_home_away,
      vs_team = input$in_vs_team,
      vs_division = input$in_vs_division,
      vs_leauge = input$in_vs_league
    )
  })
  
  make_link <- function(x){# x = datasetArgs
    
    season_df <- if(x[["season"]] == 2019) {
      readRDS(
        url(
          "https://github.com/nhatley/random_mlb_game/raw/master/data/games_processed_2019.rds"
        )
      )
    } else if(x[["season"]] == 2018) {
      readRDS(
        url(
          "https://github.com/nhatley/random_mlb_game/raw/master/data/games_processed_2018.rds"
        )
      )
    } else{
      readRDS(
        url(
          "https://github.com/nhatley/random_mlb_game/raw/master/data/games_processed.rds"      
        )
      )
    }
    
    team_df <-  if(no_selection(x[["team"]])) {
      season_df 
    } else{
      if(x[["home_away"]] == "Home"){
        season_df %>% 
          filter(team_name_home == x[["team"]])      
      } else if(x[["home_away"]] == "Away"){
        season_df %>% 
          filter(team_name_away == x[["team"]])
      } else{
        season_df %>% 
          filter(team_name_away == x[["team"]] | team_name_home == x[["team"]])          
      }
      
    }
    
    skip_vs <- all(no_selection(x[["vs_team"]]), 
                   no_selection(x[["vs_division"]]),
                   no_selection(x[["vs_leauge"]])
    )
    
    if (!skip_vs){
      
      team_df <- if(!(no_selection(x[["vs_team"]]))) {
        vs_division <- ""
        vs_leauge <- ""
        
        team_df <- team_df %>% 
          filter(team_name_away == x[["vs_team"]] | team_name_home == x[["vs_team"]])          
      } else if(!(no_selection(x[["vs_division"]]))){
        vs_leauge <- ""
        team_df <- team_df %>% 
          filter(team_division_away == x[["vs_division"]] | team_division_home == x[["vs_division"]])          
      } else{
        team_df <-team_df %>% 
          filter(team_league_away == x[["vs_league"]] | team_league_home == x[["vs_league"]])
      }
      
    }
    
    game <- sample(team_df[["gamePk"]], size = 1)
    
    link_df = team_df %>% filter(gamePk == game)
    
    link <- paste0("MLBTV Link: ", "\n", 
                   'https://www.mlb.com/tv/g', 
                   game)
    
    out = list(
      game_date = unique(link_df$gameDate)[1],
      game_team_away = unique(link_df$team_abbreviation_away)[1],
      game_team_home = unique(link_df$team_abbreviation_home)[1],
      gamePK = game,
      game_link = link
    )
    
    return(out)
  }
  
  components <-  reactive({
    input$link
    make_link(datasetArgs())
  })
  
  output$link <- renderText({ components()[["game_link"]] })
  output$game_details <- renderText({
    paste0(
      components()[["game_date"]], "\n",
      components()[["game_team_away"]], " at ",
      components()[["game_team_home"]]
    )
    
  })
  
}