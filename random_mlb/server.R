library(shiny)
library(dplyr)

all_seasons = readr::read_rds("data/games_processed.rds")
team_crosswalk = readr::read_rds("data/team_crosswalk.rds")

shinyServer(function(input, output) {
  
  season_df <- reactive({ 
    all_seasons %>%
      filter(season == input$in_season)
  })
  
  season_df_by_team <- reactive({ 
    season_df %>%
      filter(team_name_away == input$in_team | team_name_home == input$in_team)
  })
  home_away = reactive({
    if(input$in_home_away == "Home") home_away <- "Home" 
    if(input$in_home_away == "Away") home_away <- "Away" 
  })
  if (home_away == "Home"){
    season_df_by_team <- season_df_by_team %>% 
      filter(team_name_home == input$in_team)
  }
  if (home_away == "Away"){
    season_df_by_team <- season_df_by_team %>% 
      filter(team_name_away == input$in_team)
    
  }
  
  ## need to figure something out on gamePk
  game_link_gamepk = season_df_by_team %>% 
    sample_n(gamePk, 1) %>% 
    pull(gamePk)
  game_id = 1
  output$link <- textOutput({
    glue::glue('https://www.mlb.com/tv/g{game_id}')
  })
  
  
  
})
