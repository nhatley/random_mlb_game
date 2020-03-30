process_game_list <- function(game_list){
  
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(rvest))
  
  team_crosswalk = read_rds("data/team_crosswalk.rds") %>% 
    rename(name_snake = fg_url_name,
           name = full_name)
  
  all_games_by_date = game_list %>% 
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
    bind_rows
  
  all_games_by_date = suppressMessages(suppressWarnings(
    all_games_by_date %>% 
    left_join(team_crosswalk %>% 
                set_names(~paste0("team_", .x, "_away"))
    ) %>% 
    left_join(team_crosswalk %>% 
                set_names(~paste0("team_", .x, "_home"))
    )
  ))
  

return(all_games_by_date)  
}
