library(shiny)
library(dplyr)

## example: https://shiny.rstudio.com/gallery/selectize-vs-select.html
## (am v bad at shiny)

## overview of shiny::reactive()
## https://shiny.rstudio.com/articles/reactivity-overview.html
## reactive shiny example
## https://shiny.rstudio.com/gallery/reactivity.html

## search button 
## https://shiny.rstudio.com/gallery/widget-gallery.html

team_crosswalk <- readRDS(
    url(
        "https://github.com/nhatley/random_mlb_game/raw/master/data/team_crosswalk.rds"
    )
)

ui <- fluidPage(
    br(),
    fluidRow(
        column(4,
               h4("MLB Season")
        ),
        column(4,
               h4("Team")
        ),
        column(4,
               h4("As Home or Away")
        )
    ),
    fluidRow(
        column(4,
               hr(),
               selectInput('in_season', label = NULL,
                           c(Choose='', "2019", "2018"), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_team', label = NULL,
                           c(Choose='', team_crosswalk[["full_name"]]), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_home_away', label = NULL,
                           c(Choose = '', "Home", "Away"), selectize=FALSE)
        )
    ),
    fluidRow(
        column(4,
               h4("Vs Team")
        ),
        column(4,
               h4("Vs Division")
        ),
        column(4,
               h4("Vs League")
        )
    ),
    fluidRow(
        column(4,
               hr(),
               selectInput('in_vs_team', label = NULL,
                           choices = c(Choose='', team_crosswalk[["full_name"]]), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_vs_division', label = NULL,
                           choices = c(Choose='', unique(team_crosswalk[["division"]])), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_vs_league', label = NULL, 
                           choices = c(Choose='', unique(team_crosswalk[["league"]])), 
                           selectize=FALSE)
                )
    ),
    actionButton("link", label = "Find a game", width = "40%"),
    hr(),
    mainPanel(
        tabsetPanel(
            tabPanel("Link", verbatimTextOutput("link")),
            tabPanel("Game Details", verbatimTextOutput("game_details")), 
            selected = "Link",
            type = "pills"
                
        )
    )
)

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

shinyApp(ui = ui, server = server)
