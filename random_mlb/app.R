library(shiny)
library(tidyverse)

## example: https://shiny.rstudio.com/gallery/selectize-vs-select.html
## (am v bad at shiny)

## overview of shiny::reactive()
## https://shiny.rstudio.com/articles/reactivity-overview.html

all_seasons = read_rds(here::here("data/games_processed.rds"))
team_crosswalk = read_rds(here::here("data/team_crosswalk.rds"))

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
               h4("Home or Away")
        )
    ),
    fluidRow(
        column(4,
               hr(),
               selectInput('in_season', 'MLB Season', c("2019", "2018"), selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_team', 'Team', team_crosswalk[["full_name"]], selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_home_away', 'Home or Away', 
                           c(Choose = '', "Home", "Away"), selectize=FALSE)
        )
    )
)

server <- function(input, output, session) {
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
   
    
    
    }

shinyApp(ui = ui, server = server)


#     ,
#     fluidRow(
#         column(4,
#                h4("Vs. Options"),
#                p("You Do NOT HAVE to choose any of these, but they are here if you want"),
#                selectInput('in_division', 'Options', c(Choose='', state.name), selectize=FALSE)
#         ),
#         column(4,
#                hr(),
#                verbatimTextOutput('out2'),
#                selectInput('in2', 'Options', state.name, selectize=FALSE)
#         ),
#         column(4,
#                hr(),
#                verbatimTextOutput('out3'),
#                selectInput('in3', 'Options', state.name, multiple=TRUE, selectize=FALSE)
#         )
# )