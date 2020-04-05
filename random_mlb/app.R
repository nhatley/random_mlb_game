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
               h4("Home or Away")
        )
    ),
    fluidRow(
        column(4,
               hr(),
               selectInput('in_season', 
                           'MLB Season',
                           c(Choose='', "2019", "2018"), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_team', 'Team', 
                           c(Choose='', team_crosswalk[["full_name"]]), 
                           selectize=FALSE)
        ),
        column(4,
               hr(),
               selectInput('in_home_away', 'As Home or Away Team', 
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
    fluidRow(
        column(4,
        checkboxInput("in_game_details", label = "Show Game Details?", value = FALSE),
    )
    ),
    actionButton("link", label = "Find a game"),
    
    hr(),
    fluidRow(column(12, verbatimTextOutput("link")))
)

no_selection <- function(x) stringr::str_length(x) == 0

server <- function(input, output, session) {
    
    datasetArgs <- reactive({
            list(
                choice = input$link,
                season = input$in_season,
                team = input$in_team,
                home_away = input$in_home_away,
                vs_team = input$vs_team,
                vs_division = input$vs_division,
                vs_leauge = input$vs_league,
                show_game_details = input$in_game_details
        )
        
    })
    make_link <- function(x){# x = datasetArgs
        if(x[["choice"]] == 0){
            link <- cat("\n", '{ Click Find a Game } ')
        } else{   
        
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
        
        game <- sample(team_df[["gamePk"]], size = 1)
        
        
        
        link <- if(x[["show_game_details"]] == "No"){
        paste0('https://www.mlb.com/tv/g', 
                       game)
        } else{
        link_df = team_df %>% filter(gamePk == game)
            
        cat(
            "\n",    
            paste0(link_df$gameDate, " :: ",  link_df$team_abbreviation_home, " vs ",  link_df$team_abbreviation_away),
            "\n",    
            "MLBTV Link: ",    
        paste0('https://www.mlb.com/tv/g', 
                   game),
        "\n"
        
        )
        }
    }
        
        return(link)
    }

    
        output$link <- renderPrint({ make_link(datasetArgs()) })
        
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