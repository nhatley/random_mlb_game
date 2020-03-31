library(shiny)


shinyUI(
  fluidPage(
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
)


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
