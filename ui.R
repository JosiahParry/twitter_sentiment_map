library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Twitter Sentiment"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("tweet_sentiment", height = 500)
           )#,
           #box(width = NULL,
          #     dataTableOutput("top_tweets")
          # )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               uiOutput("twitter_search"),
               textInput("search_terms", label = "Select Search Term", placeholder = "Trump"),
               actionButton(inputId = "search", label = "Search", icon = icon("search")),
               p(
                 class = "text-muted",
                 paste("Note: Type in a phrase to search twitter.",
                       "The map will analyse the aggregate sentiment of that phrase",
                       "by city based on up to 150 tweets each."
                 )
               )
           ),
           box(width = NULL, status = "warning",
               sliderInput("n_cities", "Number of Cities", 
                           min = 5, max = 20, value = 10,
                           ticks = FALSE
               ),
               p(class = "text-muted",
                 paste("Note: Selects the largest n cities by population."))
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)