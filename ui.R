## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')
source('functions/movies_by_genre.R')

shinyUI(fluidPage (
  uiOutput("make_top_movie_list"),
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommendations", titleWidth = 400),
    
    dashboardSidebar(disable = FALSE,
                     sidebarMenu(
                       menuItem("Select by rating", tabName = "UBCF", icon = icon("star")),
                       menuItem("Select by genre", tabName = "Genre", icon = icon("fire"))
                     )),
    
    dashboardBody(includeCSS("css/movies.css"),
                  tabItems(
                    tabItem(tabName = "UBCF",
                            fluidRow(
                              box(
                                width = 12,
                                title = "Step 1: Rate as many movies as possible",
                                status = "info",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                div(class = "rateitems",
                                    uiOutput('ratings'))
                              )
                            ),
                            fluidRow(
                              useShinyjs(),
                              box(
                                width = 12,
                                status = "info",
                                solidHeader = TRUE,
                                title = "Step 2: Discover movies you might like",
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                ),
                                br(),
                                tableOutput("ubcf_results")
                              )
                            )),
                    tabItem(tabName = "Genre",
                            fluidRow(
                              box(
                                width = 12,
                                status = "info",
                                collapsible = TRUE,
                                solidHeader = TRUE,
                                title = "What is your favorite genre?",
                                withBusyIndicatorUI(selectInput(
                                  "favorite_genre", "Favorite genre", genre_list
                                )),
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("submit_genre", "Submit", class = "btn-warning")
                                ),
                                br(),
                                tableOutput("genre_results")
                              )
                            ))
                  ))
  )
))
