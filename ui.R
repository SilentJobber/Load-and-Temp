library(shiny)
library(shinythemes)
####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Date Range Input:",
                           
                           
                           tabPanel("Home",
                                    
                                    sidebarPanel(
                                      dateRangeInput(
                                        'daterange',
                                        label = paste('Date range input 2: range is limited,',
                                                      'dd/mm/yy, language: fr, week starts on day 1 (Monday),',
                                                      'separator is "-", start view is year'
                                        ),
                                        start = as.Date("2022-6-1"),
                                        end = as.Date("2022-6-4"),
                                        min = as.Date("2022-1-1"),
                                        max = as.Date("2022-7-1"),
                                        separator = " - ",
                                        format = "mm/dd/yy",
                                        startview = 'year',
                                        language = 'En',
                                        weekstart = 1
                                      ),
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(verbatimTextOutput("daterange"),
                                              plotOutput("plot1"),
                                              tableOutput("table1")
                                    )
                           )
                ) #navbarPage
)