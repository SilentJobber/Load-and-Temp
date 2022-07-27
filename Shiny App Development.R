rm(list = setdiff(ls(),c(lsf.str())))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("Packages and Functions.R" )

library(tidyverse)
library(shiny)
library(shinythemes)
list.files()

# input <- read.csv("test.csv") %>% mutate(x = as.Date(x))
# tempload <- read.csv("Shiny_load_temp.csv") %>% mutate(DateTime = as.POSIXct(DateTime,tz = "UTC"))
# tempload <- data.frame.DatetimeConversion(tempload) %>% select(-Hour,-Year,-Month) %>% group_by(Date) %>% mutate(PeakSYSTEM = max(SYSTEM)) %>% ungroup() %>% mutate(PeakSYSTEM = replace(PeakSYSTEM,PeakSYSTEM != SYSTEM,NA))
# df_filtered <- tempload %>% filter(Date >= input[1,2] & Date <= input[2,2]) %>% select(DateTime,temp,SYSTEM)


# scale <- 5
# ggplot(df_filtered,aes(x = DateTime)) + 
#   geom_line(aes(y = SYSTEM)) + 
#   geom_point(aes(y = temp*scale)) +   
#   scale_y_continuous(
#   # Features of the first axis
#   name = "First Axis",
#     # Add a second axis and specify its features
#   sec.axis = sec_axis(~./scale,name="Second Axis")
# )
# ?sec_axis

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Date Range Input:",
                           

                  tabPanel("Home",
                           
                  sidebarPanel(
                    dateRangeInput(
                      'daterange',
                      label = paste('Date Range Selection: Date Range cannot be greater than 31 Days'
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
####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
      tempload <- read.csv("Shiny_load_temp.csv") %>% mutate(DateTime = as.POSIXct(DateTime,tz = "UTC"))
      tempload <- data.frame.DatetimeConversion(tempload) %>% 
        select(-Hour,-Year,-Month) %>% 
        group_by(Date) %>% 
        mutate(PeakSYSTEM = max(SYSTEM)) %>% 
        ungroup() %>% 
        mutate(PeakSYSTEM = replace(PeakSYSTEM,PeakSYSTEM != SYSTEM,NA))
    
      observe({updateDateRangeInput(session,"daterange", min = min(tempload$DateTime))})
      
      observeEvent(input$daterange, {
        end_date = input$daterange[2]
        # If end date is earlier than start date, update the end date to be the same as the new start date
        if (input$daterange[2] < input$daterange[1]) {
          end_date = input$daterange[1]
        }
                if(input$daterange[2] - input$daterange[1] > 31){
          end_date = input$daterange[1] + 31
        }
        updateDateRangeInput(session,"daterange", start=input$daterange[1], end=end_date)
      })

      
      
      # output$text <- renderText({
      #   validate(
      #     need(input$daterange[2] >= input$daterange[1], "End date cannot be earlier than start date!")
      #   )
      #   input$daterange[2] >= input$daterange[1]
      # })
      
      
      
      
      
  data <- reactive({ 
    if(input$submitbutton>0){
     isolate(df_filtered <- tempload %>% filter(Date >= input$daterange[1] & Date <= input$daterange[2]) %>% select(DateTime,temp,SYSTEM,PeakSYSTEM))
    }
    #write.csv(input$daterange,"test.csv")
  })
  
  output$daterange <- renderText({
    
    if(input$submitbutton>0){
       isolate(paste("input$dateRange is", 
          paste(as.character(input$daterange), collapse = " to ")))
          }
        
  })
  output$plot1 <- renderPlot({
    if(input$submitbutton>0){
      
      isolate(ggplot(data(),aes(x = DateTime)) + 
                theme_light() + 
                labs(
                  title = "Modesto Irrigation District",
                  subtitle = "System Demand",
                  caption = "Data available from January 2008 through July 2022",
                  x = "DateTime",
                  y = "System Demand (MW)"
                ) + 
                geom_point(aes(y = PeakSYSTEM), size = 4,color = "blue",fill = "blue",alpha = .5,shape = 23,na.rm = T) +
                geom_line(aes(y = SYSTEM), color = "blue", size = 1.1)) +
                geom_label(aes(y = PeakSYSTEM,label = paste0(round(PeakSYSTEM,1)," MW")),
                  nudge_y = 20, 
                  na.rm = T
                )
      
      }
    
  })
  output$table1 <- renderTable({
    if(input$submitbutton>0){
    isolate(data() %>% mutate(DateTime = as.character(DateTime),PeakSYSTEM = NULL))
    }
  })
      
}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
