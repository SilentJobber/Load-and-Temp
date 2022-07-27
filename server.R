library(tidyverse)

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

