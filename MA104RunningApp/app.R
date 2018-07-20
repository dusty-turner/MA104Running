library(shiny) 
library(tidyverse)
library(shinydashboard)
library(plotKML)
library(dplyr)
library(geosphere)
library(lubridate)
library(shinyjs)
library(shinyFiles)
library(leaflet)
library(DT)
library(RColorBrewer)

# setwd(choose.dir(getwd(),"Choose a suitable folder")) # select subfolder 'scripts', works OK
# setwd("C:/Users/Andrew.Plucker/Desktop/textfolder")
setwd("C:/Users/Dusty.Turner/Desktop/textfoldera")

# files <- list.files(pattern = "\\b20")

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "MA104 Running App"),
                    dashboardSidebar(useShinyjs(),
                                     sidebarMenu(
                                       menuItem("Input Panel", tabName = "Data", icon = icon("dashboard"), startExpanded = TRUE,
                                                actionButton("do", "Transform Data"),
                                                conditionalPanel(
                                                  condition = "input.do == true",
                                                  actionButton("gomap","View Map")),
                                                conditionalPanel(
                                                  condition = "input.do == true",
                                                  uiOutput("ui1"),
                                                  uiOutput("ui2"))
                                       )
                                      )),
                    dashboardBody(
                                 leafletOutput("mymap"),
                                 textOutput("text"),
                                 DTOutput("table")
                      )
)                    
# Define server logic required to draw a histogram
server <- function(input, output) {
  


  
  observeEvent(input$do, {
    
    files <- list.files(pattern = "\\b20")
    
    readGPX(files[1], way=T)
    
    totalframe = NULL
    
    for (j in 1:length(files)) {
      # Select first file from the list and import data into R object
      wplist <- readGPX(files[j], way=T)
      
      # Extract latitude, longitude, elevation, time, name and comments and apppend to R dataframe
      wpdf<- wplist$tracks[[1]][[1]] 
      
      # Create vector to store calculations for distance between data points (distance given in meters) and set starting distance to 0 
      distBetween <- c()
      distBetween[1] = 0
      
      # Loop to calculate distances between all collected points
      for (i in 1:(nrow(wpdf)-1)) {
        distBetween[i+1] = distm(cbind(wpdf$lon[i],wpdf$lat[i]),cbind(wpdf$lon[i+1],wpdf$lat[i+1]),fun=distHaversine)
      }
      
      # Append the between distances as a column to the data 
      wpdf["distBetween"] <- distBetween
      
      # Convert imported date and time data into a usable format (note: time zone defaults to Z)
      wpdfNew = wpdf %>% 
        mutate(DTG = ymd_hms(time))
      
      # Create vector to store calculations for time between data points (time given in seconds) and set starting time to 0
      timeDiff <- c()
      timeDiff[1] = 0
      
      # Loop to calculate the time (in seconds) between all collected points
      for(i in 1:(nrow(wpdfNew)-1)) {
        timeDiff[i+1] = as.numeric(difftime(wpdfNew$DTG[i+1],wpdfNew$DTG[i],units = "sec"))
      }
      
      # Append the between times as a column to the data
      wpdfNew["timeDiff"] <- timeDiff
      
      # Mutate the data so that we have the cumulative time and distances and then select the data that we want.
      wpdfNew = wpdfNew %>%
        mutate(totDist = cumsum(distBetween))%>%
        mutate(totTime = cumsum(timeDiff)) %>%
        select(DTG, lon, lat, totTime, totDist, ele)
      
      # Write .csv file
      write.csv(wpdfNew, paste0("Workout_",j,".csv"))
      totalframe = rbind(totalframe,wpdfNew)

    }
  
  })
  
  # maphelper =  reactive({
  #                     mydata =read.csv("Workout_1.csv")
  #   return(mydata)
  #   })
  

 
  observeEvent(input$gomap, {
  
    temp = list.files(pattern="*.csv")
    myfiles = lapply(temp, read.delim)
    
    firsthelper = NULL
    helper = NULL
    for(i in 1:length(myfiles)){
      helper =  as.data.frame(
        matrix(
          unlist(
            strsplit(
              as.character(unlist(myfiles[[i]][[1]])) , ","
            )
          ), ncol = 7, byrow = T
          
        )
      )
      
      n <- nrow(helper)
      v <- rep(i,n)
      helper = cbind(helper,v)
      
      firsthelper = rbind(firsthelper,helper)
    }
    names(firsthelper) = c("X", "DTG", "lon", "lat", "totTime", "totDist", "ele","woNum")
    
    firsthelper = firsthelper %>%
      mutate(DTG = as.POSIXct(DTG)) %>%
      mutate(lon = as.numeric(as.character(lon))) %>%
      mutate(lat = as.numeric(as.character(lat))) %>%
      mutate(totTime = as.numeric(as.character(totTime))) %>%
      mutate(totDist = as.numeric(as.character(totDist))) %>%
      mutate(ele = as.numeric(as.character(ele))) 
    
    # firsthelper$ele = as.numeric(as.character(firsthelper$ele))
    
    
    
  output$mymap <- renderLeaflet({

    pal <- colorFactor("Dark2", firsthelper$woNum, levels = unique(firsthelper$woNum))
    
    firsthelper %>%
      filter(DTG>=input$slider[1]) %>%
      filter(DTG<=input$slider[2]) %>%
      filter(woNum==input$selections) %>%
    leaflet()  %>%
      addTiles() %>%
      addCircleMarkers(radius = 1, color = ~pal(woNum))
  })
  
  # output$table = renderTable(maphelper())
  output$table = renderDT(
    firsthelper %>%
      filter(DTG>=input$slider[1]) %>%
      filter(DTG<=input$slider[2]) %>%
      filter(woNum==input$selections)
    )
  
  output$text = renderText(
  unique(firsthelper$woNum[which(firsthelper$DTG > input$slider[1] & firsthelper$DTG < input$slider[2]) ])
  # unique(firsthelper$woNum[which(firsthelper$DTG>input$slider[1])]&firsthelper$woNum[which(firsthelper$DTG<input$slider[2])])
  )
  
   # firsthelpernames = observe({
   #   this = firsthelper %>%
   #     filter(DTG>=input$slider[1]) 
   #   thisa = unique(this$woNum)
   #   return(thisa)
   # })
  
  output$ui1 <- renderUI({
    # if (is.null(input$dataset))
    #   return()
    checkboxGroupInput(
      "selections",
      label = h4("Display Routes"),
      choices =   unique(firsthelper$woNum[which(firsthelper$DTG > input$slider[1] & firsthelper$DTG < input$slider[2]) ]),
      selected =   unique(firsthelper$woNum[which(firsthelper$DTG > input$slider[1] & firsthelper$DTG < input$slider[2]) ])

    )
  })

  output$ui2 <- renderUI({
    # if (is.null(input$dataset))
    #   return()
    # sliderInput("slider", "Time", min = as.Date("2010-01-01"),max =as.Date("2018-12-01"),value=c(as.Date("2010-01-01"),as.Date("2018-12-01")),timeFormat="%b %Y")
    sliderInput("slider", "Time", min = as.Date(min(firsthelper$DTG)),max =as.Date(max(firsthelper$DTG)),value=c(as.Date(min(firsthelper$DTG)),as.Date(max(firsthelper$DTG))),timeFormat="%b %Y")
  })

  # output$text = renderText(as.Date(input$slider[1]))
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

