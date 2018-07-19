library(shiny) 
library(tidyverse)
library(shinydashboard)
library(plotKML)
library(dplyr)
library(geosphere)
library(lubridate)
library(shinyjs)

files <- list.files(pattern = "\\b20")

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "MA104 Running App"),
                    dashboardSidebar(useShinyjs(),
                                     sidebarMenu(
                                       menuItem("Data", tabName = "Data", icon = icon("dashboard"), startExpanded = TRUE, 
                                                # fileInput('csvfile','Upload a CSV File',
                                                #           accept = c('text/csv',
                                                #                      'text/comma-separated-values,text/plain',
                                                #                      '.csv',
                                                #                      '.gpx')
                                                #  )
                                                fileInput("files", "Upload", multiple = TRUE, accept = c(".gpx")),
                                                textInput("textid", "First.Last", "Dusty.Turner"),
                                                actionButton("do", "Click Me")
                                                
                                       ),
                                       menuItem("Selections", tabName = "Selections", icon = icon("dashboard"), startExpanded = TRUE,
                                                uiOutput("ui1"))
 
                                     )),
                    
                    
                    dashboardBody(
                                 tableOutput("text")
                      )
)                    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  first = reactive({
    text = paste0("C:/Users/",input$textid,"/Desktop/textfolder")
    setwd(paste0("C:/Users/",input$textid,"/Desktop/textfolder"))
    return(text)
  })

  output$text = renderTable(first())

  
  observeEvent(input$do, {
    
    files <- list.files(pattern = "\\b20")
    
    readGPX(files[1], way=T)
    
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
      
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

