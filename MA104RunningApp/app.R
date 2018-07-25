library(tidyverse)
library(shinydashboard)
library(plotKML)
library(geosphere)
library(lubridate)
library(shinyjs)
library(leaflet)
library(DT)
library(RColorBrewer)
library(measurements)

# setwd(choose.dir(getwd(),"Choose a suitable folder")) # select subfolder 'scripts', works OK
# setwd("C:/Users/Andrew.Plucker/Desktop/textfolder")
# setwd("C:/Users/Dusty.Turner/Desktop/textfoldera")
# files <- list.files(pattern = "\\b20")

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "MA104 Running App"),
  dashboardSidebar(useShinyjs(),
                   sidebarMenu(
                     # menuItem(
                       # "Input Panel",
                       # tabName = "Data",
                       # icon = icon("dashboard"),
                       # startExpanded = TRUE,

                       fileInput(
                         'csvfile',
                         'Or Upload a CSV File',
                         accept = c(
                           'text/csv','text/comma-separated-values,text/plain','.gpx','.csv'
                         ),
                         multiple = TRUE
                       ),
                       tags$b("Download CSV Workout Data", tags$br()),
                       downloadButton("AdjMat", "Download Quality Reps .csv"),
                       tags$b(tags$br(),tags$br(),"View the Map"),
                       actionButton("gomap", "View Map"),
                       uiOutput("ui2"),
                       uiOutput("ui1")
                     # )
                   )),
  dashboardBody(
              
    fluidRow(
                box(title="Map of Quality Reps",status="warning", solidHeader = TRUE,
                  leafletOutput("mymap"),width=12, collapsible = TRUE)),
    fluidRow(
                box(title="Summary of Quality Reps", status="warning", solidHeader = TRUE,
                    DTOutput("text"),width=12, height=40, collapsible = TRUE))
  )
)
# Define server logic 
server <- function(input, output, session) {

  readfile = reactive({
    totalframe = NULL
    files = input$csvfile$datapath
    
    for (j in 1:length(files)) {
      wplist <- readGPX(files[j], way = T)
      wpdf <- wplist$tracks[[1]][[1]]
      distBetween <- c()
      distBetween[1] = 0
      # Loop to calculate distances between all collected points
      for (i in 1:(nrow(wpdf) - 1)) {
        distBetween[i + 1] = distm(cbind(wpdf$lon[i], wpdf$lat[i]),
                                   cbind(wpdf$lon[i + 1], wpdf$lat[i + 1]),
                                   fun = distHaversine)
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
      for (i in 1:(nrow(wpdfNew) - 1)) {
        timeDiff[i + 1] = as.numeric(difftime(wpdfNew$DTG[i + 1], wpdfNew$DTG[i], units = "sec"))
      }
      
      # Append the between times as a column to the data
      wpdfNew["timeDiff"] <- timeDiff
      
      # Mutate the data so that we have the cumulative time and distances and then select the data that we want.
      wpdfNew = wpdfNew %>%
        mutate(totDist = cumsum(distBetween)) %>%
        mutate(totTime = cumsum(timeDiff)) %>%
        select(DTG, lon, lat, totTime, totDist, ele) %>%
        mutate(woNum = j)
      
      totalframe = rbind(totalframe, wpdfNew)
      
    } ## End loop
    
    totalframe = totalframe %>%
      mutate(DTG = as.POSIXct(DTG)) %>%
      mutate(lon = as.numeric(as.character(lon))) %>%
      mutate(lat = as.numeric(as.character(lat))) %>%
      mutate(totTime = as.numeric(as.character(totTime))) %>%
      mutate(totDist = as.numeric(as.character(totDist))) %>%
      mutate(ele = as.numeric(as.character(ele)))
    
    return(totalframe)
  })

  output$text = renderDT({
    validate(need(input$csvfile$datapath != "", "Upload Your Quality Reps"))
    
    readfile() %>%
      group_by(woNum) %>%
      summarise(
        `Start Time` = min(DTG),
        `Distance (Miles)`  = max(totDist) %>% conv_unit(from = "m", to = "mi") %>% round(2),
        `Elevation Change (Feet)` = (max(ele) - min(ele)) %>% conv_unit("m", "ft") %>% round(2),
        # Time = max(DTG),
        # Time = difftime(max(DTG),min(DTG)),
        `Time (Minutes)` = (max(DTG) - min(DTG)) %>% round(2)
        # Time1 = max(DTG),
        # Time2 = min(DTG))
      )
  },height = "auto")  
  
  output$AdjMat <- downloadHandler(
    filename = function() {
      paste0("QualityRunReps.zip")
    },
    content = function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      iterations = readfile() %>% select(woNum) %>% summarise(max = max(woNum))
      iterations = iterations$max
      
      #loop through the sheets
      for (k in 1:iterations) {
        #write each sheet to a csv file, save the name
        fileName <- paste("QualityRep", k, ".csv", sep = "")
        # write.table(as.data.frame(as.matrix(g[])), fileName,sep = " ")
        write.csv(readfile() %>%
                    filter(woNum == k) %>%
                    as.data.frame(),
                  fileName)
        # write.table(data()$wb[i],fileName,sep = ';', row.names = F, col.names = T)
        files <- c(fileName, files)
      }
      #create the zip file
      zip(file, files)
    }
  )
  
########  
  
  # output$text2 = renderText({
  #   thing = class(readfile()$DTG)
  #   return(thing)
  # })

#Create an empty map for initial display  
  output$mymap<-renderLeaflet({
    leaflet()%>%setView(lng = -73.9560, lat = 41.3915, zoom = 15)%>%
    addTiles()
  })
  
  observeEvent(input$gomap, {
    output$mymap <- renderLeaflet({
      pal <-
        colorFactor("Dark2", readfile()$woNum, levels = unique(readfile()$woNum))
      
      if(is.null(input$selections)){
       
        leaflet()%>%
          setView(lng = -73.9560, lat = 41.3915, zoom = 15)%>%
          addTiles()
      }
        else {
       
         readfile() %>%
          filter(DTG >= input$slider[1]) %>%
          filter(DTG <= input$slider[2]) %>%
          filter(woNum == input$selections) %>%
          leaflet()  %>%
          addTiles() %>%
          addCircleMarkers(radius = 1, color = ~ pal(woNum))
        }
      
    })
    
    output$table = renderDT(
      readfile() %>%
        filter(DTG >= input$slider[1]) %>%
        filter(DTG <= input$slider[2]) %>%
        filter(woNum == input$selections)
    )
    
    
    output$ui1 <- renderUI({
      checkboxGroupInput(
        "selections",
        label = h4("Display Routes"),
        choices =   unique(readfile()$woNum[which(readfile()$DTG >= input$slider[1] &
                                                    readfile()$DTG <= input$slider[2])]),
        selected =   unique(readfile()$woNum[which(readfile()$DTG >= input$slider[1] &
                                                     readfile()$DTG <= input$slider[2])][1])
      )
    })
    
    output$ui2 <- renderUI({
      sliderInput(
        "slider",
        "Time",
        min = as.Date(min(readfile()$DTG) - days(1)),
        max = as.Date(max(readfile()$DTG) + days(1)),
        value = c(as.Date(min(readfile()$DTG) - days(1)), as.Date(max(readfile()$DTG) + days(1)))
      )
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

