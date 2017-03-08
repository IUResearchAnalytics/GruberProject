library(shiny)
library(shinyTime)
library(ggplot2)
library(plyr)
library(data.table)
library(lubridate)
library(scales)
library(RMySQL)
library(shinyTable)

setwd("~/Fitbit_project/Code")

config <- read.table('rshiny.cnf.txt')
# 
mydb <- dbConnect(MySQL(), user=as.character(config[1,2]), password=as.character(config[2,2]),
                  dbname=as.character(config[3,2]), host=as.character(config[4,2]),
                  port = as.integer(as.character(config[5,2])))

# qualdata <- dbGetQuery(conn=mydb, statement = "select * from qualdata order by Id;")

listperson <- dbGetQuery(conn = mydb, statement = "SELECT DISTINCT(ID) FROM intens;")
numPerson <- dbGetQuery(conn = mydb, statement = "SELECT COUNT(DISTINCT(ID)) FROM intens;")
dates <- list()
dates[1] <- dbGetQuery(conn = mydb, statement = "SELECT in_time FROM intens ORDER BY in_time ASC LIMIT 1;")
dates[2] <- dbGetQuery(conn = mydb, statement = "SELECT in_time FROM intens ORDER BY in_time DESC LIMIT 1;")


# mydata <- dbGetQuery(conn = mydb, statement = "SELECT ID, in_time, DATE_FORMAT(in_time, '%Y-%m-%d') as date, intensity 
#                                                 FROM intens;")

# mydata$in_time <-as.POSIXct(mydata$in_time)
# mydata$date <- as.POSIXct(mydata$date)


# Creat dates and date ranges
ui <- pageWithSidebar(
  
  headerPanel("Fitbit Data Plot (ongoing project)"),
  
  sidebarPanel(
    
    verbatimTextOutput("daterangeText"),
    
    verbatimTextOutput("samplesizeText"),

    dateRangeInput("daterange",
                   label = "Date range input: yyyy-mm-dd",
                   start = as.Date(dates[[1]]), end = as.Date(dates[[1]]) + 2,
                   startview = "year", weekstart = 1                   
                   
    ),
    
    # Create a dropdown list for selecting person who participated in the project
    selectInput("person", "Select Person", choices=listperson),
    
    downloadButton("downloadData", "Download Selected Duration Data")
  ),
  

  
  mainPanel(
    plotOutput("tsplot", height=300, brush = brushOpts(
      id = "plot_brush", resetOnNew = TRUE)),
    
    plotOutput("zoomplot", height = 300),
    
    dataTableOutput("brush_info")
    
  )
  
)


server <- function(input, output, session) {
  
  sqldata <- reactive({
    
    sql <- "SELECT ID, in_time, DATE_FORMAT(in_time, '%Y-%m-%d') as date, intensity 
    FROM intens
    WHERE DATE_FORMAT(in_time, '%Y-%m-%d') BETWEEN ?daterange1 AND ?daterange2
    AND ID = ?person;"    
    
    query <- sqlInterpolate(mydb, sql, daterange1 = as.character(input$daterange[1]),
                            daterange2 = as.character(input$daterange[2]), 
                            person = input$person)
    
    
    dbdata <- dbGetQuery(mydb, query)
    
    dbdata$in_time <-as.POSIXct(dbdata$in_time)
    dbdata$date <- as.POSIXct(dbdata$date)    
    
    return(dbdata)
    
  })
  
  
  output$daterangeText  <- renderText({
    
    paste("The date range of the existing data is", unlist(strsplit(dates[[1]], split=' '))[1], "to", unlist(strsplit(dates[[2]], split=' '))[1], collapse = "")
    
  })
  # 
  output$samplesizeText  <- renderText({
    
    paste("The number of paticipants of the project is", numPerson, collapse = " ")
    
  })
  
  selectedData <- reactive({    
    
    a <- subset(sqldata(), date >= as.Date(input$daterange[1]) & 
                  date <= as.Date(input$daterange[2]) & 
                  ID == input$person)
    
    return(a)
    
  })
  
  
  
  selectedData2 <- reactive({
    
    data3 <- subset(sqldata(), date >= as.Date(input$daterange[1]) & 
                      date <= as.Date(input$daterange[2]) & 
                      ID == input$person)
    
    # If Intensity large than or equal to 2, then Intensity2 = 1, else Intensity2 = 0
    data3$intensity2 <- ifelse(data3$intensity >= 2, 1, 0)
    
    # Apply rle() to find Intensity2 >= 1
    
    selectRow <- rle(data3$intensity2 >= 1)
    
    # Find indices of the selectRow with length of at least 1
    index <-which(selectRow$values == TRUE & selectRow$lengths >= 1)
    
    # Check if selectRow has any value in it
    any(index)
    
    # Do a communitive sum of the selectRow lengths and extract the end positions of the selectRow with length of at least 1
    # using the above found indices
    selectRow_lengths_cumsum <- cumsum(selectRow$lengths)
    
    ends <- selectRow_lengths_cumsum[index]
    
    # Find the start position of these selectRow
    newindex <- ifelse(index > 1, index -  1, 0)
    starts <- selectRow_lengths_cumsum[newindex] + 1
    if (0 %in% newindex) starts = c(1,starts)
    
    duration <- as.numeric(difftime(data3$in_time[ends], data3$in_time[starts])) / 60
    
    # Calculate average intensity per activiity
    avg_intens <- rep(0, length(starts))
    
    for (i in 1:length(starts)){
      avg_intens[i] <- sum(data3$intensity[starts[i]:ends[i]])/(ends[i]-starts[i]+1)
    }
    
    # Extrac data with the number of starts
    data4 <- data3[starts, ]
    
    EndTime <- data3$in_time[ends]
    
    data6 <- cbind(data4, EndTime, duration, avg_intens)
    
    data7 <- data6
    
    data7$class[data7$duration <= 15] <- "duration <= 15"
    data7$class[data7$duration > 15 & data7$duration <= 30] <- "15 < duration <= 30"
    data7$class[data7$duration > 30 & data7$duration <= 60] <- "30 < duration <= 60"
    data7$class[data7$duration > 60 & data7$duration <= 90] <- "60 < duration <= 90"
    data7$class[data7$duration > 90] <- "duration > 90"
    
    
    data7$classnum[data7$duration <= 15] <- 5
    data7$classnum[data7$duration > 15 & data7$duration <= 30] <- 5
    data7$classnum[data7$duration > 30 & data7$duration <= 60] <- 5
    data7$classnum[data7$duration > 60 & data7$duration <= 90] <- 5
    data7$classnum[data7$duration > 90] <- 5
    
    # colnames(data7)[3] <- "start_time"
    
    return(data7)
    
    
  })
  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  

  
  output$tsplot <- renderPlot({
    
    ggplot() +
      geom_area(data=selectedData(), aes(x=in_time, y=intensity)) + xlab("Time") +
      geom_point(data = selectedData2(), aes(x=in_time, y=classnum, shape=class, colour=class))

  })
  
  output$zoomplot <- renderPlot({
    
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
    
    ggplot() +
      geom_area(data=selectedData(), aes(x=in_time, y=intensity)) + xlab("Time") +
      geom_point(data = selectedData2(), aes(x=in_time, y=classnum, shape=class, colour=class)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    
  })
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  output$brush_info <- renderDataTable(brushedPoints(selectedData2()[,c("in_time", "EndTime", "intensity", "duration", 
                                                                    "avg_intens")], input$plot_brush))
  

  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('Intens_Duration_ID',input$person,'_',input$daterange[1],'to',input$daterange[2],'.csv', sep='') },
    content = function(file) {
      write.csv(selectedData2()[,c("ID", "in_time","EndTime", "duration","avg_intens")], file, row.names = FALSE)
    }
  )



 }


shinyApp(ui = ui, server = server)
