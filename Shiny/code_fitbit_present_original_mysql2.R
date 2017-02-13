# library(shiny)
# library(shinyTime)
# library(ggplot2)
# library(plyr)
# library(data.table)
# library(lubridate)
# library(scales)
# library(RMySQL)

config <- read.table('rshiny.cnf.txt')

mydb <- dbConnect(MySQL(), user=as.character(config[1,2]), password=as.character(config[2,2]),
                  dbname=as.character(config[3,2]), host=as.character(config[4,2]),
                  port = as.integer(as.character(config[5,2])))

# mydata <- dbReadTable(mydb, name = "intens", value = as.data.frame(dbdata))
# 
# 
# mydata2 <- dbGetQuery(conn = mydb, statement = "SELECT ID, in_time, DATE_FORMAT(in_time, '%Y-%m-%d') as date, intensity 
#                                                 FROM intens
#                    WHERE DATE_FORMAT(in_time, '%Y-%m-%d') BETWEEN '2016-12-18' AND '2016-12-19'
#                    AND ID = 46;")
# 
# mydata2$in_time <-as.POSIXct(mydata2$in_time)
# mydata2$date <- as.POSIXct(mydata2$date)


# Creat dates and date ranges
ui <- pageWithSidebar(
  
  headerPanel("Fitbit Data Plot (ongoing project)"),
  
  sidebarPanel(
    
    verbatimTextOutput("daterangeText"),
    
    verbatimTextOutput("samplesizeText"),
    
    dateRangeInput("daterange",
                   label = "Date range input: yyyy-mm-dd",
                   start = "2016-12-18", end = "2016-12-19",
                   startview = "year", weekstart = 1
    ),
    
    # Create a dropdown list for selecting person who participated in the project
    selectInput("person", "Select Person", seq(60))
    
  ),
  
  mainPanel(
    plotOutput("tsplot", height=300, brush = brushOpts(
      id = "plot_brush", resetOnNew = TRUE)),
    # verbatimTextOutput("click_info"),
    # verbatimTextOutput("brush_info"),
    
    plotOutput("zoomplot", height = 300)
    
  )
  
)


server <- function(input, output) {
  
  data <- dbGetQuery(conn = mydb, statement = "SELECT ID, in_time, DATE_FORMAT(in_time, '%Y-%m-%d') as date, intensity 
                                                FROM intens
                                                WHERE DATE_FORMAT(in_time, '%Y-%m-%d') BETWEEN 'input$daterange[1]' AND 'input$daterange[2]'
                                                      AND ID = 46;")
  
  data$in_time <-as.POSIXct(data$in_time)
  data$date <- as.POSIXct(data$date)
  
  
  # output$daterangeText  <- renderText({
  #   
  #   paste("The date range of the existing data is", as.character(min(data$date)), "to", as.character(max(data$date)), collapse = "")
  #   
  # })
  # 
  # output$samplesizeText  <- renderText({
  #   
  #   paste("The number of paticipants of the project is", length(unique(data$ID)), collapse = " ")
  #   
  # })
  
  
  output$dateRangeText <- renderText({
    paste("Input date range is",
          paste(as.character(input$dateRange), collapse = " to "))
  })
  

  
  selectedData <- reactive({
    
    dateseq <- seq(input$daterange[1], input$daterange[2], by="day" )
    
    a <- subset(data, date %in% dateseq & ID == input$person)
    
    return(a)
    
  })
  
  
  
  selectedData2 <- reactive({
    
    dateseq2 <- seq(input$daterange[1], input$daterange[2], by="day" )
    
    data3 <- subset(data, date %in% dateseq2 & ID == input$person)
    
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
    
    middle <- (starts + ends) /2
    
    duration <- as.numeric(difftime(data3$in_time[ends], data3$in_time[starts])) / 60
    
    # Extrac data with the number of starts
    data4 <- data3[starts, ]
    
    data5 <- data3[ends, ]
    
    data6 <- cbind(data4, duration)
    
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
    
    return(data7)
    
    
  })
  

  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$tsplot <- renderPlot({
    
    ggplot() +
      geom_area(data=selectedData(), aes(x=in_time, y=intensity)) +
      geom_point(data = selectedData2(), aes(x=in_time, y=classnum, shape=class, colour=class))

  })
  
  output$zoomplot <- renderPlot({
    
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }

    ggplot() +
      geom_area(data=selectedData(), aes(x=in_time, y=intensity)) +
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
  

}


shinyApp(ui = ui, server = server)