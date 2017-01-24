library(shiny)
library(shinyTime)
library(ggplot2)
library(plyr)
library(data.table)
library(lubridate)
library(scales)



# # Read in fitbit data
data1 <- read.csv("Data/001_minuteIntensitiesNarrow_20151116_20161101.csv")
# head(data)

# Copy data1
data2 <- data1

# # Add the person name column to the data
data1$PersonName <- "Person 1"

data2$PersonName <- "Person 2"

data3 <- rbind(data1, data2)


# Calculate the time duration of Intensity in secs
data3$format_time <- mdy_hms(data3$ActivityMinute)

data3$time <- as.character(data3$format_time)
data3$date <- date(data3$format_time)

data3$category[data3$Intensity == 0] <- "Intensity is 0"
data3$category[data3$Intensity == 1] <- "Intensity is 1"
data3$category[data3$Intensity == 2] <- "Intensity is 2"
data3$category[data3$Intensity == 3] <- "Intensity is 3"



# If Intensity large than or equal to 2, then Intensity2 = 1, else Intensity2 = 0
data3$Intensity2 <- ifelse(data3$Intensity >= 2, 1, 0)

splitdata <- split(data3, with(data3, interaction(PersonName)), drop = TRUE)

# Apply rle() to find Intensity2 >= 1

selectRow <- lapply(splitdata, function(x) {rle(x$Intensity2 >= 1)})

# Find indices of the selectRow with length of at least 1
index <- lapply(selectRow, function(x) {which(x$values == TRUE & x$lengths >= 1)})

# Check if selectRow has any value in it
lapply(index, function(x) {any(x)})

# Do a communitive sum of the selectRow lengths and extract the end positions of the selectRow with length of at least 1
# using the above found indices
selectRow_lengths_cumsum <- lapply(selectRow, function(x) {cumsum(x$lengths)})

ends <- mapply(function(x,y) x[y], x=selectRow_lengths_cumsum, y=index, SIMPLIFY = FALSE)

# Find the start position of these selectRow
newindex <- lapply(index, function(x) {ifelse(x > 1, x -  1, 0)})
starts <- mapply(function(x,y) x[y] + 1, x=selectRow_lengths_cumsum, y=newindex, SIMPLIFY = FALSE)

starts <- mapply(function(x,y) if ( 0 %in% x) {y = c(1, y)} else {y = y}, x=newindex, y=starts, SIMPLIFY = FALSE)


middle <- mapply(function(x,y) (x+y)/2, x=starts, y=ends, SIMPLIFY = FALSE)

duration <- mapply(function(x,y)  as.numeric(difftime(data3$format_time[y], data3$format_time[x])) / 60, x=starts,
                   y=ends, SIMPLIFY = FALSE)





# Extrac data with the number of starts
data4 <- mapply(function(x,y) x[y, ], x=splitdata, y=starts, SIMPLIFY = FALSE)

data5 <- mapply(function(x,y) x[y, ], x=splitdata, y=ends, SIMPLIFY = FALSE)

data6 <- mapply(function(x,y) cbind(x, y), x=data4, y=duration, SIMPLIFY = FALSE)

colname <- colnames(data6[[1]])
new_col_name <- c(colname[-9], "duration")
data6 <- lapply(data6, setNames, nm = new_col_name)

data7 <- do.call("rbind", data6)

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


# Creat dates and date ranges
ui <- pageWithSidebar(
  
  headerPanel("Fitbit Data Plot (ongoing project)"),
  
  sidebarPanel(
    
    verbatimTextOutput("daterangeText"),
    
    verbatimTextOutput("samplesizeText"),
    
    dateRangeInput("daterange",
                   label = "Date range input: yyyy-mm-dd",
                   start = data3$date[1], end = data3$date[1] + 1,
                   startview = "year", weekstart = 1
    ),
    
    # Create a dropdown list for selecting person who participated in the project
    selectInput("person", "Select Person", unique(data3$PersonName))
    
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
  
  output$daterangeText  <- renderText({
    
    paste("The date range of the existing data is", as.character(min(data3$date)), "to", as.character(max(data3$date)), collapse = "")
          
  })
  
  output$samplesizeText  <- renderText({
    
    paste("The number of paticipants of the project is", length(unique(data3$PersonName)), collapse = " ")
    
  })
  
  output$dateRangeText <- renderText({
    paste("Input date range is",
          paste(as.character(input$dateRange), collapse = " to "))
  })
  
  # updateSelectizeInput(session, "person", choices = unique(data3$PersonName))
  
  selectedData <- reactive({
    
    dateseq <- seq(input$daterange[1], input$daterange[2], by="day" )
    
    a <- subset(data3, date %in% dateseq & PersonName == input$person)
    
    return(a)
    
  })
  
  selectedData2 <- reactive({
    
    dateseq2 <- seq(input$daterange[1], input$daterange[2], by="day" )
    
    b <- subset(data7, date %in% dateseq2 & PersonName == input$person)
    
    return(b)
  })
  
  
  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$tsplot <- renderPlot({
    
    ggplot() +
      geom_area(data=selectedData(), aes(x=format_time, y=Intensity)) +
      geom_point(data = selectedData2(), aes(x=format_time, y=classnum, shape=class, colour=class))
    
    
    # ggplot(selectedData(), aes(format_time, Intensity, fill = category, colour = category)) + 
    #   geom_area() + 
    #   scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("2 hours")) +
    #   # scale_fill_manual(values = c("green", "yellow", "blue", "red")) +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #   ggtitle("Above plot controls below plot") +
    #   geom_point(data = selectedData2(), aes(x=format_time, y=class))
    
  })
  
  output$zoomplot <- renderPlot({
    
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
    
    # ggplot(selectedData(), aes(format_time, Intensity, fill = category, colour = category)) +
    #   geom_area() +
    #   scale_x_datetime(breaks = date_breaks("2 hours"), minor_breaks = date_breaks("1 hour"),labels = date_format("%k")) +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #   coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    
    ggplot() +
      geom_area(data=selectedData(), aes(x=format_time, y=Intensity)) +
      geom_point(data = selectedData2(), aes(x=format_time, y=classnum, shape=class, colour=class)) +
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
  
  
  # output$click_info <- renderPrint({
  #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
  #   # were a base graphics plot, we'd need those.
  #   nearPoints(selectedData()[,c("format_time", "Intensity")], input$plot_click, addDist = FALSE)
  # })
  # 
  # output$brush_info <- renderPrint({
  #   brushedPoints(selectedData()[,c("format_time", "Intensity")], input$plot_brush)
  # })
  
}


shinyApp(ui = ui, server = server)


