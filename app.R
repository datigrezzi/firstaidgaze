# a modular Shiny app to do a post-hoc calibration on gaze data
# Iyad Aldaqre
# 13.05.2019

# Tab 1: Load, Calibrate, Download
# load gaze data and stimulus image
# choose calibration stimulus (automatically from image name)
# train regression and apply to data
# download data (temporary here)

# sidebar: upload data, image, settings button, submit and download
# main: data header (rows in settings), stimz

# Tab 2: Settings
# Choose what to do with NAs
# Apply running median filter for noise cancellation
# Choose which eye to use (create new variables for either left, right, average or strict average)
# download data

library(shiny)
library(shinycssloaders)
library(MASS)

eu.dist <- function(x1, y1, x2, y2){
  return(sqrt(((x2-x1)^2)+((y2-y1)^2)))
}

getevents <- function(gazedata, timestamp_variable, media_variable){
  stimChange <- c(0, diff(as.factor(gazedata[,media_variable])))
  return(gazedata[stimChange != 0, c(timestamp_variable, media_variable)])
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Calibration Validation Tool"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('eyetracker', 'Eye Tracker', c("Tobii", "SMI", "EyeLink")),
      fileInput("gazeFile", "Upload Gaze File",  accept = c("text/csv", "text/tsv", "text/txt")),
      fileInput("stimFile", "Upload Stimulus Image",  accept = c('image/png', 'image/jpeg','image/jpg')),
      textInput("targetTimes", "Targets Onset:", value = "1200; 4200; 7200; 10200"),
      actionButton("calibrate", "Start Calibration"),
      br(),
      br(),
      downloadButton("newGazeFile", label = "Download")
    ),

    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      id = "tabs",
      tabPanel(title = "Start",
               tags$head(tags$style(
                 type="text/css",
                 "#image img {max-width: 100%}"
               )),
               br(),
               textOutput("clickinst"),
               br(),
               imageOutput("image", width = "auto", height = "auto",
                           click = "image_click"
               ),
               verbatimTextOutput("clicktext"),
               verbatimTextOutput("timetext")
               ),
      tabPanel(title = "Verify",
               withSpinner(tableOutput("gazeDataOut")),
               withSpinner(plotOutput("gazePlotOut"))
               ),
      tabPanel(title = "Settings",
               br(),
               fluidRow(
                 column(
                   4,
                   uiOutput('gazeCoordinatesVariableX'),
                   uiOutput('gazeCoordinatesVariableY')#, 'X Gaze Coordinate Variable', 'GazePointX (ADCSpx)')
                 ),
                 column(
                   4,
                   uiOutput('timestampVariable'), #, 'Timestamp variable', 'RecordingTimestamp'),
                   uiOutput('mediaVariable') #, 'Stimulus Name Variable', 'MediaName')
                 ),
                 column(
                   4,
                   textInput('distanceThreshold', 'Max Point Distance', '400'),
                   textInput('targetDuration', 'Target Duration (ms)', '1000')
                 )
               ),
               fluidRow(
                 column(
                   2,
                   selectInput('separator', 'Sep', c('TAB', 'Comma', 'Semicolon'))
                 ),
                 column(
                   2,
                   selectInput('decimal', 'Dec', c('Point', 'Comma'))
                 ),
                 column(
                   2,
                   textInput('screenResolutionW', 'Width', '1280')
                 ),
                 column(
                   2,
                   textInput('screenResolutionH', 'Height', '1024')
                 ),
                 column(
                   2,
                   textInput('calibStimTop', 'Top', '224')
                 ),
                 column(
                   2,
                   textInput('calibStimLeft', 'Left', '280')
                 )
               ),
               fluidRow(
                 column(
                   2,
                   uiOutput('nastring') # textInput('nastring', 'NA String', "NA")
                 )
               )
               ),
      tabPanel(title = "Help",
               br(),
               h4("Coming soon..."))
    ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  
  output$gazeCoordinatesVariableX <- renderUI({
    if(input$eyetracker == "Tobii"){
      gazexVarName <- 'GazePointX (ADCSpx)'
    }
    else if(input$eyetracker == "EyeLink"){
      gazexVarName <- 'RIGHT_GAZE_X'
    }
    else if(input$eyetracker == "SMI"){
      # TODO: CHANGE
      gazexVarName <- 'Point of Regard Left X [px]'
    }
    textInput('gazeCoordinatesVariableX', label = 'X Gaze Coordinate Variable', value = gazexVarName)
  })
  
  output$gazeCoordinatesVariableY <- renderUI({
    if(input$eyetracker == "Tobii"){
      gazeyVarName <- 'GazePointY (ADCSpx)'
    }
    else if(input$eyetracker == "EyeLink"){
      gazeyVarName <- 'RIGHT_GAZE_Y'
    }
    else if(input$eyetracker == "SMI"){
      # TODO: CHANGE
      gazeyVarName <- 'Point of Regard Left Y [px]'
    }
    textInput('gazeCoordinatesVariableY', label = 'X Gaze Coordinate Variable', value = gazeyVarName)
  })
  
  output$timestampVariable <- renderUI({
    if(input$eyetracker == "Tobii"){
      tsVarName <- 'RecordingTimestamp'
    }
    else if(input$eyetracker == "EyeLink"){
      tsVarName <- 'TIMESTAMP'
    }
    else if(input$eyetracker == "SMI"){
      # TODO: CHANGE
      tsVarName <- 'RecordingTime [ms]'
    }
    textInput('timestampVariable', label = 'X Gaze Coordinate Variable', value = tsVarName)
  })
  
  output$mediaVariable <- renderUI({
    if(input$eyetracker == "Tobii"){
      mediaVarName <- 'MediaName'
    }
    else if(input$eyetracker == "EyeLink"){
      mediaVarName <- 'VIDEO_NAME'
    }
    else if(input$eyetracker == "SMI"){
      # TODO: CHANGE
      mediaVarName <- 'Stimulus'
    }
    textInput('mediaVariable', label = 'X Gaze Coordinate Variable', value = mediaVarName)
  })
  output$nastring <- renderUI({
    if(input$eyetracker == "Tobii"){
      nastr <- 'NA'
    }
    else if(input$eyetracker == "EyeLink"){
      nastr <- '.'
    }
    else if(input$eyetracker == "SMI"){
      nastr <- '-'
    }
    textInput('nastring', 'NA String', nastr)
  })
  
  getSep <- reactive({
    if(input$separator == 'TAB'){
      return("\t")
    }
    else if(input$separator == 'Comma'){
      return(',')
    }
    else if(input$separator == 'Semicolon'){
      return(';')
    }
  })
  
  getDec <- reactive({
    if(input$decimal == 'Comma'){
      return(',')
    }
    else if(input$decimal == 'Point'){
      return('.')
    }
  })
  
  allclicks <- reactiveVal()
  gazedata <- reactiveVal()
  
  output$image <- renderImage({
    req(input$stimFile)
    # Return a list
    list(src = input$stimFile$datapath)
  }, deleteFile = F)
  
  output$clickinst <- renderText({
    req(input$stimFile)
    "Click on target positions in order of appearance:"
  })
  
  observeEvent(input$image_click, {
    newclicks <- rbind(allclicks(), cbind(input$image_click$x, input$image_click$y))
    allclicks(newclicks)
  })
  
  observeEvent(input$gazeFile, {
    # TODO> add try, catch with NULL
    gazedata(read.table(input$gazeFile$datapath, sep = getSep(), header = T, dec = getDec(), as.is = T, stringsAsFactors = F, na.strings = input$nastring))
  })
  
  # get media start time
  calibstimstart <- reactive({
    req(input$gazeFile)
    req(input$stimFile)
    allEvents <- getevents(gazedata(), make.names(input$timestampVariable), make.names(input$mediaVariable))
    stimstart <- as.numeric(allEvents[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), allEvents[,make.names(input$mediaVariable)]),make.names(input$timestampVariable)])
    return(stimstart)
  })
  
  getTimes <- reactive({
    fullstring <- input$targetTimes
    if(nchar(fullstring) > 0){
      times <- as.numeric(unlist(strsplit(sub(" ", "", fullstring), ";")))
      times <- times[!is.na(times)]
      return(times)
    }
  })
  
  output$clicktext <- renderPrint({
    req(input$stimFile)
    cat("Clicks:\n")
    allclicks()
  })
  
  output$timetext <- renderPrint({
    req(input$stimFile)
    cat("Times:\n")
    getTimes()
  })
  
  output$gazeDataOut <- renderTable({
    req(input$gazeFile)
    head(gazedata())
  })
  
  output$gazePlotOut <- renderPlot({
    req(input$gazeFile)
    req(input$stimFile)
    req(input$mediaVariable)
    plot(as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]),make.names(input$gazeCoordinatesVariableX)]), as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]),make.names(input$gazeCoordinatesVariableY)]), xlim = c(0, as.numeric(input$screenResolutionW)), ylim = c(as.numeric(input$screenResolutionH), 0), xlab = "X Gaze Coordinates", ylab = "Y Gaze Coordinates")
    if(is.element("newx", names(gazedata()))){
      points(gazedata()$newx[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], gazedata()$newy[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], col = "red")
    }
    if(!is.null(allclicks())){
      points(allclicks()[,1] + as.numeric(input$calibStimLeft), allclicks()[,2] + as.numeric(input$calibStimTop), col = 'blue', pch = 15)
    }
  })
  
  observeEvent(input$calibrate, {
    req(input$gazeFile)
    req(input$mediaVariable)
    targets <- allclicks()
    times <- getTimes()
    targetsX <- targets[,1] + as.numeric(input$calibStimLeft)
    targetsY <- targets[,2] + as.numeric(input$calibStimTop)
    if(!is.null(input$mediaVariable)){
      tempdata <- gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]), ]
      
      for(thisstart in calibstimstart()){
        # prep data for regression
        for(ii in 1:length(targetsX)){
          tempdata$target_x[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[ii] + thisstart & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[ii] + thisstart + as.numeric(input$targetDuration)] <- targetsX[ii]
          tempdata$target_y[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[ii] + thisstart & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[ii] + thisstart + as.numeric(input$targetDuration)] <- targetsY[ii]
        }
      }
      tempdata <- tempdata[!is.na(tempdata$target_x) & eu.dist(as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableX)]), as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableY)]), tempdata$target_x, tempdata$target_y) <= as.numeric(input$distanceThreshold),]
      true_x <- tempdata$target_x
      test_x <- as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableX)])
      x_model <- rlm(true_x ~ test_x, psi = psi.bisquare)
      true_y <- tempdata$target_y
      test_y <- as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableY)])
      y_model <- rlm(true_y ~ test_y, psi = psi.bisquare)
      
      newx <- predict(x_model, newdata = data.frame(test_x = as.numeric(gazedata()[,make.names(input$gazeCoordinatesVariableX)])))
      newy <- predict(y_model, newdata = data.frame(test_y = as.numeric(gazedata()[,make.names(input$gazeCoordinatesVariableY)])))
      if(is.element("newx", names(gazedata()))){
        gazedata(subset(gazedata(), select=-c(newx,newy)))
      }
      gazedata(cbind(gazedata(), newx, newy))
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$newGazeFile <- downloadHandler(
    filename = function() {
      paste("calibrated_",input$gazeFile$name, sep = "")
    },
    content = function(file) {
      write.table(gazedata(), file, sep = getSep(), dec = getDec(), row.names = FALSE)
    }
  )
  
}
# TODO: download data, dynamic sep
# Run the application
shinyApp(ui = ui, server = server)
