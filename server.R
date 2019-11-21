library(shiny)
library(MASS)
library(png)
library(jpeg)
library(DT)
source("calibrationValidation.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # increase maximum file size
    options(shiny.maxRequestSize=80*1024^2)
    # set tracker-specific variable names
    output$gazeCoordinatesVariableX <- renderUI({
        if(input$eyetracker == "Tobii"){
            gazexVarName <- 'GazePointX (ADCSpx)'
        }
        else if(input$eyetracker == "EyeLink"){
            gazexVarName <- 'RIGHT_GAZE_X'
        }
        else if(input$eyetracker == "SMI"){
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
            gazeyVarName <- 'Point of Regard Left Y [px]'
        }
        textInput('gazeCoordinatesVariableY', label = 'Y Gaze Coordinate Variable', value = gazeyVarName)
    })
    output$timestampVariable <- renderUI({
        if(input$eyetracker == "Tobii"){
            tsVarName <- 'RecordingTimestamp'
        }
        else if(input$eyetracker == "EyeLink"){
            tsVarName <- 'TIMESTAMP'
        }
        else if(input$eyetracker == "SMI"){
            tsVarName <- 'RecordingTime [ms]'
        }
        textInput('timestampVariable', label = 'Timestamp Variable', value = tsVarName)
    })
    output$mediaVariable <- renderUI({
        if(input$eyetracker == "Tobii"){
            mediaVarName <- 'MediaName'
        }
        else if(input$eyetracker == "EyeLink"){
            mediaVarName <- 'VIDEO_NAME'
        }
        else if(input$eyetracker == "SMI"){
            mediaVarName <- 'Stimulus'
        }
        textInput('mediaVariable', label = 'Media Name Variable', value = mediaVarName)
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
    # get other settings
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
    # prepare "global" variables for clicks and gaze data
    allclicks <- reactiveVal()
    gazedata <- reactiveVal()
    calibMetrics <- reactiveVal()
    imageScaleRatio <- reactiveVal(value = 1)
    
    loadImage <- reactive({
        req(input$stimFile)
        extension <- tolower(substr(input$stimFile$datapath, nchar(input$stimFile$datapath)-3, nchar(input$stimFile$datapath)))
        if(extension == ".jpg" || extension == "jpeg"){
            originalImage <- readJPEG(input$stimFile$datapath)
        }
        else if(extension == ".png"){
            originalImage <- readPNG(input$stimFile$datapath)
        }
        return(originalImage)
    })
    
    output$clickinst <- renderText({
        req(input$stimFile)
        "Click on target positions in order of appearance:"
    })
    
    observeEvent(input$image_click, {
        newclicks <- rbind(allclicks(), cbind(input$image_click$x, input$image_click$y))
        allclicks(newclicks)
    })
    
    observeEvent(input$gazeFile, {
        gazedata(read.table(input$gazeFile$datapath, sep = getSep(), header = T, dec = getDec(), as.is = T, stringsAsFactors = F, na.strings = input$nastring))
    })
    
    # get media start time
    calibstimstart <- reactive({
        req(input$gazeFile)
        req(input$stimFile)
        allEvents <- getevents(gazedata(), make.names(input$timestampVariable), make.names(input$mediaVariable))
        stimstart <- as.numeric(allEvents[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), allEvents[,make.names(input$mediaVariable)]) & !grepl(input$calibStimExclude, allEvents[,make.names(input$mediaVariable)], ignore.case = T),make.names(input$timestampVariable)])
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
    
    output$gazeDataOut <- renderDataTable({
        req(input$gazeFile)
        datatable(head(gazedata()), options = list(paging = FALSE, searching = FALSE, scrollX = TRUE))
    })
    
    output$gazePlotOut <- renderPlot({
        req(input$gazeFile)
        req(input$stimFile)
        req(input$mediaVariable)
        plot(1, xlim = c(0, as.numeric(input$screenResolutionW)), ylim = c(as.numeric(input$screenResolutionH), 0), xlab = "X Gaze Coordinates", ylab = "Y Gaze Coordinates", type = 'n')
        # draw image in background
        img <- loadImage()
        rasterImage(img,as.numeric(input$calibStimLeft),as.numeric(input$calibStimTop), as.numeric(input$calibStimLeft) + dim(img)[2],as.numeric(input$calibStimTop) + dim(img)[1])
        # plot raw data
        points(as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]) & !grepl(input$calibStimExclude, gazedata()[,make.names(input$mediaVariable)], ignore.case = T),make.names(input$gazeCoordinatesVariableX)]), as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])  & !grepl(input$calibStimExclude, gazedata()[,make.names(input$mediaVariable)], ignore.case = T),make.names(input$gazeCoordinatesVariableY)]), col = "gray", pch = 20)
        # plot calibrated points
        if(is.element("newx", names(gazedata()))){
            points(gazedata()$newx[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], gazedata()$newy[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], col = "red", pch = 18)
        }
        # plot clicks
        if(!is.null(allclicks())){
            points(allclicks()[,1], allclicks()[,2], col = 'blue', pch = 15)
        }
    })
    
    observeEvent(input$calibrate, {
        req(input$gazeFile)
        req(input$stimFile)
        req(input$mediaVariable)
        targets <- allclicks()
        times <- getTimes()
        targetsX <- targets[,1]
        targetsY <- targets[,2]
        if(!is.null(input$mediaVariable)){
            # add target positions
            gazedata(addTargets(gazedata = gazedata(), stimstart = calibstimstart(), stimtype = input$calibStimType, targets = targets, tsvariable = input$timestampVariable, stimvariable, stimtimes = times, targetduration = input$targetDuration))
            # run calibration validation
            gazedata(validateCalibrate(gazedata = gazedata(), calib_media_name = input$stimFile$name, media_variable = input$mediaVariable, gaze_point_x_variable = input$gazeCoordinatesVariableX, gaze_point_y_variable = input$gazeCoordinatesVariableY, max_distance = input$distanceThreshold))
            # calculate calibration metrics
            calibMetrics(metrics(gazedata = gazedata(), targets = targets, xcoordsvar = input$gazeCoordinatesVariableX, ycoordsvar = input$gazeCoordinatesVariableY))
        }
    })
    
    output$calibmetrics <- renderDataTable({
        req(input$gazeFile)
        datatable(calibMetrics(), options = list(paging = FALSE, searching = FALSE, scrollX = TRUE))
    })
    
    # Downloadable csv of gaze dataset
    output$newGazeFile <- downloadHandler(
        filename = function() {
            paste("calibrated_",input$gazeFile$name, sep = "")
        },
        content = function(file) {
            write.table(gazedata(), file, sep = getSep(), dec = getDec(), row.names = FALSE)
        }
    )
    
    # Downloadable csv of calibration metrics
    output$downloadMetrics <- downloadHandler(
        filename = function() {
            paste("metrics_",input$gazeFile$name, sep = "")
        },
        content = function(file) {
            write.table(calibMetrics(), file, sep = getSep(), dec = getDec(), row.names = FALSE)
        }
    )
    
}