library(shiny)
library(MASS)
library(png)
library(jpeg)

eu.dist <- function(x1, y1, x2, y2){
    return(sqrt(((x2-x1)^2)+((y2-y1)^2)))
}

getevents <- function(gazedata, timestamp_variable, media_variable){
    allmedia <- unique(gazedata[,media_variable])
    if(length(allmedia) == 1){
        mediatime <- min(gazedata[gazedata[,media_variable] == allmedia[1], timestamp_variable])
        return(gazedata[gazedata[, timestamp_variable] == mediatime, c(timestamp_variable, media_variable)])
    }
    else if(length(allmedia) > 1){
        stimChange <- c(0, diff(as.factor(gazedata[,media_variable])))
        return(gazedata[stimChange != 0, c(timestamp_variable, media_variable)])
    }
    else{
        return(NULL)
    }
}

getclicks <- function(input, click_object){
    # x <- input$click_object$x
    print(input$click_object)
}

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
    imageScaleRatio <- reactiveVal(value = 1)
    
    loadImage <- reactive({
        req(input$stimFile)
        extension <- tolower(substr(input$stimFile$datapath, nchar(input$stimFile$datapath)-3, nchar(input$stimFile$datapath)))
        # print(extension)
        if(extension == ".jpg" || extension == "jpeg"){
            originalImage <- readJPEG(input$stimFile$datapath)
        }
        else if(extension == ".png"){
            originalImage <- readPNG(input$stimFile$datapath)
        }
        return(originalImage)
    })
    # output$image <- renderImage({
    #     req(input$stimFile)
    #     # get original image for size and resize
    #     # TODO: get original dimansions once in a reactive function and return size
    #     ### from here
    #     originalImage <- loadImage()
    #     originalDimensions <- dim(originalImage)[c(2,1)] # width [2], height [1]
    #     # client size
    #     viewDimensions = c(session$clientData$output_image_width, session$clientData$output_image_height)
    #     ### till here
    #     
    #     # calculate original and view ratio
    #     origRatio <- originalDimensions[1] / originalDimensions[2]
    #     viewRatio <- viewDimensions[1] / viewDimensions[2]
    #     # print(paste("Original:", origRatio, "View:", viewRatio))
    #     # which side was used to fill canvas (if client is wider, height was used, else width was)
    #     if(round(viewRatio, 2) == round(origRatio, 2) | viewRatio <= 1 | viewRatio == Inf){
    #         # print("width used")
    #         conversionRatio <- viewDimensions[1] / originalDimensions[1]
    #     }
    #     else {
    #         # print("height used")
    #         conversionRatio <- viewDimensions[2] / originalDimensions[2]
    #     }
    #     print(conversionRatio)
    #     imageScaleRatio(conversionRatio)
    #     list(
    #         src = input$stimFile$datapath
    #     )
    # }, deleteFile = F)
    
    output$clickinst <- renderText({
        req(input$stimFile)
        "Click on target positions in order of appearance:"
    })
    
    observeEvent(input$image_click, {
        # newclicks <- rbind(allclicks(), cbind(input$image_click$x / imageScaleRatio(), input$image_click$y / imageScaleRatio()))
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
    
    # TODO: cbind xy clicked target coordinates with start times and stimulus names to help getting correct click order
    # output$clicktext <- renderPrint({
    #     req(input$stimFile)
    #     cat("Clicks:\n")
    #     allclicks()
    # })
    
    output$gazeDataOut <- renderTable({
        req(input$gazeFile)
        head(gazedata())
    })
    
    output$gazePlotOut <- renderPlot({
        req(input$gazeFile)
        req(input$stimFile)
        req(input$mediaVariable)
        plot(1, xlim = c(0, as.numeric(input$screenResolutionW)), ylim = c(as.numeric(input$screenResolutionH), 0), xlab = "X Gaze Coordinates", ylab = "Y Gaze Coordinates")
        # draw image in background
        img <- loadImage()
        rasterImage(img,as.numeric(input$calibStimLeft),as.numeric(input$calibStimTop), as.numeric(input$calibStimLeft) + dim(img)[2],as.numeric(input$calibStimTop) + dim(img)[1])
        # plot raw data
        points(as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]) & !grepl(input$calibStimExclude, gazedata()[,make.names(input$mediaVariable)], ignore.case = T),make.names(input$gazeCoordinatesVariableX)]), as.numeric(gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])  & !grepl(input$calibStimExclude, gazedata()[,make.names(input$mediaVariable)], ignore.case = T),make.names(input$gazeCoordinatesVariableY)]))
        # plot calibrated points
        if(is.element("newx", names(gazedata()))){
            points(gazedata()$newx[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], gazedata()$newy[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)])], col = "red")
        }
        # plot clicks
        if(!is.null(allclicks())){
            points(allclicks()[,1], allclicks()[,2], col = 'blue', pch = 15)
        }
    })
    
    observeEvent(input$calibrate, {
        req(input$gazeFile)
        req(input$mediaVariable)
        targets <- allclicks()
        times <- getTimes()
        targetsX <- targets[,1]
        targetsY <- targets[,2]
        if(!is.null(input$mediaVariable)){
            tempdata <- gazedata()[grepl(substr(input$stimFile$name, 1,nchar(input$stimFile$name) - 4), gazedata()[,make.names(input$mediaVariable)]), ]
            
            # check which type of verifiation stimulus is used
            if(input$calibStimType == "Single"){
                for(thisstart in calibstimstart()){
                    # prep data for regression
                    for(ii in 1:length(targetsX)){
                        tempdata$target_x[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[ii] + thisstart & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[ii] + thisstart + as.numeric(input$targetDuration)] <- targetsX[ii]
                        tempdata$target_y[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[ii] + thisstart & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[ii] + thisstart + as.numeric(input$targetDuration)] <- targetsY[ii]
                    }
                }
            }
            else if(input$calibStimType == "Multiple"){
                for(ii in 1:length(calibstimstart())){
                    tempdata$target_x[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[1] + calibstimstart()[ii] & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[1] + calibstimstart()[ii] + as.numeric(input$targetDuration)] <- targetsX[ii]
                    tempdata$target_y[as.numeric(tempdata[, make.names(input$timestampVariable)]) >= times[1] + calibstimstart()[ii] & as.numeric(tempdata[, make.names(input$timestampVariable)]) <= times[1] + calibstimstart()[ii] + as.numeric(input$targetDuration)] <- targetsY[ii]
                }
            }
            
            # prepare data to train regression
            tempdata2 <- tempdata[!is.na(tempdata$target_x) & eu.dist(as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableX)]), as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableY)]), tempdata$target_x, tempdata$target_y) <= as.numeric(input$distanceThreshold),]
            true_x <- tempdata$target_x
            test_x <- as.numeric(tempdata[,make.names(input$gazeCoordinatesVariableX)])
            # TODO: add trycatch
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