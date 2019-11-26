require(MASS)

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

getCalibStimStart <- function(events, calib_stim_name = input$stimFile$name, media_variable = input$mediaVariable, tsvariable = input$timestampVariable, excluded_stim = input$calibStimExclude){
    return(as.numeric(events[grepl(substr(calib_stim_name, 1,nchar(calib_stim_name) - 4), events[,make.names(media_variable)]) & !grepl(excluded_stim, events[,make.names(media_variable)], ignore.case = T),make.names(tsvariable)]))
}

addTargets <- function(gazedata, stimstart, stimtype, targets, tsvariable, stimvariable, stimtimes, targetduration){
    # remove targets if they were assigned before
    if(is.element("target_x", names(gazedata))){
        gazedata <- subset(gazedata, select=-c(target_x,target_y))
    }
    # check type of calibration stimulus
    if(stimtype == "Single"){
        for(thisstart in stimstart){
            for(ii in 1:length(targets[,1])){
                gazedata$target_x[as.numeric(gazedata[, make.names(tsvariable)]) >= stimtimes[ii] + thisstart & as.numeric(gazedata[, make.names(tsvariable)]) <= stimtimes[ii] + thisstart + as.numeric(targetduration)] <- targets[ii,1]
                gazedata$target_y[as.numeric(gazedata[, make.names(tsvariable)]) >= stimtimes[ii] + thisstart & as.numeric(gazedata[, make.names(tsvariable)]) <= stimtimes[ii] + thisstart + as.numeric(targetduration)] <- targets[ii,2]
            }
        }
    }
    else if(stimtype == "Multiple"){
        for(ii in 1:length(stimstart)){
            gazedata$target_x[as.numeric(gazedata[, make.names(tsvariable)]) >= stimtimes[1] + stimstart[ii] & as.numeric(gazedata[, make.names(tsvariable)]) <= stimtimes[1] + stimstart[ii] + as.numeric(targetduration)] <- targets[ii, 1]
            
            gazedata$target_y[as.numeric(gazedata[, make.names(tsvariable)]) >= stimtimes[1] + stimstart[ii] & as.numeric(gazedata[, make.names(tsvariable)]) <= stimtimes[1] + stimstart[ii] + as.numeric(targetduration)] <- targets[ii, 2]
            
        }
    }
    return(gazedata)
}

validateCalibrate <- function(gazedata = gazedata(), calib_media_name = input$stimFile$name, media_variable = input$mediaVariable, gaze_point_x_variable = input$gazeCoordinatesVariableX, gaze_point_y_variable = input$gazeCoordinatesVariableY, max_distance = input$distanceThreshold){
    # TODO: train data with half target duration, use other half for metrics
    # After calculating metrics training can then be run with all media
    
    # TODO: change to warning
    if(!is.element("target_x", names(gazedata))){
        print("Make sure to run addTargets() before validating calibration!")
    }
    # select only calibration media data, ignoring extension          
    tempdata <- gazedata[grepl(substr(calib_media_name, 1,nchar(calib_media_name) - 4), gazedata[,make.names(media_variable)]), ]
    # prepare data to train regression
    tempdata <- tempdata[!is.na(tempdata$target_x) & eu.dist(as.numeric(tempdata[,make.names(gaze_point_x_variable)]), as.numeric(tempdata[,make.names(gaze_point_y_variable)]), tempdata$target_x, tempdata$target_y) <= as.numeric(max_distance),]
    true_x <- tempdata$target_x
    test_x <- as.numeric(tempdata[,make.names(gaze_point_x_variable)])
    # TODO: add trycatch
    x_model <- rlm(tempdata$target_x ~ test_x, psi = psi.bisquare)
    true_y <- tempdata$target_y
    test_y <- as.numeric(tempdata[,make.names(gaze_point_y_variable)])
    y_model <- rlm(true_y ~ test_y, psi = psi.bisquare)

    newx <- predict(x_model, newdata = data.frame(test_x = as.numeric(gazedata[,make.names(gaze_point_x_variable)])))
    newy <- predict(y_model, newdata = data.frame(test_y = as.numeric(gazedata[,make.names(gaze_point_y_variable)])))
    # remove newx and newy if available to replace with new ones
    if(is.element("newx", names(gazedata))){
        gazedata <- subset(gazedata, select=-c(newx,newy))
    }
    # add updatednewx and newy
    return(cbind(gazedata, newx, newy))
}

### After running validateCalibrate() we can calculate calibration metrics
# TODO: add test media time for testing performance (skipping segment with which regression was trained)
# TODO: optimize loop
metrics <- function(gazedata, targets, xcoordsvar, ycoordsvar){
    xcoordsvar <- make.names(xcoordsvar)
    ycoordsvar <- make.names(ycoordsvar)
    accuracies_original <- NULL
    accuracies_corrected <- NULL
    precisions_original <- NULL
    precisions_corrected <- NULL
    for(i in 1:nrow(targets)){
        idx <- gazedata$target_x == targets[i,1] & !is.na(gazedata$target_x)
        tmp_data <- gazedata[idx,]
        accuracies_original <- c(accuracies_original, mean(eu.dist(tmp_data[,xcoordsvar], tmp_data[,ycoordsvar], tmp_data$target_x, tmp_data$target_y), na.rm = T))
        if(is.element("newx", names(tmp_data))){
            accuracies_corrected <- c(accuracies_corrected, mean(eu.dist(tmp_data$newx, tmp_data$newy, tmp_data$target_x, tmp_data$target_y), na.rm = T))
        }
        tmp_precisions_original <- NULL
        tmp_precisions_corrected <- NULL
        for(ii in 1:nrow(tmp_data)){
            tmp_precisions_original <- c(tmp_precisions_original, eu.dist(tmp_data[ii, xcoordsvar], tmp_data[ii,ycoordsvar], tmp_data[ii+1:nrow(tmp_data), xcoordsvar], tmp_data[ii+1:nrow(tmp_data), ycoordsvar]))
            if(is.element("newx", names(tmp_data))){
                tmp_precisions_corrected <- c(tmp_precisions_corrected, eu.dist(tmp_data$newx[ii], tmp_data$newy[ii], tmp_data$newx[ii+1:nrow(tmp_data)], tmp_data$newy[ii+1:nrow(tmp_data)]))
            }
        }
        precisions_original <- c(precisions_original, mean(tmp_precisions_original, na.rm = T))
        if(is.element("newx", names(tmp_data))){
            precisions_corrected <- c(precisions_corrected, mean(tmp_precisions_corrected, na.rm = T))
        }
    }
    metricsout <- data.frame(accuracies_original, accuracies_corrected, precisions_original, precisions_corrected)
    return(metricsout)
}

if(FALSE){
    # choose gaze data file
    thisfile <- file.choose()
    # these settings are for LMU Babylab data for MB2
    mygazedata <- read.table(thisfile, header=T, sep='\t', dec=".")
    # Get participant list
    participants <- unique(mygazedata$ParticipantName)
    # prepare empty data frame for metrics
    allMetrics <- data.frame()
    for(participant in participants){
        # select participant data
        tmp <- mygazedata[mygazedata$ParticipantName == participant,]
        # get all events for this recording
        theseEvents <- getevents(tmp, timestamp_variable = "RecordingTimestamp", media_variable = "MediaName")
        # get calibration stimulus onsets; this stimulus is the one used for MB2 pilot
        stimstart <- getCalibStimStart(theseEvents, calib_stim_name = "star_calib.jpg", media_variable = "MediaName", tsvariable = "RecordingTimestamp", excluded_stim = "instruction")
        # specific target onsets, relative to calibration stimulus onset; these are specific to MB2
        times <- c(1200, 4200, 7200, 10200)
        # top-left corner of the stimulus; these are specific to LMU Babylab data for MB2
        stimTopLeft <- c(280, 224)
        # set target locations, correcting for stimulus top-left
        targets <- data.frame(posX = c(50, 190, 726, 390) + stimTopLeft[1], posY = c(38, 500, 157, 235) + stimTopLeft[2])
        # add targets positions to data set; here we can choose shorter target duration to split data for train-test
        tmp2 <- addTargets(tmp, stimstart = stimstart, stimtype = "Single", targets = targets, tsvariable = "RecordingTimestamp", stimvariable = "MediaName", stimtimes = times, targetduration = 500)
        # calculate calibration and add new columns with corrected data
        tmp2 <- validateCalibrate(tmp2, calib_media_name = "star_calib.jpg", media_variable = "MediaName", gaze_point_x_variable = "GazePointX (ADCSpx)", gaze_point_y_variable = "GazePointY (ADCSpx)", max_distance = 400)
        # add targets again, skipping the training portion of the data to test the calibration on test data
        tmp2 <- addTargets(tmp2, stimstart = stimstart, stimtype = "Single", targets = targets, tsvariable = "RecordingTimestamp", stimvariable = "MediaName", stimtimes = times + 500, targetduration = 500)
        # calculate metrics and add to metrics data frame
        tmpCalibMetric <- metrics(tmp2, targets = targets, xcoordsvar = "GazePointX (ADCSpx)", ycoordsvar = "GazePointY (ADCSpx)")
        tmpCalibMetric$participant <- participant
        allMetrics <- rbind(allMetrics, tmpCalibMetric)
    }
}