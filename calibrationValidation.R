
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
    # TODO: change to warning
    if(!is.element("target_x", names(gazedata))){
        print("Make sure to run addTargets() before validating calibration!")
    }
    ### start calibration function here
    # select only calibration media data, ignoring extension          
    tempdata <- gazedata[grepl(substr(calib_media_name, 1,nchar(calib_media_name) - 4), gazedata[,make.names(media_variable)]), ]
    # prepare data to train regression
    tempdata2 <- tempdata[!is.na(tempdata$target_x) & eu.dist(as.numeric(tempdata[,make.names(gaze_point_x_variable)]), as.numeric(tempdata[,make.names(gaze_point_y_variable)]), tempdata$target_x, tempdata$target_y) <= as.numeric(max_distance),]
    
    true_x <- tempdata2$target_x
    test_x <- as.numeric(tempdata2[,make.names(gaze_point_x_variable)])
    # TODO: add trycatch
    x_model <- rlm(tempdata2$target_x ~ test_x, psi = psi.bisquare)
    true_y <- tempdata2$target_y
    test_y <- as.numeric(tempdata2[,make.names(gaze_point_y_variable)])
    y_model <- rlm(true_y ~ test_y, psi = psi.bisquare)
    
    newx <- predict(x_model, newdata = data.frame(test_x = as.numeric(gazedata[,make.names(gaze_point_x_variable)])))
    newy <- predict(y_model, newdata = data.frame(test_y = as.numeric(gazedata[,make.names(gaze_point_y_variable)])))
    # remove newx and newy if available to replace with new ones
    if(is.element("newx", names(gazedata))){
        gazedata <- subset(gazedata, select=-c(newx,newy))
    }
    # add updatednewx and newy
    return(cbind(gazedata, newx, newy))
    ### finish calibration validation here
    
    # TODO: add training media index to keep some media for testing
    # After calculating metrics training can then be run with all media
}

### After running validateCalibrate() we can calculate calibration metrics
# TODO: add test media index for testing performance
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