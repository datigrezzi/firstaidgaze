library(shiny)
library(shinycssloaders)
library(htmlwidgets)

ui <- navbarPage("First Aid Gaze",
    tabPanel("Data",
        fluidRow(
            sidebarPanel(
                selectInput('eyetracker', 'Eye Tracker', c("Tobii", "SMI", "EyeLink")), #data,
                selectInput('separator', 'Separator', c('TAB', 'Comma', 'Semicolon')), #data
                selectInput('decimal', 'Decimal', c('Point', 'Comma'), selected = 'Comma'), #data
                uiOutput('nastring') #data textInput('nastring', 'NA String', "NA")
            ),
            sidebarPanel(
                uiOutput('gazeCoordinatesVariableX'), #var
                uiOutput('gazeCoordinatesVariableY'), #var, 'X Gaze Coordinate Variable', 'GazePointX (ADCSpx)')
                uiOutput('timestampVariable'), #var, 'Timestamp variable', 'RecordingTimestamp')
                uiOutput('mediaVariable') #var, 'Stimulus Name Variable', 'MediaName')
            )
        ),
        br(),
        fileInput("gazeFile", "Upload Gaze File", accept = c("text/tsv", ".tsv", "text/csv","text/comma-separated-values,text/plain",".csv")), #data
        br(),
        withSpinner(dataTableOutput("gazeDataOut")) #data ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
        
    ),
    tabPanel(title = "Stimulus",
        fluidRow(
            sidebarPanel(
                selectInput('calibStimType', 'Stimulus Type', c("Single", "Multiple")), #stim
                textInput("targetTimes", "Targets Onset:", value = "1200; 4200; 7200; 10200"), #stim
                textInput('targetDuration', 'Target Duration', '1000'), #stim
                textInput('calibStimExclude', 'Exclude Stimulus', "instruction") #stim
            ),
            sidebarPanel(
                textInput('screenResolutionW', 'Screen Width', '1280'), #stim
                textInput('screenResolutionH', 'Screen Height', '1024'), #stim
                textInput('calibStimTop', 'Stimulus Top', '224'), #stim
                textInput('calibStimLeft', 'Stimulus Left', '280') #stim
            ),
            sidebarPanel(
                fileInput("stimFile", "Upload Stimulus Image",  accept = c('image/png', 'image/jpeg','image/jpg')), #stim
                textInput('distanceThreshold', 'Max Point Distance', '400'), #calib
                actionButton("calibrate", "Start Calibration"), #calib
                br(),
                br(),
                downloadButton("newGazeFile", label = "Data"), # data
                downloadButton("downloadMetrics", label = "Metrics") # metrics
            )
        ),
        br(),
        # tags$head(tags$style(type="text/css","#image img {max-width: 100%}")), #stim
        textOutput("clickinst"), #stim
        br(),
        withSpinner(plotOutput("gazePlotOut", click = "image_click")),# #stim
        br(),
        withSpinner(dataTableOutput("calibmetrics")) #data ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
    ),
    tabPanel(title = "Help",
        br(),
        includeMarkdown("readme.md")
    )
)
