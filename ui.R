library(shiny)
library(shinycssloaders)
library(htmlwidgets)

ui <- navbarPage("First Aid Gaze",
    tabPanel("Data",
        selectInput('eyetracker', 'Eye Tracker', c("Tobii", "SMI", "EyeLink")), #data
        fluidRow(
            column(
                2,
                uiOutput('nastring') #data textInput('nastring', 'NA String', "NA")
            ),
            column(
                2,
                selectInput('separator', 'Sep', c('TAB', 'Comma', 'Semicolon'))#data
            ),
            column(
                2,
                selectInput('decimal', 'Dec', c('Point', 'Comma'))#data
            )
        ),
        fluidRow(
            column(
             4,
             uiOutput('gazeCoordinatesVariableX'), #var
             uiOutput('gazeCoordinatesVariableY')#var, 'X Gaze Coordinate Variable', 'GazePointX (ADCSpx)')
            ),
            column(
             4,
             uiOutput('timestampVariable'), #var, 'Timestamp variable', 'RecordingTimestamp'),
             uiOutput('mediaVariable') #var, 'Stimulus Name Variable', 'MediaName')
            )
        ),
        fileInput("gazeFile", "Upload Gaze File",  accept = "text"), #data
        br(),
        withSpinner(tableOutput("gazeDataOut")) #data
    ),
    tabPanel(title = "Stimulus",
        fileInput("stimFile", "Upload Stimulus Image",  accept = c('image/png', 'image/jpeg','image/jpg')), #stim
        textInput("targetTimes", "Targets Onset:", value = "1200; 4200; 7200; 10200"), #stim
        fluidRow(
            column(
                2,
                textInput('screenResolutionW', 'Screen Width', '1280')#stim
            ),
            column(
                2,
                textInput('screenResolutionH', 'Height', '1024')#stim
            ),
            column(
                2,
                textInput('calibStimTop', 'Top', '224')#stim
            ),
            column(
                2,
                textInput('calibStimLeft', 'Left', '280')#stim
            )
        ),
        fluidRow(
            column(
                3,
                selectInput('calibStimType', 'Stimulus Type', c("Single", "Multiple"))#stim
            ),
            column(
                3,
                textInput('calibStimExclude', 'Exclude Stimulus', "instruction")#stim
            ),
            column(
                4,
                textInput('distanceThreshold', 'Max Point Distance', '400'),#calib
                textInput('targetDuration', 'Target Duration (ms)', '1000')#stim
            )
        ),
        
        actionButton("calibrate", "Start Calibration"),#calib
        downloadButton("newGazeFile", label = "Download"), #calib

        tags$head(tags$style(type="text/css","#image img {max-width: 100%}")), #stim
        br(),
        textOutput("clickinst"), #stim
        br(),
        withSpinner(plotOutput("gazePlotOut", click = "image_click"))#, #stim
        # imageOutput("image", width = "auto", height = "auto"), #stim
        # br(),
        # verbatimTextOutput("clicktext") #replace by points on image
    ),
    # tabPanel(title = "Verify",
    #     withSpinner(plotOutput("gazePlotOut"), click = "image_click") #stim
    # ),
    tabPanel(title = "Help",
        br(),
        h4("Coming soon...")
    )
)
