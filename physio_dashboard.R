# libraries --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(zoo)
library(xts)
library(RHRV)
library(dplyr)
library(mgcv)
library(ggplot2)
library(dygraphs)

# general ######################################################################
options(scipen = 999)
t_start <- 1500000000000 #time start in UNIX time
length_final <- 100

# peak detection ---------------------------------------------------------------
peaks <- function (x, y = NULL, mode = "maxmin"){
  if (!mode %in% c("max", "min", "maxmin"))
    stop("unknown mode:", mode)
  xy <- xy.coords(x, y)
  x <- xy$x
  y <- xy$y
  l <- length(y)
  ym1 <- c(y[-1], y[l])
  yp1 <- c(y[1], y[-l])
  if (mode == "min") {
    xx <- x[y < ym1 & y < yp1]
    yy <- y[y < ym1 & y < yp1]
  }
  else if (mode == "max") {
    xx <- x[y > ym1 & y > yp1]
    yy <- y[y > ym1 & y > yp1]
  }
  else {
    xx <- x[y > ym1 & y > yp1 | y < ym1 & y < yp1]
    yy <- y[y > ym1 & y > yp1 | y < ym1 & y < yp1]
  }
  list(x = xx, y = yy)
}
# dyUnzoom ---------------------------------------------------------------------
dyUnzoom <-function(dygraph) { #function to add the unzoom button
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}

# ui ###########################################################################

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input_tab", icon = icon("files-o")),
      menuItem("Features", tabName = "features_tab", icon = icon("sliders")),
      menuItem("Biometrics", tabName = "biometrics_tab", icon = icon("dashboard"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input_tab",
      fluidRow(tags$head(tags$style(type="text/css", "
      #loadmessage {
      position: fixed;
      top: 500px;
      left: 0px;
      width: 100%;
      padding: 5px 0px 5px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 100%;
      color: #000000;
      background-color: #808080;
      z-index: 105;}"
                                    )
                         ),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage")
                                ),
               tabBox(title = "", width=NULL,
                      tabPanel(title=tagList(shiny::icon("cloud-download"), "API connect"),
                               sidebarLayout(
                                                    sidebarPanel(
                                                      actionButton(inputId = "connect_api", label = "API connection",style = "color: black; background-color: #FF8000"),
                                                      dateInput("date",label = "Date input:",value = "2017-08-17",weekstart = 1),
                                                      DT::dataTableOutput("sessionidtable")
                                                    ),
                                                    mainPanel(
                                                      tags$p("first two rows of the Heartrate file"),
                                                      tableOutput("HR_db_table"),
                                                      tags$p("first two rows of the ECG file"),
                                                      tableOutput("ECG_db_table"),
                                                      tags$p("first two rows of the Breathingrate file"),
                                                      tableOutput("BR_db_table"),
                                                      tags$p("first two rows of the Skintemperature file"),
                                                      tableOutput("ST_db_table"),
                                                      tags$p("first two rows of the GSR file"),
                                                      tableOutput("GSR_db_table")
                                                    )
                                                  )
                                         ),
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         tabPanel(title=tagList(shiny::icon("sign-in"), "Data Upload"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      fileInput("HR_upload", "Choose Heart Rate CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("ECG_upload", "Choose ECG CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("BR_upload", "Choose Breathing Rate CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("ST_upload", "Choose Skin Temperature CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("GSR_upload", "Choose GSR CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      )
                                                    ),
                                                    mainPanel(
                                                      tags$p("first two rows of the Heartrate file"),
                                                      tableOutput("HR_upload_table"),
                                                      tags$p("first two rows of the ECG file"),
                                                      tableOutput("ECG_upload_table"),
                                                      tags$p("first two rows of the Breathingrate file"),
                                                      tableOutput("BR_upload_table"),
                                                      tags$p("first two rows of the Skintemperature file"),
                                                      tableOutput("ST_upload_table"),
                                                      tags$p("first two rows of the GSR file"),
                                                      tableOutput("GSR_upload_table")
                                                    )
                                                  )
                                         ),
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         tabPanel(title=tagList(shiny::icon("random"), "Data Simulation"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      sliderInput("duration", "Session duration (min):",min=1, max=60, value=5, step= 1),
                                                      sliderInput("autocorrelation", "Autocorrelation",min=0, max=0.99, value=0.9, step= 0.01),
                                                      actionButton(inputId = "simulate_data", label = "Simulate data",style = "color: black; background-color: #FF8000")
                                                    ),
                                                    mainPanel(
                                                      tags$p("first two rows of the Heartrate file"),
                                                      tableOutput("HR_simulate_table"),
                                                      tags$p("first two rows of the ECG file"),
                                                      tableOutput("ECG_simulate_table"),
                                                      tags$p("first two rows of the Breathingrate file"),
                                                      tableOutput("BR_simulate_table"),
                                                      tags$p("first two rows of the Skintemperature file"),
                                                      tableOutput("ST_simulate_table"),
                                                      tags$p("first two rows of the GSR file"),
                                                      tableOutput("GSR_simulate_table")
                                                    )
                                                  )
                                         )
                                  ) #close tabBox
                                ) #close fluidpage
                                ), #close tabItem "input_tab"
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                                                 
                      tabItem(tabName = "features_tab",
                              fluidRow(tags$head(tags$style(type="text/css", "
                                                      #loadmessage {
                                                      position: fixed;
                                                      top: 500px;
                                                      left: 0px;
                                                      width: 100%;
                                                      padding: 5px 0px 5px 0px;
                                                      text-align: center;
                                                      font-weight: bold;
                                                      font-size: 100%;
                                                      color: #000000;
                                                      background-color: #808080;
                                                      z-index: 105;
                                                      }")
                                                ),
                                                conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")
                                                ),
                                      tabBox(title = "", width=NULL,
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                       tabPanel(title=tagList(shiny::icon("heart-o"), "Heart Rate"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    tags$p("Evolution of Heart Rate (HR) according the time")

                                                  ),
                                                  mainPanel(
                                                    dygraphOutput("plot_HR", height = "300px")
                                                  )
                                                )
                                       ),
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                      tabPanel(title=tagList(shiny::icon("heartbeat"), "ElectroCardioGram"),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$p("Evolution of ElectroCardioGram (ECG) according the time and Heart Rate Variability (HRV) analysis"),
                                                   sliderInput("peak_detection", "Threshold to identify ECG R peaks:",min=-2, max=2, value=0.7, step= 0.1),
                                                   sliderInput("window_size", "Size of window for calculations (seconds):",min=100, max=1000, value=100, step= 100),
                                                   sliderInput("window_shift", "Displacement of window for calculations (seconds):",min=10, max=100, value=30, step= 10),
                                                   sliderInput("length_HRVHF", "Length of the HRV (high frequency) analysis",min=100, max=1000, value=100, step= 100),
                                                   selectInput("wavelet", "Mother wavelet used to calculate the spectrogram",c("fourier","wavelet"),selected="fourier"),
                                                   actionButton("display_spectrum", "Display HRV spectrum plot"),
                                                   actionButton("display_frequency", "Display HRV frequency plot"),
                                                   actionButton("display_poincare", "Display HRV Poincare plot"),
                                                   plotOutput("plot_advancedHRV", height = "300px")
                                                   ),
                                                 mainPanel(
                                                   dygraphOutput("plot_ECG", height = "300px"),
                                                   dygraphOutput("plot_RR", height = "300px"),
                                                   dygraphOutput("plot_HRVHF", height = "300px")
                                                 )
                                                )
                                               ),
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                      tabPanel(title=tagList(shiny::icon("leaf"), "Breathing Rate"),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$p("Evolution of Breathing Rate (BR) according the time")
                                                 ),
                                                 mainPanel(
                                                   dygraphOutput("plot_BR", height = "300px") 
                                                 )
                                                )
                                               ),
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                      tabPanel(title=tagList(shiny::icon("thermometer"), "Skin Temperature"),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$p("Evolution of Skin Temperature (ST) according the time")
                                                 ),
                                                 mainPanel(
                                                   dygraphOutput("plot_ST", height = "300px")
                                                 )
                                                )
                                               ),
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                      tabPanel(title=tagList(shiny::icon("hand-paper-o"), "Galvanic Skin Response"),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$p("Evolution ofGalvanic Skin Response (GSR) according the time and Skin Conductance Level (SCL) / Skin Conductance Response (SCR) analysis")
                                                 ),
                                                 mainPanel(
                                                   dygraphOutput("plot_GSR", height = "300px"),
                                                   dygraphOutput("plot_SCL", height = "300px"),
                                                   dygraphOutput("plot_SCR", height = "300px")
                                                 )
                                                )
                                      )
                                      ) #close tabBox
                              ) #close fluidpage
                      ), #close tabItem "feature_tab"
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                                                 
                                    tabItem(tabName = "biometrics_tab",
                                              fluidRow(
                                                      tags$head(tags$style(type="text/css", "
                                                                                            #loadmessage {
                                                                                            position: fixed;
                                                                                            top: 500px;
                                                                                            left: 0px;
                                                                                            width: 100%;
                                                                                            padding: 5px 0px 5px 0px;
                                                                                            text-align: center;
                                                                                            font-weight: bold;
                                                                                            font-size: 100%;
                                                                                            color: #000000;
                                                                                            background-color: #808080;
                                                                                            z-index: 105;
                                                                                            }
                                                                                            ")),
                                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),
                                                      tabBox(title = "", width=NULL,
                                                          tabPanel(title=tagList(shiny::icon("pie-chart"), "Biometrics"),
                                                                        helpText("Click and drag on the plot to zoom and select date ranges"),
                                                                        dygraphOutput("plot_biometric")
                                                          ),
                                                          tabPanel(title=tagList(shiny::icon("area-chart"), "Descriptive"),
                                                                        helpText("Representation of individual biometric pattern"),
                                                                        plotOutput("plot_descriptive")
                                                          ),
                                                          tabPanel(title=tagList(shiny::icon("sitemap"), "Scores"),
                                                                        helpText("The estimated impact of the biometrics on total sessions, values higher than one are statistically significant.  Get more detail in the analysis section."),
                                                                        fluidRow(valueBoxOutput("HRbox"),
                                                                                 valueBoxOutput("BRbox"),
                                                                                 valueBoxOutput("STbox"),
                                                                                 valueBoxOutput("HFbox"),
                                                                                 valueBoxOutput("SCLbox"),
                                                                                 valueBoxOutput("SCRbox")
                                                            )
                                                        )

                                  ) #close tabBox
                                ) #close fluidpage
                              ) #close tabItem "biometrics_tab"
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                                                 
                      ) #close tabItems
                    ) #close Dashboardbody
) #close Dashboardpage

##########################################################################################################
#                                                    server                                              #
##########################################################################################################
server <- function(input, output) {
  options(shiny.maxRequestSize=60*1024^2) # size of the upload files
  ##################################################################################
  ################# input_tab ######################################################
  ##################################################################################
  
  ################# API connect ####################################################
  day_id <- eventReactive(input$connect_api, {
    pg <- dbDriver("PostgreSQL") 
    db <- dbConnect(drv = pg, 
                    user="xxxxxx",
                    password="xxxxxxx",
                    host="xxxxxxxx",
                    dbname="xxxxx")
    day_query1 <- as.character(input$date)
    day_query2 <- as.character(base::as.Date(day_query1)+1)
    qr <- paste0("SELECT DISTINCT session_id FROM ts_combined WHERE time >= '",day_query1,"' AND time < '",day_query2,"';")
    res <- dbGetQuery(db, qr)
    # closing database
    dbDisconnect(db)
    # sending dataframe
    res
  })
  
  output$sessionidtable <- DT::renderDataTable(day_id(), selection = 'single',options = list(lengthChange = FALSE,searching = FALSE))
  
  raw_db <- eventReactive(input$sessionidtable_cell_clicked, {
    pg <- dbDriver("PostgreSQL") 
    db <- dbConnect(drv = pg, 
                    user="xxxxxxx",
                    password="xxxxxx",
                    host="xxxxxxxx",
                    dbname="eai")
    session_id_query <- input$sessionidtable_cell_clicked$value
    day_query1 <- as.character(input$date)
    day_query2 <- as.character(base::as.Date(day_query1)+1)
    qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS date_part FROM ts_combined WHERE session_id = '",session_id_query,"' AND time >= '",day_query1,"' AND time < '",day_query2,"';")
    res <- dbGetQuery(db, qr) 
    # closing database
    dbDisconnect(db)
    # sending dataframe
    res
  })
  
  api_db_session_date <- reactive({
    if (length(raw_db()) > 0) {
      res <- raw_db() %>%
        distinct(date_part,metric,.keep_all = TRUE) %>% # remove duplicates
        group_by(date_part) %>% # select variables to spread the table
        spread(metric, value) %>% 
        ungroup(date_part) %>%
        mutate(time_date = as.POSIXct(as.numeric(as.character(date_part))/1000, origin = "1970-01-01", tz="Europe/London"))
      res
    }
  })
  output$HR_db_table <- renderTable({
    if (length(grep("heartrate", colnames(api_db_session_date()))) > 0) {
      db_HR <- api_db_session_date() %>%
        select(time_date,heartrate)
      head(db_HR,2)
    }
  })  
  output$ECG_db_table <- renderTable({
    if (length(grep("ECG", colnames(api_db_session_date()))) > 0) {
      db_ECG <- api_db_session_date() %>%
        select(time_date,ecg)
      head(db_ECG,2)
    }
  })
  output$BR_db_table <- renderTable({
    if (length(grep("breathingrate", colnames(api_db_session_date()))) > 0) {
      db_BR <- api_db_session_date() %>%
        select(time_date,breathingrate)
      head(db_BR,2)
    }
  })
  output$ST_db_table <- renderTable({
    if (length(grep("skintemperature", colnames(api_db_session_date()))) > 0) {
      db_ST <- api_db_session_date() %>%
        select(time_date,skintemperature)
      head(db_ST,2)
    }
  })
  output$GSR_db_table <- renderTable({
    if (length(grep("gsr", colnames(api_db_session_date()))) > 0) {
      db_GSR <- api_db_session_date() %>%
        select(time_date,gsr)
      head(db_GSR,2)
    }
  })
  
  ################# Upload data ####################################################
  # upload buttons
  data_HR_upload <- reactive({
    inFile <- input$HR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_ECG_upload <- reactive({
    inFile <- input$ECG_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_BR_upload <- reactive({
    inFile <- input$BR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_ST_upload <- reactive({
    inFile <- input$ST_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_GSR_upload <- reactive({
    inFile <- input$GSR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  # upload tables
  output$HR_upload_table <- renderTable({
    head(data_HR_upload(),2)
  })  
  output$ECG_upload_table <- renderTable({
    head(data_ECG_upload(),2)
  })
  output$BR_upload_table <- renderTable({
    head(data_BR_upload(),2)
  })
  output$ST_upload_table <- renderTable({
    head(data_ST_upload(),2)
  })
  output$GSR_upload_table <- renderTable({
    head(data_GSR_upload(),2)
  })
  ################# Data simulation ####################################################
  # "simulate data" button
  data_HR_sim <- eventReactive(input$simulate_data,{
    time <- seq(t_start, t_start+(input$duration*60000), by = 500)
    Heartrate <- as.numeric(arima.sim(model = list(ar = input$autocorrelation) , n = length(time), mean = 8, sd = 5))
    data.frame(time,Heartrate)
  })
  data_ECG_sim <- eventReactive(input$simulate_data,{
    time <- seq(t_start, t_start+(input$duration*60000), by = 4)
    QRS <- c(0,-0.1,-0.5,-0.8,0,1.2,0,-0.8,-0.5,-0.1,0)
    Lead.One.mV <- c(QRS,rep(0,sample(80:100, 1)))
    while (length(Lead.One.mV)<length(time)){
      Lead.One.mV <- c(Lead.One.mV,QRS,rep(0,sample(80:100, 1)))
    }
    Lead.One.mV <- round(Lead.One.mV[1:length(time)],6)
    data.frame(time,Lead.One.mV)
  })
  data_BR_sim <- eventReactive(input$simulate_data,{
    time <- seq(t_start, t_start+(input$duration*60000), by = 1500)
    Breathing.Rate <- as.numeric(arima.sim(model = list(ar = input$autocorrelation) , n = length(time), mean = 4, sd = 5))
    data.frame(time,Breathing.Rate)
  })
  data_ST_sim <- eventReactive(input$simulate_data,{
    time <- seq(t_start, t_start+(input$duration*60000), by = 1500)
    Skin.Temperature <- as.numeric(arima.sim(model = list(ar = input$autocorrelation) , n = length(time), mean = 3.7, sd = 0.5))
    data.frame(time,Skin.Temperature)
  })
  data_GSR_sim <- eventReactive(input$simulate_data,{
    time <- seq(t_start, t_start+(input$duration*60000), by = 500)
    GSR_100Microsimens <- as.numeric(arima.sim(model = list(ar = input$autocorrelation) , n = length(time), mean = 100, sd = 120))
    data.frame(time,GSR_100Microsimens)
  })
  # simulate tables
  output$HR_simulate_table <- renderTable({
    head(data_HR_sim(),2)
  })  
  output$ECG_simulate_table <- renderTable({
    head(data_ECG_sim(),2)
  })
  output$BR_simulate_table <- renderTable({
    head(data_BR_sim(),2)
  })
  output$ST_simulate_table <- renderTable({
    head(data_ST_sim(),2)
  })
  output$GSR_simulate_table <- renderTable({
    head(data_GSR_sim(),2)
  })
  ##################################################################################
  ################# features_tab ###################################################
  ##################################################################################
  
  # Heart Rate #####################################################################
  data_HR <- reactive({
    if (!is.null(data_HR_sim())){
      data <- data_HR_sim()
    }  
    data <- data %>%
      dplyr::select(time,Heartrate) %>%
      dplyr::mutate(biometric = "HR") %>%
      dplyr::rename(value = Heartrate) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
    return(data)
  })
  
  output$plot_HR <- renderDygraph({
    HR_xts <- xts(x = data_HR()$value, order.by = data_HR()$time_date) # specific dataframe with time series class
    dygraph(HR_xts, main = " ", ylab = "Heart Rate (bpm)", xlab = "time", group = "biometrics")%>%
        dySeries("V1", label = "Heart Rate (bpm)") %>%
        dyRangeSelector(height = 20) %>%
        dyOptions(colors = 'red') %>%
        dyUnzoom() %>%
        dyLegend(width = 300)
  })
  
  # ElectroCardioGram ##############################################################
  data_ECG <- reactive({
    if (!is.null(data_ECG_sim())){
      data <- data_ECG_sim()
    }  
    data <- data %>%
      dplyr::select(time,Lead.One.mV) %>%
      dplyr::mutate(biometric = "ECG") %>%
      dplyr::rename(value = Lead.One.mV) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
    return(data)
  })
  
  feature_beat <- reactive({
    peaks.det <- data_ECG() %>%
      dplyr::select(time,value) %>%
      peaks() # peak detection
    peaks.det %>%
      as.data.frame() %>%
      subset(y > input$peak_detection) %>% # select peaks inferior to -0.5 (R peaks) in a new dataframe (peaks.det.true) because equivital ecg is inverted
      dplyr::rename(time = x) %>% # dplyr::rename the columns
      dplyr::rename(peak = y) %>%
      dplyr::mutate(RR = c(NA,diff(time))) %>%# identify the times of each peak
      dplyr::mutate(RRDiffs = c(NA,abs(diff(RR)))) %>%
      dplyr::mutate(beat = (time-time[1])/1000) %>% # sum each peak time to creates a progressive time line
      dplyr::mutate(NIHR = c(NA,60/diff(beat))) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
   }) 

  hrv.data <- reactive({
    hrv.data <-  CreateHRVData()
    hrv.data <-  SetVerbose(hrv.data, TRUE )
    beat <- as.data.frame(feature_beat()$beat)
    colnames(beat) <- "Time"
    hrv.data$Beat <- beat
    date <- as.POSIXct("1900-01-01 GMT")
    hrv.data$datetime <- date
    hrv.data <-  BuildNIHR(hrv.data) # extract HRV
    hrv.data <-  FilterNIHR(hrv.data) # filter HRV outliers
    hrv.data <-  InterpolateNIHR(hrv.data, freqhr = 4) # Linear or Spline interpolator for build the sample heart rate signal
    hrv.data <-  CreateTimeAnalysis(hrv.data, size = input$window_size,interval = 7.8125) # Creates data analysis structure for time analysis calculation
    hrv.data <-  CreateFreqAnalysis(hrv.data) # Creates data analysis structure for frequency analysis calculations
    hrv.data <-  CalculatePowerBand(hrv.data,indexFreqAnalysis= 1,size = input$window_size, shift = input$window_shift, sizesp = 2048, type = input$wavelet) # Calculates power of the heart rate signal at ULF, VLF, LF and HF bands
    hrv.data <-  CreateNonLinearAnalysis(hrv.data)
    return(hrv.data)
  })
  
  data_HRVHF <- reactive({
    value <- hrv.data()$FreqAnalysis[[length(hrv.data()$FreqAnalysis)]]$HF # selects only the High Frequency power band
    value	<- spline(value,n=input$length_HRVHF)
    value <- as.vector(value$y)
    time <- seq(dplyr::first(data_ECG()$time),dplyr::last(data_ECG()$time), length.out = input$length_HRVHF)
    time_date <- as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London")
    biometric <- "HRVHF"
    data.frame(time,value,biometric,time_date)
  }) 

  output$plot_ECG <- renderDygraph({
    ECG_xts <- xts(x = data_ECG()$value, order.by = data_ECG()$time_date) # specific dataframe with time series class
    dygraph(ECG_xts, main = " ", ylab = "ElectroCardioGram (mV)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "ElectroCardioGram (mV)") %>%
      dyLimit(input$peak_detection, color = "back", label = "peak detection threshold") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'red') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  output$plot_RR <- renderDygraph({
    RR_xts <- xts(x = feature_beat()$RRDiffs, order.by = feature_beat()$time_date) # specific dataframe with time series class
    dygraph(RR_xts, main = " ", ylab = "RR diff distance (ms)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "RR diff distance (ms)") %>%
      dyLimit(20, color = "blue", label = "NN20") %>%
      dyLimit(50, color = "blue", label = "NN50") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'red') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  output$plot_HRVHF <- renderDygraph({
    HRVHF_xts <- xts(x = data_HRVHF()$value, order.by = data_HRVHF()$time_date) # specific dataframe with time series class
    dygraph(HRVHF_xts, main = " ", ylab = "Heart Rate Variability (high frequency)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "HRV") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'red') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
observeEvent(input$display_spectrum,{
  output$plot_advancedHRV <- renderPlot({
    signal  <-  1000/(feature_beat()$NIHR/60)
    signal  <-  na.remove(signal)
    spec.pgram(signal)
  })
})

observeEvent(input$display_frequency,{
  output$plot_advancedHRV <- renderPlot({
    if (input$wavelet == "fourier") {
      PlotPowerBand(hrv.data(), indexFreqAnalysis = 1, ymax = 200, ymaxratio = 1.7)
    } else if (input$wavelet == "wavelet") {
      PlotPowerBand(hrv.data(), indexFreqAnalysis = 1, ymax = 700, ymaxratio = 50)
    }
  })
})
observeEvent(input$display_poincare,{
  output$plot_advancedHRV <- renderPlot({
    PoincarePlot(hrv.data(),indexNonLinearAnalysis=1,timeLag=1, doPlot=TRUE)
  })
})
  # Breathing Rate ###################################################################
  data_BR <- reactive({
    if (!is.null(data_BR_sim())){
      data <- data_BR_sim()
    }  
    data <- data %>%
      dplyr::select(time,Breathing.Rate) %>%
      dplyr::mutate(biometric = "BR") %>%
      dplyr::rename(value = Breathing.Rate) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
    return(data)
  })

  output$plot_BR <- renderDygraph({
    BR_xts <- xts(x = data_BR()$value, order.by = data_BR()$time_date) # specific dataframe with time series class
    dygraph(BR_xts, main = " ", ylab = "Breathing Rate (rpm)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "Breathing Rate (rpm)") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'blue') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  # Skin Temperature ##################################################################
  data_ST <- reactive({
    if (!is.null(data_ST_sim())){
      data <- data_ST_sim()
    }  
    data <- data %>%
      dplyr::select(time,Skin.Temperature) %>%
      dplyr::mutate(biometric = "ST") %>%
      dplyr::rename(value = Skin.Temperature) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
    return(data)
  })
  output$plot_ST <- renderDygraph({
    ST_xts <- xts(x = data_ST()$value, order.by = data_ST()$time_date) # specific dataframe with time series class
    dygraph(ST_xts, main = " ", ylab = "Skin Temperature (degree C)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "Skin Temperature (degree C)") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'orange') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  # Galvanic Skin Response ###############################################################
  data_GSR <- reactive({
    if (!is.null(data_GSR_sim())){
      data <- data_GSR_sim()
    }  
    data <- data %>%
      dplyr::select(time,GSR_100Microsimens) %>%
      dplyr::mutate(biometric = "GSR") %>%
      dplyr::rename(value = GSR_100Microsimens) %>%
      dplyr::mutate(time_date = as.POSIXct(as.numeric(as.character(time))/1000, origin = "1970-01-01", tz="Europe/London"))
    data_zoo <- zoo(data$value,data$time) # zoo format
    data_st <- stl(ts(data_zoo, frequency=100), "periodic") # seasonal decomposition of GSR data
    data$SCL <- as.numeric(data_st$time.series[,"trend"])
    data$SCR <- as.numeric(data_st$time.series[,"remainder"])
    return(data)
  })
  
    output$plot_GSR <- renderDygraph({
    GSR_xts <- xts(x = data_GSR()$value, order.by = data_GSR()$time_date) # specific dataframe with time series class
    dygraph(GSR_xts, main = " ", ylab = "Galvanic Skin Response (microS)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "GSR (microS)") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'green') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  output$plot_SCL <- renderDygraph({
    SCL_xts <- xts(x = data_GSR()$SCL, order.by = data_GSR()$time_date) # specific dataframe with time series class
    dygraph(SCL_xts, main = " ", ylab = "Skin Conductance Level (microS)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "SCL (microS)") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'green') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  output$plot_SCR <- renderDygraph({
    SCR_xts <- xts(x = data_GSR()$SCR, order.by = data_GSR()$time_date) # specific dataframe with time series class
    dygraph(SCR_xts, main = " ", ylab = "Skin Conductance Response (microS)", xlab = "time", group = "biometrics")%>%
      dySeries("V1", label = "SCR (microS)") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = 'green') %>%
      dyUnzoom() %>%
      dyLegend(width = 300)
  })
  
  ##################################################################################
  ################# biometrics_tab #################################################
  ##################################################################################
  
  data_biometric <- reactive({
    data_HR_zoo <- zoo(data_HR()$value, order.by = data_HR()$time_date)
    data_BR_zoo <- zoo(data_BR()$value, order.by = data_BR()$time_date)
    data_ST_zoo <- zoo(data_ST()$value, order.by = data_ST()$time_date)
    data_SCL_zoo <- zoo(data_GSR()$SCL, order.by = data_GSR()$time_date)
    data_SCR_zoo <- zoo(data_GSR()$SCR, order.by = data_GSR()$time_date)
    data_HF_zoo <- zoo(data_HRVHF()$value, order.by = data_HRVHF()$time_date)
    #################################################
    data_biometric <- merge.zoo(data_HR_zoo,data_BR_zoo,data_ST_zoo,data_SCL_zoo,data_SCR_zoo,data_HF_zoo)
    data_biometric <- na.approx(data_biometric)
    colnames(data_biometric) <- c("HR","BR","ST","SCL","SCR","HRV-HF")
    return(data_biometric)
  })
  
  output$plot_biometric <- renderDygraph({  
    custom_palette <- c("red","blue","orange","green","darkgreen","purple")
    dygraph(data_biometric()) %>% 
      dyRangeSelector() %>% 
      dyOptions(colors = custom_palette) %>%
      dyLegend(width = 500)
  })
  
  ######################################### descriptive plot #########################################
  data_descriptive <- reactive({
    data_HR <- data_HR()
    data_HRVHF <- data_HRVHF()
    data_BR <- data_BR()
    data_ST <- data_ST()
    data_SCL <- data_GSR() %>%
      select(time,SCL,time_date) %>%
      mutate(biometric = "SCL") %>%
      dplyr::rename(value = SCL)
    data_SCR <- data_GSR() %>%
      select(time,SCR,time_date) %>%
      mutate(biometric = "SCR") %>%
      dplyr::rename(value = SCR)
    data_descriptive <- rbind(data_HR,data_HRVHF,data_BR,data_ST,data_SCL,data_SCR)
    return(data_descriptive)
  })
  output$plot_descriptive <- renderPlot({
    ggplot(data=data_descriptive(),aes(x=time_date, y=value))+
      geom_line()+
      facet_wrap(~biometric, scales = "free")
  })
  
  ################################################# Display GAMM edf #################################################
  output$HRbox <- renderValueBox({
    gamm.HR <- gam(value~s(time),data=data_HR(),method ="REML",correlation = corAR1()) # processes GAMM (fixed effect = time,random effect = participant, method = restricted maximum likelihood, correlation =  autocorrelation structure of order 1)
    summary_gamm.HR <- summary(gamm.HR)
    summary_gamm.HR <- round(summary_gamm.HR$edf,2)
    valueBox(
      summary_gamm.HR, "Heart Rate", icon = icon("heartbeat"),
      color = "red"
    )
  })
  output$BRbox <- renderValueBox({
    #check GAM BR
    gamm.BR <- gam(value~s(time),data=data_BR(),method ="REML",correlation = corAR1())
    summary_gamm.BR <- summary(gamm.BR)
    summary_gamm.BR <- round(summary_gamm.BR$edf,2)
    valueBox(
      summary_gamm.BR, "Breathing Rate", icon = icon("leaf"),
      color = "blue"
    )
  })
  output$STbox <- renderValueBox({
    #check GAM ST
    gamm.ST <- gam(value~s(time),data=data_ST(),method ="REML",correlation = corAR1())
    summary_gamm.ST <- summary(gamm.ST)
    summary_gamm.ST <- round(summary_gamm.ST$edf,2)
    valueBox(
      summary_gamm.ST, "Skin Temperature", icon = icon("thermometer"),
      color = "yellow"
    )
  })
  output$HFbox <- renderValueBox({
    #check GAM HF.HRV
    gamm.HF.HRV <- gam(value~s(time),data=data_HRVHF(),method ="REML",correlation = corAR1())
    summary_gamm.HF.HRV <- summary(gamm.HF.HRV)
    summary_gamm.HF.HRV <- round(summary_gamm.HF.HRV$edf,2)
    valueBox(
      summary_gamm.HF.HRV, "Heart Rate Activity (high frequency)", icon = icon("smile-o"),
      color = "purple"
    )
  })
  output$SCLbox <- renderValueBox({
    #check GAM SCL
    gamm.SCL <- gam(SCL~s(time),data=data_GSR(),method ="REML",correlation = corAR1())
    summary_gamm.SCL <- summary(gamm.SCL)
    summary_gamm.SCL <- round(summary_gamm.SCL$edf,2)
    valueBox(
      summary_gamm.SCL, "Skin Conductance Level", icon = icon("hand-paper-o"),
      color = "green"
    )
  })
  output$SCRbox <- renderValueBox({
    #check GAM SCR
    gamm.SCR <- gam(SCR~s(time),data=data_GSR(),method ="REML",correlation = corAR1())
    summary_gamm.SCR <- summary(gamm.SCR)
    summary_gamm.SCR <- round(summary_gamm.SCR$edf,2)
    valueBox(
      summary_gamm.SCR, "Skin Conductance Response", icon = icon("tint"),
      color = "olive"
    )
  })
}# close server

shinyApp(ui, server)