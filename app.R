####################################### WELCOME TO THE SHINY APP AdRI_GAMLSS ######################
####################################### from Sandra K. (2023) #####################################
###################################################################################################

####################################### Scripts ###################################################

source("R/analysis.R")
source("R/gamlss.R")

####################################### Libraries #################################################

if("boot" %in% rownames(installed.packages())){
  library(boot)} else{
    install.packages("boot")
    library(boot)}

if("dplyr" %in% rownames(installed.packages())){
  library(dplyr)} else{
    install.packages("dplyr")
    library(dplyr)}

if("DT" %in% rownames(installed.packages())){
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if("gamlss" %in% rownames(installed.packages())){
  library(gamlss)} else{
    install.packages("gamlss")
    library(gamlss)}

if("gamlss.add" %in% rownames(installed.packages())){
  library(gamlss.add)} else{
    install.packages("gamlss.add")
    library(gamlss.add)}

if("plotly" %in% rownames(installed.packages())){
  library(plotly)} else{
    install.packages("plotly")
    library(plotly)}

if("rpart" %in% rownames(installed.packages())){
  library(rpart)} else{
    install.packages("rpart")
    library(rpart)}

if("rpart.plot" %in% rownames(installed.packages())){
  library(rpart.plot)} else{
    install.packages("rpart.plot")
    library(rpart.plot)}

if("shinydashboard" %in% rownames(installed.packages())){
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

if("zoo" %in% rownames(installed.packages())){
  library(zoo)} else{
    install.packages("zoo")
    library(zoo)}

####################################### USER INTERFACE ############################################

ui <- dashboardPage(
  dashboardHeader(title = "AdRI_GAMLSS", titleWidth = 175),
  dashboardSidebar(width = 175,
                   sidebarMenu(
                     menuItem("Analysis", tabName = "analysis", icon = icon("database")),
                     menuItem("GAMLSS", tabName = "gamlss", icon = icon("chart-line"), startExpanded = FALSE,
                       menuSubItem("GAMLSS and LMS", tabName = "gamlsslms", icon = icon("chart-line")),
                       menuSubItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
                       menuSubItem("Percentiles", tabName = "percentiles", icon = icon("table")))
                   )),
  dashboardBody(
    
    tags$style("html, body {overflow: visible !important;"),
    
    tabItems(
    tabItem(tabName = "analysis",
            fillPage(fluidRow(
              ### Sidebar - Analysis ###
              box(
                title = tagList(shiny::icon("gear"), "Settings"),
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                
                helpText("Data Upload:"),
                
                selectInput("dataset", "Select preinstalled dataset:", choice = list.files(pattern = c(".csv"), recursive = TRUE)),
                uiOutput("dataset_file"),
                actionButton('reset', 'Reset Input', icon = icon("trash")),
                hr(),
                
                helpText("Data Preprocessing:"),
                
                radioButtons(
                  "days_or_years",
                  "Unit for the age:",
                  c("year" = "age", "day" = "age_days")),
                
                conditionalPanel(
                  condition = "input.days_or_years == 'age'",
                  sliderInput(
                    "age_end",
                    "Select age-range:",
                    min = 0 ,
                    max = 100,
                    value = c(0, 18))),
                
                conditionalPanel(
                  condition = "input.days_or_years == 'age_days'",
                  numericInput(
                    "age_input_min",
                    "Select age-range from:",
                    0,
                    min = 0,
                    max = 100 * 365)),
                
                conditionalPanel(
                  condition = "input.days_or_years == 'age_days'",
                  numericInput("age_input", "to:", 100, min = 1, max = 100 * 365)),
                
                selectInput("sex", "Select the sex:",
                  choices = list(
                    "Male + Female" = "t",
                    "Male" = "m",
                    "Female" = "f")),
                
                textInput("text_unit", "Unit of the analyte:", value = "Unit"),
                checkboxInput("unique", "First unique values", value = TRUE)
              ),
              
              ### MainPanel - Analysis ###
              column(
                width = 9,
                tabsetPanel(
                  tabPanel("Overview", icon = icon("home"),
                    
                    p(br(), strong("Shiny App for calculating age-dependent Reference Intervals!"), br(), br(),
                      "This Shiny App was developed to create age-dependent Reference Intervals using 
                      Generalized Additive Models for Location, Scale and Shape (GAMLSS).", br(),
                      "For further information visit our", a("Wiki", href = "https://github.com/SandraKla/AdRI_GAMLSS/wiki"),"!"),
                    
                    plotlyOutput("scatterplot_plotly", height ="700px")
                  ),
                  
                  tabPanel("Dataset", icon = icon("table"),
                    
                    p(br(), strong("Shiny App for calculating age-dependent Reference Intervals!"), br(), br(),
                    "This Shiny App was developed to create age-dependent Reference Intervals using 
                    Generalized Additive Models for Location, Scale and Shape (GAMLSS).", br(),
                    "For further information visit our", a("Wiki", href = "https://github.com/SandraKla/AdRI_GAMLSS/wiki"),"!"),       
                                  
                    DT::dataTableOutput("datatable")),
                  
                  tabPanel("Barplots", icon = icon("chart-bar"),
                           
                    p(br(), strong("Shiny App for calculating age-dependent Reference Intervals!"), br(), br(),
                    "This Shiny App was developed to create age-dependent Reference Intervals using 
                    Generalized Additive Models for Location, Scale and Shape (GAMLSS).", br(),
                    "For further information visit our", a("Wiki", href = "https://github.com/SandraKla/AdRI_GAMLSS/wiki"),"!"), 
                    
                    plotOutput("barplot_sex", height = "375px"),
                    plotOutput("barplot_value", height = "375px")),
                  
                  tabPanel("Statistics", icon = icon("calculator"),
                    
                    p(br(), strong("Shiny App for calculating age-dependent Reference Intervals!"), br(), br(),
                    "This Shiny App was developed to create age-dependent Reference Intervals using 
                    Generalized Additive Models for Location, Scale and Shape (GAMLSS).", br(),
                    "For further information visit our", a("Wiki", href = "https://github.com/SandraKla/AdRI_GAMLSS/wiki"),"!"),        
                           
                    plotOutput("qqplot", height = "375px"),
                    plotOutput("lognorm", height = "375px"))
                )
              )
            ))),
    
    tabItem(tabName = "gamlsslms",
            fillPage(fluidRow(
              
              ### Sidebar - GAMLSS ###
              box(
                title = tagList(shiny::icon("gear"), "Settings"),
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                
                actionButton("button_lms", "Start LMS", icon("calculator"), onclick = "$(tab).removeClass('disabled')"),
                htmlOutput("buttons_lms"),
                hr(),
                selectInput(
                  "distribtion_gamlss",
                  "Distribution for GAMLSS:",
                  choices = list(
                    "Log-Normal Distribution" = "LOGNO",
                    "Normal Distribution" = "NO",
                    "Box-Cox" = c(
                      #"Box-Cole Green Distribution" = "BCCG",
                      "Box-Cole Green Distribution (orginal)" = "BCCGo",
                      #"Box-Cole Green Exp. Distribution" = "BCPE",
                      "Box-Cole Green Exp. Distribution (orginal)" = "BCPEo",
                      #"Box-Cole Green T-Distribution" = "BCT",
                      "Box-Cole Green T-Distribution (orginal)" = "BCTo"
                    )
                  )
                ),
                checkboxInput("checkbox", "Distribution proposed by the LMS", value = FALSE),
                actionButton("button_gamlss", "Start GAMLSS", icon("calculator"), onclick = "$(tabs).removeClass('disabled')"),
                htmlOutput("buttons_gamlss")
              ),
              
              ### MainPanel - GAMLSS ###
              column(width = 9,
                tabsetPanel(
                  tabPanel(
                    "LMS", 
                    icon = icon("chart-line"), 
                    value = "nav_lms",
                    
                    fluidRow(
                    box(title = tagList(shiny::icon("chart-line"), "Plot"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "primary",

                        plotOutput("lms", height = "475px")
                    ),
                    
                    box(title = tagList(shiny::icon("calculator"), "Statistics"),
                      width = 12,
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      status = "info",
                      
                      verbatimTextOutput("lms_text"),
                      plotOutput("lms_plot"),
                      plotOutput("lms_fitted"),
                      plotOutput("lms_wormplots"))
                  )),
                  
                  tabPanel(
                    "GAMLSS (Splines)",
                    icon = icon("chart-line"),
                    value = "nav_gamlss",
                    
                    fluidRow(
                    box(title = tagList(shiny::icon("chart-line"), "Plot"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "primary",
                        
                        plotOutput("gamlss_models_splines", height = "475px")
                    ),
                    
                    box(title = tagList(shiny::icon("calculator"), "Statistics"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "info",
                        
                        p("GAMLSS with P-Splines:"),
                        verbatimTextOutput("gamlss_text_psplines"),
                        plotOutput("gamlss_term_pb"),
                        plotOutput("gamlss_fitted_pb_"),
                        p("GAMLSS with Cubic-Splines:"),
                        verbatimTextOutput("gamlss_text_splines"),
                        plotOutput("gamlss_term_cs"),
                        plotOutput("gamlss_fitted_cs_"),
                        p("Wormplots for GAMLSS with the P-Splines and Cubic Splines:"),
                        plotOutput("wormplots_splines"))
                  )),
                  
                  tabPanel(
                    "GAMLSS (Polynomials)",
                    icon = icon("chart-line"),
                    value = "nav_gamlss",
                    
                    fluidRow(
                    box(title = tagList(shiny::icon("chart-line"), "Plot"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "primary",
                        
                        plotOutput("gamlss_models_poly", height ="475px")
                    ),
                    
                    box(title = tagList(shiny::icon("calculator"), "Statistics"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "info",
                        
                        p("GAMLSS with Polynomial Degree 3:"),
                        verbatimTextOutput("gamlss_text_poly"),
                        plotOutput("gamlss_term_poly"),
                        plotOutput("gamlss_fitted_poly_"),
                        p("GAMLSS with Polynomial Degree 4:"),
                        verbatimTextOutput("gamlss_text_poly4"),
                        plotOutput("gamlss_term_poly4"),
                        plotOutput("gamlss_fitted_poly4_"),
                        p("Wormplots for GAMLSS with the Polynomial Degree 3 and 4:"),
                        plotOutput("wormplots_poly"))
                  )),
                 
                  tabPanel(
                    "GAMLSS (Neural Network)",
                    icon = icon("brain"),
                    value = "nav_gamlss",
                    
                    fluidRow(
                    box(title = tagList(shiny::icon("chart-line"), "Plot"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "primary",
                        
                        plotOutput("gamlss_net", height = "475px")),
                    
                    box(title = tagList(shiny::icon("calculator"), "Statistics"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "info",
                        
                        verbatimTextOutput("net_text"),
                        # Plot neural network with term.plot(nn_)
                        plotOutput("network_term"),
                        plotOutput("network_fitted"),
                        plotOutput("nn_wormplots"))
                    )),

                  tabPanel(
                    "GAMLSS (Decision Tree)",
                    icon = icon("tree"),
                    value = "nav_gamlss",
                    
                    fluidRow(
                    box(title = tagList(shiny::icon("chart-line"), "Plot"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "primary",
                        
                        plotOutput("gamlss_tree", height = "475px")
                    ),
                    
                    box(title = tagList(shiny::icon("calculator"), "Statistics"),
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        status = "info",
                    
                        verbatimTextOutput("tree_text"),
                        plotOutput("rpart_tree"),
                        plotOutput("tree_term"),
                        plotOutput("tree_fitted"),
                        plotOutput("tr_wormplots"))
                    )
                  )
                )
              ), tags$script(src = 'tabs_enabled.js')))), 
    
    ### GAMLSS - Comparison ###
    ### GAMLSS - Comparison ###
    tabItem(tabName = "comparison",
            fluidRow(
              
              box(
                title = tagList(shiny::icon("gear"), "Settings"),
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                
                p("Models can be compared visually or with the Akaike Information Criterion (AIC),
                  Generalized Information Criterion (GAIC), Bayesian Information Criterion (BIC),
                  or Pseudo R-Squared (R^2). The model with the smallest value for AIC, BIC and GAIC is the best model for the data.
                  The Pseudo R-Squared (R^2) should be as large as possible for a good model. These values are colored.")
              ),

              box(
                title = tagList(shiny::icon("balance-scale"), "Comparison"),
                width = 9,
                solidHeader = TRUE,
                status = "primary",
                
                DT::dataTableOutput("table_compare"),
                plotOutput("metrics", height = "475px")
              ))),
    
    ### GAMLSS - Prediction ###
    tabItem(tabName = "percentiles",
            
            fluidRow(column(
              width = 3,
              
              fluidRow(
              box(
                title = tagList(shiny::icon("gear"), "Settings"),
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                
                selectInput(
                  "select_model",
                  "Select model:",
                  choices = list(
                    "Splines" = c("P-Splines" = "pb_ri",
                                  "Cubic Splines" = "cs_ri"),
                    "LMS" = c(LMS = "lms_ri"),
                    "Polynomial" = c(
                      "Polynomial (Degree 3)" = "poly_ri",
                      "Polynomial (Degree 4)" = "poly4_ri"
                    ),
                    "Machine Learning" = c("Neural Network" = "nn_ri",
                                           "Decision Tree" = "tr_ri")
                  )
                )
              ))),
              
             column(width = 9,
                       
                box(title = tagList(shiny::icon("chart-line"), "Percentiles"),
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "primary",
                    
                    plotOutput("gamlss_prediction", height = "475px")),
                
                box(title = tagList(shiny::icon("table"), "Table"),
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "info",
    
                    DT::dataTableOutput("gamlss_tables"))
              )
    ))
  ))
)

####################################### SERVER ####################################################

server <- function(input, output, session) {

  options(shiny.sanitize.errors = TRUE)
  
  ##################################### Reactive Expressions ######################################

  ##################################### Reactive Dataset ##########################################
  ########### Data is subset with the function select_data() with the given age interval ##########
  #################################################################################################
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL)
  })
  
  values_lis <- reactiveValues(
    upload_state_lis = NULL
  )
  
  data_analyte <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Load data...", detail = "", value = 2)
    
    cat(paste("*** Welcome to the Shiny App AdRI_GAMLSS! ***\n"))
    
    req(input$dataset, input$age_end)
    
    lms_ready <<- FALSE        # To check if the lms method was used
    modelsprediction <<- FALSE # To check if the models are build 
    residuals_ready <<- FALSE  # Check if the residuals are calculated
    
    # Read the data (from the CALIPER study or from the generator)
    if(is.null(dataset_input())){
    data_data <- read.csv2(input$dataset, header = TRUE, 
                           stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "")}
    if(!is.null(dataset_input()))
    {data_data <- read.csv2(dataset_input()[["datapath"]], header = TRUE, 
                            stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "")}
    ################################### Age is given by days ######################################
    if(input$days_or_years == "age_days"){
      
      # Preprocessing the data
      data_analyte <- select_data_days(data_data, input$age_input_min, input$age_input, input$sex)
      
      ################################# First samples #############################################
      
      rows_table_ <- nrow(data_analyte) 
      
      # Take only the first and unique samples from the data if ID is given
      if(input$unique == TRUE){
        
        data_analyte <- 
          data_analyte %>% 
          group_by(patient) %>% 
          filter(row_number()==1)
        # Convert into tibble so as.data.frame()
        data_analyte <- as.data.frame(data_analyte)
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("*** Information!", rows_table_ - nrow(data_analyte), "values of the patients were present several times and were deleted. ***\n"))}
      }
    } 
    
    else{
      
      ################################# Age in years (default) ####################################
      
      # Preprocessing the data
      data_analyte <- select_data(data_data,input$age_end[1] ,input$age_end[2], input$sex)
      
      ################################# First samples #############################################
      
      rows_table_ <- nrow(data_analyte) 
      
      if(input$unique == TRUE){
        
        data_analyte <- 
          data_analyte %>% 
          group_by(patient) %>% 
          filter(row_number()==1)
        # Convert into tibble so as.data.frame()
        data_analyte <- as.data.frame(data_analyte)
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("*** Information!", rows_table_ - nrow(data_analyte), "values of the patients were present several times and were deleted. ***\n"))}
      }
    }
    
    cat("\n")

    data_analyte_short <<- data_analyte
    on.exit(progress$close())
    data_analyte
  })
  
  ##################################### Build rpart Decision Tree #################################
  
  build_rpart <- reactive({
    rpart_ready <- make_rpart(data_analyte(), as.numeric(input$tree_minsplit))
  })
  
  ##################################### Reactive GAMLSS ###########################################
  # make_gamlss() is used with six different smooth additive terms for the GAMLLSS models #########
  # with different distributions. #################################################################
  
  build_gamlss_model <- eventReactive(input$button_gamlss, {
    
    req(input$dataset, input$age_end, input$distribtion_gamlss, 50, "RS")
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with GAMLSS-Models...", detail = "", value = 2)

    # Error message
    if(input$checkbox == TRUE){
      validate(need(lms_ready == TRUE, 
      "Please make first the LMS-Method to get the proposed distribution!"))
      gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], lms_$family[1], 100, "RS")} 

    else{gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], input$distribtion_gamlss, 100, "RS")}
    
    # Save global value to check later if the models are already calculated
    modelsprediction <<- TRUE
    
    on.exit(progress$close())
  })
  
  ##################################### Reactive LMS ##############################################
  
  lms_reactive <- eventReactive(input$button_lms, {
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with LMS...", detail = "", value = 2)
    
    lms_model <- make_lms(data_analyte())
    
    lms_ready <<- TRUE # Value to check if lms is accomplished
    on.exit(progress$close())
  })
 
  ##################################### Overview ##################################################
  
  # Scatterplot from the data_analyte() with plotly
  output$scatterplot_plotly <- renderPlotly({

    ylab_ <<- paste0(data_analyte()[1,7]," [", input$text_unit,"]")
    
    #if(input$fast == FALSE){
      fig <- plot_ly(data_analyte(), x = ~age_days, y = ~value,
                   text = ~ paste('</br>Patient: ', patient,
                                  '</br>Station: ', code,
                                  '</br>Age [Years]: ', age,
                                  '</br>Age [Days]: ', age_days,
                                  '</br>Value: ', value),
                   type = "scatter",
                   symbol = ~sex,
                   symbols = c('circle', 17),
                   color = ~sex,
                   colors = c("indianred", "cornflowerblue"),
                   mode = "markers",
                   marker = list(size = 10)) %>%
          layout(xaxis = list(title="Age [Days]", titlefont=list(size=20), tickfont = list(size = 15)),
                 yaxis = list(title=ylab_, titlefont=list(size=20), tickfont = list(size = 15)))
  })
  
  # Barplot with the distribution of the sex
  output$barplot_sex <- renderPlot({

    ylab_ <<- paste0(data_analyte()[1,7]," [", input$text_unit,"]")
    
    if(!(nrow(data_analyte())) == 0){
      hist_data_w <- subset(data_analyte(), sex == "F", select = age)
      hist_data_m <- subset(data_analyte(), sex == "M", select = age)
  
      hist_w <- hist(hist_data_w$age, breaks=seq(min(data_analyte()[,3])-1,max(data_analyte()[,3]),by=1))$counts
      hist_m <- hist(hist_data_m$age, breaks=seq(min(data_analyte()[,3])-1,max(data_analyte()[,3]),by=1))$counts
  
      barplot(rbind(hist_m,hist_w), col = c("cornflowerblue","indianred"),
            names.arg=seq(min(data_analyte()[,3]), max(data_analyte()[,3]), by=1), xlab = "AGE_YEARS", las = 1, beside = TRUE, ylab = "Number of data")
      abline(h=0)
      legend("topright", legend = c(paste0("Men: ", nrow(hist_data_m)), paste0("Female: ", nrow(hist_data_w))), col = c("cornflowerblue","indianred"), pch = c(17, 20))
    
      par(new = TRUE)
      boxplot(data_analyte()[,3], horizontal = TRUE, axes = FALSE, col = rgb(0, 0, 0, alpha = 0.15))
    }
  })
  
  output$barplot_value <- renderPlot({
    
    ylab_ <<- paste0(data_analyte()[1,7]," [", input$text_unit,"]")
    
    if(!(nrow(data_analyte())) == 0){
      boxplot(data_analyte()[,5]~interaction(data_analyte()[,2], data_analyte()[,3]), xlab = "Age", 
              ylab = ylab_, col = c("indianred", "cornflowerblue"), las = 2)
    }
  })
  
  # QQ-Plot for the complete dataset  
  output$qqplot <- renderPlot({
    
    if(!(nrow(data_analyte())) == 0){
      qqnorm(data_analyte()[,5], pch = 20, col = "grey")
      qqline(data_analyte()[,5])
    }
  })
  
  # Bowley and Lognormfunction
  output$lognorm <- renderPlot({
    if(!(nrow(data_analyte())) == 0){
      try(def.distribution(data_analyte()[,5]))
    }
  })
  
  # Data-Table
  output$datatable <- DT::renderDataTable({
    
    data_table <- data_analyte()
    colnames(data_table) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "STATION", "ANALYTE")
    
    DT::datatable(data_table, extensions = 'Buttons', rownames= FALSE, 
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')),
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Overview from Dataset'))
  })

  ################################ LMS #############################################
  
  # LMS-Percentile Plot
  output$lms <- renderPlot({
    
    lms_reactive()
    centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 1, 
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3),lwd.centiles = 2, 
             legend = FALSE, col = "lightgrey")
  })

  # Analysis LMS
  output$lms_plot <- renderPlot({
    
    lms_reactive()
    plot(lms_, parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
  
  # Analysis LMS
  output$lms_fitted <- renderPlot({
    
    lms_reactive()
    new_lms_data <- data.frame(value_lms = data_analyte()[[5]], age_lms = data_analyte()[[4]])
    fittedPlot(lms_ ,x = new_lms_data$age_lms, xlab = "Age [Days]")
  })
  
  # Analysis LMS-Text
  output$lms_text <- renderPrint({
    
    lms_reactive()
    print(lms_) 
    cat("Power:")
    print(lms_$power) 
    cat("\n")
    centiles(lms_, cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  # Wormplots LMS
  output$lms_wormplots <- renderPlot({
    lms_reactive()
    try(wp(lms_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
  })

  ##################################### GAMLSS ####################################################
  
  output$all_gamlss <- renderPlot({
    centiles.com(pb_, cs_, poly_, poly4_, nn_, tr_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, 
                  legend = TRUE, main = "GAMLSS")
  })
  
  output$buttons_gamlss <- renderUI({
    build_gamlss_model()
    print("*** Your GAMLSS are ready! ***")
  })
  
  output$buttons_lms <- renderUI({
    lms_reactive()
    print("*** Your LMS model is ready! ***")
  })
  
  # Centiles Plot with gamlss (P-Splines, Cubic Splines) ######
  output$gamlss_models_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow=c(1,2))
    
    centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 1, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
    centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 1, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Centiles Plot with gamlss (Polynomials Degree 3 and 4) ######
  output$gamlss_models_poly <- renderPlot({
    
    build_gamlss_model()
    par(mfrow=c(1,2))
 
    centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
             ylab = ylab_, pch = 20, cex = 1, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
    centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 1, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Plot fitted models for P-Splines 
  output$gamlss_fitted_pb_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(pb_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Cubic Splines
  output$gamlss_fitted_cs_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(cs_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 3
  output$gamlss_fitted_poly_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 4
  output$gamlss_fitted_poly4_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly4_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_psplines <- renderPrint({
     
    build_gamlss_model()
    suppressWarnings({summary(pb_)})
    cat("\n")
    centiles(pb_, cent=c(2.5,50,97.5), plot=FALSE)
    
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_splines <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(cs_)})
    cat("\n")
    centiles(cs_, cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_poly <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(poly_)})
    cat("\n")
    centiles(poly_,cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_poly4 <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(poly4_)})
    cat("\n")
    centiles(poly4_, cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  # Plot the changed terms for P-Splines
  output$gamlss_term_pb <- renderPlot({

    build_gamlss_model()
    try(plot(pb_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Cubic Splines
  output$gamlss_term_cs <- renderPlot({
    
    build_gamlss_model()
    try(plot(cs_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Polynomial Degree 3
  output$gamlss_term_poly <- renderPlot({
    
    build_gamlss_model()
    try(plot(poly_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                                 c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                 col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Plot the changed terms for Polynomial Degree 4
  output$gamlss_term_poly4 <- renderPlot({
    
    build_gamlss_model()
     try(plot(poly4_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                                  c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                  col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Wormplots from P-Splines and Cubic Splines
  output$wormplots_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
    
    try(wp(pb_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
    try(wp(cs_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
  })
  
  # Wormplots from Polynomials Degree 3 and 4
  output$wormplots_ploy <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
    
    try(wp(poly_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
    try(wp(poly4_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
  })
  
  # Neural Network (machine learning) ##############################################
  
  # Centiles Plot with the Neural Network 
  output$gamlss_net <- renderPlot({
    
    build_gamlss_model()
    centiles(nn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 1,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })

  # Neural Network - Analysis
  output$network_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(nn_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  }) 

  # Neural Network - Analysis
  output$network_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Neural Network - Analysis
  output$net_text<- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(nn_)})
    centiles(nn_,cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  #Wormplots from the Neural Network
  output$nn_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(nn_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
  })
  
  # Decision Tree #################################################################################
  
  # Centiles Plot with the Decision Tree
  output$gamlss_tree <- renderPlot({
    
    build_gamlss_model()
    
    centiles(tr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 1,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Decision Tree - Analysis
  output$tree_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(tr_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                              c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })

  # Decision Tree - Analysis
  output$tree_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Decision Tree - Analysis
  output$tree_text <- renderPrint({
    
    build_gamlss_model()
    print(getSmo(tr_))
    suppressWarnings({summary(tr_)})
    centiles(tr_,cent=c(2.5,50,97.5), plot=FALSE)
  })

  # Plotted Decision Tree
  output$rpart_tree <- renderPlot({
    
    build_gamlss_model()
    rpart.plot(getSmo(tr_), roundint=FALSE, box.palette = "RdBu")
  })
  
  # Wormplots from Decision Tree
  output$tr_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(tr_, ylim.all = 3, col = "cornflowerblue", n.inter= 9))
  })
  
  # Comparism #####################################################################################
  
  # Comparison Table for all GAMLSS and LMS
  output$table_compare <- DT::renderDataTable({
  
    if(lms_ready == TRUE){ 
      build_gamlss_model()
      lms_reactive()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_, lms_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_,lms_, k=3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
      
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(BIC(pb_,cs_,poly_, poly4_, nn_,tr_, lms_))
      BIC_$model <- rownames(BIC_)
      colnames(BIC_) <- c("BIC.df","BIC","model")
      rownames(BIC_) <- c()
      
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("pb_","cs_","poly_","poly4_","nn_","tr_","lms_"), 
                        R2 = c(Rsq(pb_), Rsq(cs_),Rsq(poly_), Rsq(poly4_), Rsq(nn_), Rsq(tr_), Rsq(lms_)))
      
      # Merge the Metrics
      compare_models <- merge(AIC_,GAIC_,by=c("model"))
      compare_models <- merge(compare_models,BIC_,by=c("model"))
      compare_models <- merge(compare_models,R_2,by=c("model"))
      
      compare_models["df"] <- c()
      compare_models["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models$model[compare_models$model == "nn_"] <- "Neural Network"
      compare_models$model[compare_models$model == "cs_"] <- "Cubic Splines"
      compare_models$model[compare_models$model == "poly_"] <- " Polynomials (Degree 3) "
      compare_models$model[compare_models$model == "pb_"] <- "P-Splines"
      compare_models$model[compare_models$model == "tr_"] <- "Decision Tree"
      compare_models$model[compare_models$model == "poly4_"] <- " Polynomials (Degree 4)"
      compare_models$model[compare_models$model == "lms_"] <- "LMS"}
    
    if(lms_ready==FALSE){
      
      build_gamlss_model()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_, k=3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
      
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(BIC(pb_,cs_,poly_, poly4_, nn_,tr_)) 
      BIC_$model <- rownames(BIC_)
      colnames(BIC_) <- c("BIC.df","BIC","model")
      rownames(BIC_) <- c()
      
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("pb_","cs_","poly_","poly4_","nn_","tr_"), 
                        R2 = c(Rsq(pb_), Rsq(cs_),Rsq(poly_), Rsq(poly4_), Rsq(nn_), Rsq(tr_)))
      # Error in solve.default(oout$hessian) : 
      #   Lapack routine dgesv: system is exactly singular: U[4,4] = 0
      
      # Merge the Metrics
      compare_models <- merge(AIC_,GAIC_,by=c("model"))
      compare_models <- merge(compare_models,BIC_,by=c("model"))
      compare_models <- merge(compare_models,R_2,by=c("model"))
      compare_models["df"] <- c()
      compare_models["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models$model[compare_models$model == "nn_"] <- "Neural Network"
      compare_models$model[compare_models$model == "cs_"] <- "Cubic Splines"
      compare_models$model[compare_models$model == "poly_"] <- " Polynomials (Degree 3)"
      compare_models$model[compare_models$model == "pb_"] <- "P-Splines"
      compare_models$model[compare_models$model == "tr_"] <- "Decision Tree"
      compare_models$model[compare_models$model == "poly4_"] <- " Polynomials (Degree 4)"}
    
    compare_models <- compare_models
    
    if(residuals_ready == TRUE){
     
      build_gamlss_model()
      build_outlier()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_, k=3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
    
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(model = c("opb_","ocs_","opoly_","opoly4_","onn_","otr_"),
                        BIC = c(BIC(opb_), BIC(ocs_),BIC(opoly_), BIC(opoly4_), BIC(onn_), BIC(otr_)))
      
    
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("opb_","ocs_","opoly_","opoly4_","onn_","otr_"),
                        R2 = c(Rsq(opb_), Rsq(ocs_),Rsq(opoly_), Rsq(opoly4_), Rsq(onn_), Rsq(otr_)))
      
      # Merge the Metrics
      compare_models_residuals <- merge(AIC_,GAIC_,by=c("model"))
      compare_models_residuals <- merge(compare_models_residuals,BIC_,by=c("model"))
      compare_models_residuals <- merge(compare_models_residuals,R_2,by=c("model"))
      
      compare_models_residuals["df"] <- c()
      compare_models_residuals["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models_residuals$model[compare_models_residuals$model == "onn_"] <- "Neural Network (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "ocs_"] <- "Cubic Splines (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opoly_"] <- " Polynomials (Degree 3) (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opb_"] <- "P-Splines (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "otr_"] <- "Decision Tree (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opoly4_"] <- " Polynomials (Degree 4) (refit)"
      
      compare_models <- rbind(compare_models, compare_models_residuals)
    }
    
    # Round the data
    compare_models <- round_df(compare_models, 3)
    
    row_smallest_aic <- compare_models[which.min(compare_models$AIC),]$AIC
    row_smallest_gaic <- compare_models[which.min(compare_models$GAIC),]$GAIC
    row_smallest_bic <- compare_models[which.min(compare_models$BIC),]$BIC
    biggest_r2 <- compare_models[which.max(compare_models$R2),]$R2
    
    compare_models <<- compare_models
  
    DT::datatable(compare_models, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print'))) %>%
      DT:: formatStyle(columns = "AIC", background = styleEqual(row_smallest_aic, "cornflowerblue")) %>%
      DT:: formatStyle(columns = "GAIC", background = styleEqual(row_smallest_gaic, "indianred")) %>%
      DT:: formatStyle(columns = "BIC", background = styleEqual(row_smallest_bic, "seagreen")) %>%
      DT:: formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) 
  })
  
  # Plot with the Metrics (AIC, GAIC, BIC and R^2)
  output$metrics <- renderPlot({
    
    build_gamlss_model()
    if(lms_ready == TRUE){lms_reactive()}
    
    par(mar = c(12, 3, 3, 3))
    par(mfrow = c(1,2))

    barplot(rbind(compare_models[,2],compare_models[,3],compare_models[,4]), ylab = "Value", 
            ylim = c(min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) - min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100), 
                     max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) + max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100)),
            xpd = FALSE, beside = TRUE, las = 2, names.arg=c(compare_models[,1]), col = c("cornflowerblue","indianred","seagreen3"))
    legend("topright", legend = c("AIC","GAIC","BIC"),
           col = c("cornflowerblue","indianred","seagreen3"), pch = 20)
    
    barplot(compare_models[,5], ylab = "Value",  ylim = c(0, 1), las = 2,
         names.arg=c(compare_models[,1]), col = c("lavender"))
    legend("topright", legend = c("Pseudo R^2"),col = c("lavender"), pch = 20)
  })
  
  ##################################### Prediction ################################################
  
  # Predict new values with the fitted models
  output$gamlss_prediction <- renderPlot({
    
    build_gamlss_model()
    
    if(input$select_model == "lms_ri"){
      if(lms_ready == TRUE){
        lms_reactive()
        
        # Create new x_values with all possible days in the age range
        data_subset <- data_analyte()
        subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
        x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by=1)
        
        lms_ri <<- centiles.pred(lms_, xname="age_lms", xvalues=x_values, cent = c(2.5,50,97.5))}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
        stop()}}

    # Create new x_values with all possible days in the age range
    data_subset <- data_analyte()
    subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
    x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by=1)
    
    pb_ri <<- centiles.pred(pb_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    cs_ri <<- centiles.pred(cs_, xname="age_days",  xvalues=x_values, cent = c(2.5,50,97.5))
    poly_ri <<- centiles.pred(poly_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    poly4_ri <<- centiles.pred(poly4_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    nn_ri <<- centiles.pred(nn_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    tr_ri <<- centiles.pred(tr_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    
    model <- eval(parse(text = input$select_model))
    
    plot(model$x, model$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3, type = "l", col = "indianred", ylim = c(0,max(model$`97.5`)))
    lines(model$x, model$`50`, cex = 0.5, lty = 1, col = "black")
    lines(model$x, model$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
  })

  # Tables of the predicted values ################################################################
  
  # Table for prediction
  output$gamlss_tables <- DT::renderDataTable({
    
    build_gamlss_model()
    
    if(input$select_model == "lms_ri"){
      if(lms_ready == TRUE){
        lms_reactive()
        
        # Create new x_values with all possible days in the age range
        data_subset <- data_analyte()
        subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
        x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by=1)
        
        lms_ri <<- centiles.pred(lms_, xname="age_lms", xvalues=x_values, cent = c(2.5,50,97.5))}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
        stop()}}
    
    model <- eval(parse(text = input$select_model))
    
    if(exists(input$select_model)){
      table_prediction <- data.frame(model)
      colnames(table_prediction) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
      DT::datatable(table_prediction, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the Reference Intervals')) %>%
      DT::formatRound(c(2:4), 2)}
  })

  ##################################### Download ##################################################
  ################################ Predictions #####################################

  output$Download_pb_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_P_Splines.csv")},
    content = function(file) {
      write.csv2(pb_ri, file, row.names = FALSE)})

  output$Download_cs_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Cubic_Splines.csv")},
    content = function(file) {
      write.csv2(cs_ri, file, row.names = FALSE)})

  output$Download_poly_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Poly3.csv")},
    content = function(file) {
      write.csv2(poly_ri, file, row.names = FALSE)})

  output$Download_poly4_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Poly4.csv")},
    content = function(file) {
      write.csv2(poly4_ri, file, row.names = FALSE)})

  output$Download_tr_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Tree.csv")},
    content = function(file) {
      write.csv2(tr_ri, file, row.names = FALSE)})

  output$Download_nn_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Neural_Net.csv")},
    content = function(file) {
      write.csv2(nn_ri, file, row.names = FALSE)})

  output$Download_lms_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_LMS.csv")},
    content = function(file) {
      write.csv2(lms_ri, file, row.names = FALSE)})

  output$download_tree <- downloadHandler(
    filename =  function() {
      "Decision_Tree.eps"},
    content = function(file) {
      setEPS()
      postscript(file)
      build_rpart()
      rpart.plot(rpart_, box.palette = "RdBu", roundint = FALSE)
      dev.off()})
}  

####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
