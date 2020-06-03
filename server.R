####################################################################################
####### MSPA
####### SEPAL shiny application
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or
# software or in the documentation accompanying it, for program maintenance and
# upgrading as well as for any # damage that may arise from them. FAO also declines
# any responsibility for updating the data and assumes no responsibility for errors
# and omissions in the data provided. Users are, however, kindly asked to report any
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2018/08/16
## mspa / server
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("www/scripts/text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "Français") {
      source("www/scripts/text_french.R", 
             local = TRUE, 
             encoding = "UTF-8")
      #print("fr")
    }
    if (input$language == "Español") {
      source("www/scripts/text_spanish.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("sp")
    }
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  ##################################################################################################################################
  ## Allow to download test data
  output$dynUI_download_test <- renderPrint({
    req(input$download_test_button)
    
    dir.create(file.path("~", "mspa_data_test"),showWarnings = F)
    
    withProgress(message = paste0('Downloading data in ', dirname("~/mspa_data_test/")),
                 value = 0,
                 {
                   system("wget -O ~/mspa_data_test/mspa_input_example.tif  https://github.com/openforis/data_test/raw/master/mspa_input_example.tif")
                   
                 })
    
    list.files("~/mspa_data_test/")
  })
  

  ##################################################################################################################################
  ############### Select Forest-Non Forest mask
  shinyFileChoose(
    input,
    'input_file',
    filetype = "tif",
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ################################# File path
  file_path <- reactive({
    validate(need(input$input_file, "Missing input: Please select time series folder"))
    req(input$input_file)
    df <- parseFilePaths(volumes, input$input_file)
    file_path <- as.character(df[, "datapath"])

  })
  
  ################################# File directory
  data_dir <- reactive({
    req(input$input_file)
    file_path <- file_path()
    paste0(dirname(file_path),"/")
    })

  ##################################################################################################################################
  ############### Parameters title as a reactive
  parameters <- reactive({
    req(input$input_file)

    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    mspa5 <- as.numeric(input$option_dostats)

    paste(mspa1,mspa2,mspa3,mspa4,mspa5,sep=" ")
    
  })
  
  ############### Parameters title as a reactive
  outname <- reactive({
    req(input$input_file)
    
    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    
    paste(mspa1,mspa2,mspa3,mspa4,sep="_")
    
  })
  ##################################################################################################################################
  ############### Insert the start button
  output$StartButton <- renderUI({
    validate(need(input$input_file, "Missing input: select an input binary mask"))
    actionButton('StartButton', textOutput('start_button'))
  })
  
  
  ##################################################################################################################################
  ############### Run MSPA
  mspa_res <- eventReactive(input$StartButton,
                             {
                               req(input$input_file)
                               req(input$StartButton)
                               
                               time_start  <- Sys.time()
                               data_dir    <- data_dir()
                               file_path   <- file_path()
                               
                               file.copy("www/scripts/MSPA/",
                                         data_dir,
                                         recursive = T,
                                         overwrite = T)
                               
                               dir.create(paste0(data_dir,"MSPA/input/"),showWarnings = F)
                               dir.create(paste0(data_dir,"MSPA/output/"),showWarnings = F)
                               dir.create(paste0(data_dir,"MSPA/tmp/"),showWarnings = F)
                               
                               file.copy(file_path,paste0(data_dir,"MSPA/input/input.tif"))
                               
                               parameters <- parameters()
                               write(parameters,paste0(data_dir,"MSPA/input/mspa-parameters.txt"))
                               the_dir <- getwd()
                               
                               setwd(paste0(data_dir,"MSPA"))
                               system("chmod 755 mspa_lin64")
                               print("have a break")
                               withProgress(message = paste0('Processing MSPA for ', file_path),
                                            value = 0,
                                            {
                                              system("bash sepal_mspa")
                                            })
                               
                               setwd(the_dir)
                               
                               process_time <- Sys.time() - time_start
                               
                               write(process_time,paste0(data_dir,"MSPA/output/mspa-process.txt"))
                               
                               raster(paste0(data_dir,"MSPA/output/input_",outname(),".tif"))
                             })
  
  ############### Display Button
  output$display_button <- renderUI({
    req(mspa_res())
    actionButton('display_button',"Display results as map")
  })

  
  ############### Display the results as map
  output$display_res <- renderPlot({
    req(mspa_res())
    req(input$display_button)
    print('Check: Display the map')
    mspa_res <- raster(paste0(data_dir(),"MSPA/output/input_",outname(),".tif"))
    plot(mspa_res, axes = FALSE)
  })
  
  ##################################################################################################################################
  ############### Display parameters
  output$parameterSummary <- renderText({
    req(input$input_file)
    print(paste0("Parameters are : ",parameters()))
  })
  
  ##################################################################################################################################
  ############### Display time
  output$message <- renderText({
    req(mspa_res())
    time <- readLines(paste0(data_dir(),"MSPA/output/mspa-process.txt"))
    str(time)
    print(paste0("Processing time ",time))
    #print(as.character(time))
  })
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
