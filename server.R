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
  ############### Allow to download test data
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
  
  
  ############### PRIMARY INPUT SELECTION
  shinyFileChoose(
    input,
    'input_file',
    filetype = "tif",
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  ############### Select classes to be included as Forest
  output$selectUI_forest <- renderUI({
    print('Check: output$selectUI_cat_forest')
    
    validate(need(statsInput(),"Need stats"))
    
    req(statsInput())
    stats <- statsInput()
    categories <- as.list(unique(stats$map_code))
    
    selectInput(
      "cat_forest",
      label = "Foreground codes",
      choices = categories,
      multiple = TRUE
    )
  })
  
  ############### Select classes to be included as Non Forest
  output$selectUI_noforest <- renderUI({
    print('Check: output$selectUI_cat_nofor')
    
    req(statsInput())
    stats <- statsInput()
    
    req(input$cat_forest)
    cat_forest <- input$cat_forest
    
    ## exclude classes already chosen in forest
    categories <- as.list(unique(stats$map_code[!stats$map_code %in% cat_forest]))
    
    selectInput(
      "cat_nofor",
      label = "Background codes",
      choices = categories,
      multiple = TRUE
    )
    
  })
  
  # ############### Select classes to be included as No Data
  # output$selectUI_nodata <- renderUI({
  #   print('Check: output$selectUI_cat_nodata')
  #   
  #   req(statsInput())
  #   stats <- statsInput()
  #   
  #   req(input$cat_forest)
  #   cat_forest <- input$cat_forest
  #   
  #   req(input$cat_nofor)
  #   cat_nofor <- input$cat_nofor
  #   
  #   ## exclude classes already chosen in forest
  #   categories <- as.list(unique(stats$map_code[!stats$map_code %in% c(cat_forest,cat_nofor)]))
  #   
  #   selectInput(
  #     "cat_nodat",
  #     label = "No Data codes",
  #     choices = categories,
  #     multiple = TRUE,
  #     selected=categories
  #   )
  #   
  # })
  
  
  
  
  ##################################################################################################################################
  ###############        REACTIVE VARIABLES
  
  
  ############### INPUT File path
  file_path <- reactive({
    req(input$input_file)
    df <- parseFilePaths(volumes, input$input_file)
    file_path <- as.character(df[, "datapath"])
    file_path
  })
  
  ###############   INPUT File directory
  input_dir <- reactive({
    req(input$input_file)
    file_path <- file_path()
    paste0(dirname(file_path),"/")
  })
  
  ###############   INPUT File Basename
  the_basename <- reactive({
    req(input$input_file)
    base           <- basename(as.character(file_path()))
    the_basename   <- paste0(substr(base,1,nchar(base)-4))
    
    the_basename
  })
  
  
  ############### RECLASSIFICATION TABLE
  reclass <- reactive({
    #eventReactive(input$ValidateCodesButton,{
    req(input$cat_forest)
    req(input$cat_nofor)
    
    stats <- statsInput()
    
    ### Codes for FOREGROUND are hand selected
    c_for <- as.numeric(input$cat_forest)
    
    ### Codes for BACKGROUND are hand selected
    c_nfr <- as.numeric(input$cat_nofor)
    
    ### Codes for NO DATA are automatically all the rest
    c_ndt <- as.numeric(unique(stats$map_code[!stats$map_code %in% c(c_for,c_nfr)]))
    
    ## Reclass table
    rcl <- data.frame(cbind(c(c_for,c_nfr,c_ndt),
                            c(rep(2,length(c_for)),
                              rep(1,length(c_nfr)),
                              rep(0,length(c_ndt))
                            )))
    rcl
  })
  
  
  ############### Parameters title as a reactive
  parameters <- reactive({
    req(reclass())
    
    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    mspa5 <- as.numeric(input$option_dostats)
    
    paste(mspa1,mspa2,mspa3,mspa4,mspa5,sep=" ")
    
  })
  
  ############### Parameters suffix as a reactive
  parameters_u <- reactive({
    req(reclass())
    
    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    mspa5 <- as.numeric(input$option_dostats)
    
    paste(mspa1,mspa2,mspa3,mspa4,sep="_")
    
  })
  
  
  ##################################################################################################################################
  ########################       REACTIVE NAMES
  
  ############### MASK NAME
  mspa_mask_name <- reactive({
    req(input$input_file)
    paste0(msp_dir,"tmp_mask_mspa_",the_basename(),".tif")
  })
  
  ############### OUTPUT NAME FOR THE TIF
  mspa_output_name <- reactive({
    req(input$input_file)
    req(reclass())
    paste0(msp_dir,"mspa_",the_basename(),"_",parameters_u(),".tif")
  })
  
  ############### OUTPUT NAME FOR THE STATS
  mspa_stat_name <- reactive({
    req(input$input_file)
    req(reclass())
    paste0(msp_dir,"mspa_",the_basename(),"_",parameters_u(),"_stats.txt")
  })
  
  
  
  
  ##################################################################################################################################
  ########################       BUTTONS
  
  ############### VALIDATE INPUT BUTTON
  output$ValidateInputButton <- renderUI({
    validate(need(input$input_file, "Missing input: select an input"))
    actionButton('ValidateInputButton', textOutput('validate_input_button'))
  })
  
  ############### VALIDATE CODES BUTTON
  output$ValidateCodesButton <- renderUI({
    req(input$input_file)
    validate(need(input$cat_nofor, "Missing input: select codes"))
    actionButton('ValidateCodesButton', textOutput('validate_codes_button'))
  })
  
  ############### START PROCESS button
  output$mspaStartButton <- renderUI({
    validate(need(input$input_file, "Missing input: select an input"))
    validate(need(reclass(),"Select the codes for reclassification"))
    actionButton('mspaStartButton', textOutput('mspa_start_button'))
  })
  
  
  ############### DOWNLOAD TIF FILE BUTTON
  output$ui_download_mspa <- renderUI({
    req(mspa_res())
    downloadButton('download_mspa_map',label = textOutput('download_mspa_button'))
  })
  
  ############### DOWNLOAD STAT FILE BUTTON
  output$ui_download_mspa_stat <- renderUI({
    req(mspa_res())
    downloadButton('download_mspa_stat',
                   label = textOutput('download_mspa_stat_button'))
  })
  
  
  
  
  
  ##################################################################################################################################
  ###############     PROCESSES
  
  ############### Compute the code and pixel count table
  statsInput <-  eventReactive(input$ValidateInputButton,
                               {
                                 print('Check: calcul stats')
                                 req(file_path())
                                 
                                 dataname <- file_path()
                                 input_dir <- input_dir()
                                 
                                 stats        <- pixel_count(dataname)
                                 stats$edit   <- stats$V1
                                 names(stats) <- c('map_code', 'map_area', 'map_edited_class')
                                 stats        <- arrange(stats, map_code)
                                 
                                 ############ Final result to be stored in the variable
                                 stats
                                 
                               })
  
  
 
  
  
  ############### Run MSPA
  mspa_res <- eventReactive(input$mspaStartButton,
                            {
                              req(input$mspaStartButton)
                              req(the_basename())
                              req(reclass())
                              the_basename <- the_basename()
                              
                              parameters   <- parameters()
                              parameters_u <- parameters_u()
                              
                              time_start   <- Sys.time()
                              
                              if(!file.exists(mspa_output_name())){
                                
                                tmp_mask <- reclassify(raster(file_path()),reclass())
                                
                                writeRaster(tmp_mask,mspa_mask_name(),
                                            datatype='INT1U',
                                            options="COMPRESS=LZW",
                                            overwrite=T)
                                
                                system(sprintf("cp -r -f %s %s",
                                               paste0("www/scripts/MSPA/*"),
                                               tmp_dir
                                ))
                                
                                system(sprintf("chmod 755 %s",
                                               tmp_dir
                                ))
                                
                                file.copy(mspa_mask_name(),
                                          paste0(tmp_dir,"input/input.tif"),overwrite = T)
                                
                                write(parameters,paste0(tmp_dir,"input/mspa-parameters.txt"))
                                
                                system(sprintf("chmod 755 %s/mspa_lin64",
                                               tmp_dir
                                ))
                                
                                print("have a break")
                                rootdir <- getwd()
                                
                                withProgress(message = paste0('Processing MSPA for ',the_basename),
                                             value = 0,
                                             {
                                               setwd(tmp_dir)
                                               system(sprintf("bash %s/sepal_mspa",
                                                              tmp_dir
                                               ))
                                               setwd(rootdir)
                                             })
                                
                                process_time <- Sys.time() - time_start
                                
                                write(process_time,paste0(tmp_dir,"output/mspa-process.txt"))
                                
                                file.copy(paste0(tmp_dir,"output/input_",parameters_u,".tif"),
                                          mspa_output_name(),
                                          overwrite = T)
                                
                                file.copy(paste0(tmp_dir,"output/input_",parameters_u,"_stat.txt"),
                                          mspa_stat_name(),
                                          overwrite = T)
                              }
                              
                              raster(mspa_output_name())
                            })
  
  
  
  
  ##################################################################################################################################
  ###############     RESULT DISPLAYS
  
  
  ############### Display input file path
  output$file_path_text <- renderText({
    req(input$input_file)
    print(paste0("File chosen is : ",file_path()))
  })
  
  ############### Display parameters
  output$parameterSummary <- renderText({
    req(input$input_file)
    print(paste0("Parameters are : ",parameters()))
  })
  
  ############### Display the codes
  output$mapAreaTable <- renderTable({
    req(statsInput())
    stats <- statsInput()[,1:2]
    names(stats) <- c('Code', 'Pixel count')
    stats
  },
  include.rownames = FALSE)
  
  ############### Display the reclassification table
  output$reclass_table <- renderTable({
    req(reclass())
    reclass()
  },
  include.rownames = FALSE)
  
  ############### Display statistics
  output$mspa_summary <- renderTable({
    req(mspa_res())
    
    the_basename <- the_basename()
    parameters   <- parameters()
    parameters_u <- parameters_u()
    
    #time <- readLines(paste0(tmp_dir,"/output/mspa-process.txt"))
    
    if(file.exists(mspa_stat_name())){
      
      res  <- readLines(mspa_stat_name())
      
      info <- res[20:26]
      out  <- data.frame(cbind(str_split_fixed(info,": ",2)[,1],
                               paste0(
                                 round(
                                   as.numeric(
                                     str_split_fixed(
                                       str_split_fixed(info,": ",2)[,2],
                                       " %",2)[,1]),
                                   1),
                                 " %")
      ))
      names(out) <- c("Class","Proportion")
      
      out
    }else{NULL}
      
  })
  
  
  
  ############### Display the results as map
  output$display_res <- renderPlot({
    req(mspa_res())
    plot(mspa_res(), axes = FALSE)
  })
  
  
  
  
  
  ##################################################################################################################################
  ###############     DOWNLOADS
  
  
  ##################################################################################################################################
  ############### Enable to download the map (.tif)
  output$download_mspa_map <- downloadHandler(
    filename = function() {
      basename(mspa_output_name())
    },
    content  = function(xx) {
      to_export <- raster(mspa_output_name())
      writeRaster(to_export, xx)
    }
  )
  
  
  
  ##################################################################################################################################
  ############### Enable to download the map (.tif)
  output$download_mspa_stat <- downloadHandler(
    filename = function() {
      basename(mspa_stat_name())
    },
    content  = function(xx) {
      to_export <- readLines(mspa_stat_name())
      writeLines(to_export, xx)
    }
  )
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
