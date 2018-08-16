############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "MSPA analysis" })

output$t0_title <- reactive({  "MSPA" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Download test dataset"})
output$start_button             <- reactive({'Launch MSPA calculation'})


#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$title_language <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$title_description <- reactive({"Description"})

output$body_description  <- reactive({
  HTML(paste0(
    "Perform a MSPA analysis on a binary mask. Values of the mask are:
    <br/>
    - Foreground value 2<br/>
    - Background value 1<br/>
    - NoData value     0<br/>
    <br/>
    Adapted from ",
    a(href="http://forest.jrc.ec.europa.eu/download/software/guidos/mspa/"," Guidos Toolbox developed by JRC"),
    "<br/>For support ask",
    a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})


output$title_download_testdata <- reactive({"Download test data"})

############################ INTRODUCTION TAB - BOX 2
output$body_ts_dir  <- reactive({
  HTML(paste0(
    "Choose the binary file "
    )
    )})

############################ INTRODUCTION TAB - BOX 5
output$title_result <- reactive({"Results"})


############################ INTRODUCTION TAB - BOX 4
output$title_disclaimer <- reactive({"Disclaimer"})

output$body_disclaimer  <- reactive({
  HTML(paste0(
    "FAO declines all responsibility for errors or deficiencies in the database 
    or software or in the documentation accompanying it for program maintenance and 
    upgrading as well as for any damage that may arise from them.<br/>
    FAO also declines any responsibility for updating the data and assumes 
    no responsibility for errors and omissions in the data provided.<br/>
    Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
))})







