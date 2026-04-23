library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(terra)

setwd('C:/Users/Rui/Documents/b3/Cubit')
source("helpers.R")

# User interface ----
ui <- page_sidebar(
  title = "FloppyDisk2Cube",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Select a file ----
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = TRUE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons(
      "sep",
      "Separator",
      choices = c(
        Comma = ",",
        Semicolon = ";",
        Tab = "\t"
      ),
      selected = ","
    ),
    
    # Input: Select quotes ----
    radioButtons(
      "quote",
      "Quote",
      choices = c(
        None = "",
        "Double Quote" = '"',
        "Single Quote" = "'"
      ),
      selected = '"'
    ),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons(
      "disp",
      "Display",
      choices = c(
        Head = "head",
        All = "all"
      ),
      selected = "head"
    )
  ),
  
 
  
  # Output: Data file ----
  navset_card_underline(
    # Show scatterplot
    nav_panel("Input data", tableOutput(outputId = "contents")),
    # Show data table
    nav_panel("Cube data", tableOutput(outputId = "processed"), downloadButton("downloadData", "Download")), 
    # Show temporal decay plot (to be implemented)
    #nav_panel("Temporal Degradation", )
  )
)

server <- function(input, output) {
  
  retrieve_file <- reactive({
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    return(df)
  })
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
   if (input$disp == "head") {
      return(head(retrieve_file()))
    } else {
      return(retrieve_file)
    }
  })
  
  data_into_cube <- reactive({
    # load grid (e.g. EEA grid 10 km)
    target_grid <- st_read("eea_grid/Grid_ETRS89-LAEA_10K.shp")
    # assign GBIF species key for your specie e.g., Cakile maritima
    specieskey <- "3048831" # automate specieskey extraction from GBIF
    # define data layer projection
    grid_crs <- st_crs(4326) 
    
    floppydatacube <- floppydisk2cube(retrieve_file(), target_grid, specieskey, grid_crs)
    data_cube <<- floppydatacube
    
    return(floppydatacube)
  })
  
  output$processed <- renderTable({
    
    return(data_into_cube())
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename="processed_data.csv",
    content = function(file) {
      write.csv(data_into_cube(), file, row.names = FALSE, quote=F)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
