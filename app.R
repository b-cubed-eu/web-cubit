library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(terra)

#setwd('C:/Users/Rui/Documents/b3/Cubit')
#setwd("/srv/shiny-server/Cubit")
source("helpers.R")

# User interface ----
ui <- page_sidebar(
  titlePanel(title= "Cubit",
             tags$div(
               tags$img(src = "Cubit-logo.png", height = "40px", style = "margin-right:10px;"),
             )
  ),
  
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
    
    selectInput("grid_source", "Choose grid:",
                choices = c("Use built-in grid" = "preset",
                            "Upload your own" = "custom")),
    
    # Show preset dropdown only if "preset" selected
    conditionalPanel(
      condition = "input.grid_source == 'preset'",
      selectInput("preset_choice", "Select a preset grid:",
                  choices = c("Grid 100km" = "100km",
                              "Grid 10km" = "10km", 
                              "Grid 1km" = "1km"))
    ),
    
    # Show file upload only if "custom" selected
    conditionalPanel(
      condition = "input.grid_source == 'custom'",
      fileInput("file_grid", "Upload your file:", accept = ".gpkg")
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
    # Merge with another cube
    nav_panel("Merge Cubes",
              fileInput(
                "new_cube",
                "Choose CSV File",
                multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              tableOutput(outputId = "merged"), 
              downloadButton("downloadMerged", "Download"))
  
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
  
  get_target_grid <- reactive({
    
  })
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    validate(
      need(input$file1 != "", "Please select a data set")
    )
    
    
   if (input$disp == "head") {
      return(head(retrieve_file()))
    } else {
      return(retrieve_file)
    }
  })
  
  data_into_cube <- reactive({
    req(retrieve_file())
    
    
    if (input$grid_source=='preset'){
      # load grid (e.g. EEA grid 10 km)
      
      target_grid <- st_read(get_corresponding_preset_grid(input$preset_choice)) #get_corresponding_preset_grid(input$preset_choice)
    } 
    
    if (input$grid_source == 'custom') {
      req(input$file_grid)
      target_grid <- st_read(input$file_grid$datapath)
    }
      
    # assign GBIF species key for your specie e.g., Cakile maritima
    #specieskey <- "3048831" # automate specieskey extraction from GBIF
    # define data layer projection
    grid_crs <- st_crs(4326) 
    
    floppydatacube <- floppydisk2cube(retrieve_file(), target_grid, grid_crs)
    data_cube <<- floppydatacube
    
    return(floppydatacube)
  })
  
  output$processed <- renderTable({
    
    validate(
      need(input$file1 != "", Cubit_error_message)
    )
    
    validate(
      check_req_fields(retrieve_file())
    )
    
    output_cube <- data_into_cube()
    
    
    
    
    return(output_cube)
    
  })
  
  retrieve_new_cube_file <- reactive({
    req(input$new_cube)
    
    print('pleasework')
    df <- read.csv(
      input$new_cube$datapath,
      header = T,
      sep = ","
    )
    return(df)
  })
  
  output$merged <- renderTable({
    
     # validate(
     #   need(input$new_cube != "", "Please upload a new cube to merge with the one you just created!")
     # ),
    #merged_cube <- retrieve_new_cube_file()
    merged_cube <- merge_cubes(retrieve_new_cube_file(), data_into_cube())
    
    return(merged_cube)
  } 
  )
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename="processed_data.csv",
    content = function(file) {
      req(input$file1)
      req(data_into_cube())
      write.csv(data_into_cube(), file, row.names = FALSE, quote=F)
    }
  )
  
  output$downloadMerged <- downloadHandler(
    filename="merged_data.csv",
    content = function(file) {
      req(input$new_cube)
      req(retrieve_new_cube_file())
      write.csv(merge_cubes(retrieve_new_cube_file(), data_into_cube()), file, row.names = FALSE, quote=F)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
