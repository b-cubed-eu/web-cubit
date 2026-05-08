library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(terra)

source("helpers.R")

# User interface ----
ui <- page_sidebar(
  titlePanel(title= "Cubit"
             #tags$div(
            #   tags$img(src = "Cubit-logo.png", height = "40px", style = "margin-right:10px;"),
             #)
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
    nav_panel("Cube data", 
              
              uiOutput("cube_config_ui"),
              
              
              conditionalPanel(
                #JavaScript expression to evaluate whether the configuration of the cube is finished
              condition = "output.config_done == true",
                #return created cube based on the config once it's done
              tableOutput(outputId = "processed"), 
              downloadButton("downloadData", "Download"))
              ), 
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
  
  #set max size of upload to 30MB
  options(shiny.maxRequestSize=30*1024^2) 
  
  #read uploaded file
  retrieve_file <- reactive({
    req(input$file1)


    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    
    #every occurrences must have corresponding coordinates
    df_filt <- filter_missing_coords(df)
    
    return(df_filt)
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
      return(retrieve_file())
    }
  })

  data_into_cube <- reactive({
    req(retrieve_file())


    if (input$grid_source=='preset'){
      # load pre-built grid (e.g. EEA grid 10 km)
      target_grid <- get_corresponding_preset_grid(input$preset_choice)

    }

    if (input$grid_source == 'custom') {
      req(input$file_grid)
      target_grid <- st_read(input$file_grid$datapath)
    }
    
    # Set the seed for reproducibility
    # if (input$seed) {
    #   set.seed(as.integer(input$seed) )
    # }
    #
    # assign GBIF species key for your specie e.g., Cakile maritima
    #specieskey <- "3048831" # automate specieskey extraction from GBIF
    # define data layer projection
    grid_crs <- st_crs(4326)

    #join user-defined columns for aggregating with the necessary eeacellcode that will be defined from the coordinates
    aggregate_cols <- c('eeacellcode', input$aggregate_cols)
    
    if (isTRUE(input$coordinate_uncertainty_col)){
      if (isTRUE(input$coordinate_uncertainty_na)){
        corrected_uncertainty <- assess_uncertainty(retrieve_file(), coord_uncertainty_col=input$coordinate_uncertainty_col, default_na=input$coordinate_uncertainty_na)
      } else {
        corrected_uncertainty <- assess_uncertainty(retrieve_file(), coord_uncertainty_col=input$coordinate_uncertainty_col)
      }
      #if the user didn't choose anything use defaults
    } else {
      corrected_uncertainty <- assess_uncertainty(retrieve_file())
    }
    
    #main function for cubing data
    floppydatacube <- floppydisk2cube(data_in = corrected_uncertainty, aggregate_columns = aggregate_cols, target_grid=target_grid, grid_crs=grid_crs)
    data_cube <<- floppydatacube

    return(floppydatacube)
  })
  
  config_done <- reactiveVal(FALSE)
  
  #create panel for cube configuration based on data in uploaded file
  output$cube_config_ui <- renderUI({
    
    req(retrieve_file())
    
    if (config_done()) {
      return(NULL)
    }
    
    #get columns from uploaded ful
    cols <- names(retrieve_file())
    
    tagList(
      
      selectInput(
        "aggregate_cols",
        "Columns to aggregate on",
        choices = cols,
        multiple = TRUE
      ),
      
      selectInput(
        "coordinate_uncertainty_col",
        "Coordinate uncertainty column",
        choices = cols,
        multiple = FALSE,
        selected = grep(
          "coordinateUncertainty",
          cols,
          value = TRUE
        )[1]
      ),
      
      numericInput(
        "coordinate_uncertainty_na",
        "Replacement value for missing coordinate uncertainty (meters)",
        value = 1000,
        min = 0
      ),
      
      actionButton(
        "apply_cube_config",
        "Create cube"
      )
    )
  })
  
  #check if necessay options have been set
  observeEvent(input$apply_cube_config, {
    
    req(input$aggregate_cols)
    
    
    config_done(TRUE)
  })
  
  output$config_done <- reactive({
    config_done()
  })
  
  #controls whether a reactive output continues updating when it is hidden in the UI.
  outputOptions(output, "config_done", suspendWhenHidden = FALSE)

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


    df <- read.csv(
      input$new_cube$datapath,
      header = T,
      sep = ","
    )
    return(df)
  })

  output$merged <- renderTable({

      validate(
        need(input$new_cube != "", "Please upload a new cube to merge with the one you just created!")
      )
    
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
