library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(terra)
library(shinyjs)
library(stringr)
library(sp)

source("utils.R")

# User interface ----
ui <- page_sidebar(
  useShinyjs(),
  titlePanel(
    title = "Cubit"
   
  ),

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    tags$div(
      tags$img(src = "Cubit-logo.png", height="100%", width="100%", style = "margin-right:10px;")
    ),
    # Input: Select a file ----
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c(
        "text/csv",
        ".csv",
        "text/tsv",
        ".tsv"
      )
    ),
    selectInput("grid_source", "Choose grid:",
      choices = c(
        "Use built-in grid" = "preset",
        "Upload your own" = "custom"
      )
    ),

    # Show preset dropdown only if "preset" selected
    conditionalPanel(
      condition = "input.grid_source == 'preset'",
      selectInput("preset_choice", "Select a preset grid:",
        choices = c(
          "EEA 100km" = "100km",
          "EEA 10km" = "10km",
          "EEA 1km" = "1km"
        )
      )
    ),

    # Show file upload only if "custom" selected
    conditionalPanel(
      condition = "input.grid_source == 'custom'",
      fileInput("file_grid", "Upload your file:", accept = ".gpkg"),
      
      numericInput(
        "grid_crs",
        "Indicate data layer projection (EPSG code)",
        value = 4326,
        min = 0
      )
    ),


    # Horizontal line ----
    tags$hr(),
    
    uiOutput("csv_options_ui"),

    # Horizontal line ----
    tags$hr(),

   
  ),


  # Output: Data file ----
  navset_card_underline(
    # Show scatterplot
    nav_panel("Input data", tableOutput(outputId = "contents")),
    # Show data table
    nav_panel(
      "Cube data",
      uiOutput("cube_config_ui"),
      conditionalPanel(
        # JavaScript expression to evaluate whether the configuration of the cube is finished
        condition = "output.config_done == true",
        # return created cube based on the config once it's done
        tableOutput(outputId = "processed"),
        downloadButton("downloadData", "Download")
        
        
        
      )
    ),
    # Merge with another cube
    
    nav_panel(
      "Merge Cubes",
      
      fileInput(
        "new_cube",
        "Choose second cube (CSV/TSV)",
        accept = c(".csv", ".tsv")
      ),
      
      
      tags$hr(),
      uiOutput("premadecube_config_ui"),
      tags$hr(),
      
      uiOutput("mapping_builder_ui"),
      
      actionButton("add_mapping", "➕ Add mapping"),
      actionButton("run_merge", "Merge cubes"),
      
      tags$hr(),
      
      tableOutput("merged"),
      downloadButton("downloadMerged", "Download")
    )

   
  )
)

server <- function(input, output) {
  # set max size of upload to 30MB
  options(shiny.maxRequestSize = 200 * 1024^2)
  #display generic error message instead of full error trace
  #options(shiny.sanitize.errors = TRUE)
  output$csv_options_ui <- renderUI({
    
    req(input$file1)
    
    tagList(
    
      
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
    actionButton(
      "load_file",
      "Load file"
    )
    )
      
    
  })

  # read uploaded file
  retrieve_file <- eventReactive(input$load_file, {
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = T, #file must always have an header
      sep = input$sep,
      quote = input$quote
    )

    

    return(df)
  })
  

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    req(retrieve_file())

    validate(
      need(!is.null(input$file1), "Please select a data set")
    )
    
    

    
    
    return(head(retrieve_file()))
    
  })

  data_into_cube <- reactive({
    req(retrieve_file())


    if (input$grid_source == "preset") {
      # load pre-built grid (e.g. EEA grid 10 km)
      target_grid <- get_corresponding_preset_grid(input$preset_choice)
    }

    if (input$grid_source == "custom") {
      req(input$file_grid)
      target_grid <- st_read(input$file_grid$datapath)
    }

    # define data layer projection
    if (input$grid_crs){
      grid_crs <- st_crs(input$grid_crs)
    } else {
      grid_crs <- st_crs(4326) # default to WGS84
    }
    
    
    # every occurrence must have corresponding coordinates
    df_filt <- filter_missing_coords(retrieve_file(), y_col = input$y_col, x_col = input$x_col)
    
    if(input$coordinate_uncertainty_col %in% names(df_filt)){
    
      df_filt <- df_filt %>% rename_at(input$coordinate_uncertainty_col, ~'coordinateUncertainty')
    } else{
      df_filt[ , 'coordinateUncertainty'] <- NA
    }
    
    if (isFALSE(input$use_custom_uncertainty)){
      
     
     
        corrected_uncertainty <- assess_uncertainty(df_filt,
                                default_na = input$coordinate_uncertainty_na)
      
    } else {
      
      
        corrected_uncertainty <- assess_uncertainty(df_filt, special_rule=input$custom_uncertainty)
      
      }
        
    
    # main function for cubing data
    
    floppydatacube <- floppydisk2cube(data_in = corrected_uncertainty,
                                     aggregate_columns = input$aggregate_cols, 
                                     target_grid = target_grid,
                                     grid_crs = grid_crs,
                                     seed=input$seed,
                                     y_col=input$y_col,
                                     x_col=input$x_col)
    
    floppydatacube <- floppydatacube %>% rename_at('coordinateUncertainty', ~input$coordinate_uncertainty_col)
    
    data_cube <- floppydatacube

    return(floppydatacube)
  })

  # create panel for cube configuration based on data in uploaded file
  output$cube_config_ui <- renderUI({
    req(retrieve_file())

    # get columns from uploaded file
    cols <- names(retrieve_file())
    
    fluidRow(
      column(6, 
        tagList(
          selectInput(
            "aggregate_cols",
            "Columns to aggregate on",
            choices = cols,
            multiple = TRUE
          ),
          selectInput(
            "coordinate_uncertainty_col",
            "Coordinate uncertainty",
            choices =c("None" = "", cols),
            multiple = FALSE,
            selected = {
              hit <- grep("coordinateUncertainty", cols, value = TRUE)[1]
              if (is.na(hit)) NULL else hit
            }
          ),
          numericInput(
            "seed",
            "Establish seed for random grid allocation",
            value = 42,
            min = 0
          )
          
        )
      ),
      column(6,
        tagList(
          selectInput(
            "y_col",
            "Y-coordinate/Latitude",
            choices =c("None" = "", cols),
            multiple = FALSE,
            selected = {
              hit <- grep("Latitude", cols, value = TRUE, ignore.case=TRUE)[1]
              if (is.na(hit)) NULL else hit
            }
          ),
          selectInput(
            "x_col",
            "X-coordinate/Longitude",
            choices =c("None" = "", cols),
            multiple = FALSE,
            selected = {
              hit <- grep("Longitude", cols, value = TRUE, ignore.case=TRUE)[1]
              if (is.na(hit)) NULL else hit
            }
          ),
          numericInput(
            "coordinate_uncertainty_na",
            "Replacement value for missing coordinate uncertainty (meters)",
            value = 1000,
            min = 0
          ),
          checkboxInput(
            "use_custom_uncertainty",
            "Specify different uncertainty values for time periods",
            value = FALSE
          ),
          
          disabled(textInput(
            "custom_uncertainty",
            "Custom uncertainty (m): e.g. 2000-2010, 500; 2011-2020, 200; 2021-2026, 50",
          )),
            
          
        )
      ),
      actionButton(
        "apply_cube_config",
        "Create cube"
      )
    )
  })
  
  observe({
    
    if (isTRUE(input$use_custom_uncertainty)) {
      
      shinyjs::enable("custom_uncertainty")
      
    } else {
      
      shinyjs::disable("custom_uncertainty")
    }
  })
  
  config_done <- reactiveVal(FALSE)
  
  # check if necessay options have been set
  observeEvent(input$apply_cube_config, {
    req(input$aggregate_cols)

    config_done(TRUE)
  })
  
 
  output$config_done <- reactive({
    
    config_done()
  })

  # controls whether a reactive output continues updating when it is hidden in the UI.
  outputOptions(output, "config_done", suspendWhenHidden = FALSE)

  output$processed <- renderTable({
    validate(
      need(input$file1 != "", 'Please select a valid dataset')
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
  
  #this will create the column mapping ui for the cubes
  #corresponding to coordinate uncertainty and occurrence counts 
  #cause they must be processed differently
  #it will try to get the columns by itself based on names
  output$premadecube_config_ui <- renderUI({
    req(data_into_cube())
    
    # get columns from uploaded file
    cols <- names(data_into_cube())
    cols2 <- names(retrieve_new_cube_file())
    
    fluidRow(
      column(6, 
             tagList(
               
               selectInput(
                 "cubeA_uncertainty_col",
                 "Coordinate uncertainty",
                 choices = c(cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("coordinateUncertainty", cols, value = TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               
               selectInput(
                 "cubeA_count_col",
                 "Number of occurrences",
                 choices =c("None" = "", cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("count$", cols, value = TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               
               
             )
      ),
      column(6,
             selectInput(
               "cubeB_uncertainty_col",
               "Coordinate uncertainty",
               choices =c("None" = "", cols2),
               multiple = FALSE,
               selected = {
                 hit <- grep("coordinateUncertainty", cols2, value = TRUE)[1]
                 if (is.na(hit)) NULL else hit
               }
             ),
             
             selectInput(
               "cubeB_count_col",
               "Number of occurrences",
               choices =c("None" = "", cols2),
               multiple = FALSE,
               selected = {
                 hit <- grep("count$", cols2, value = TRUE)[1]
                 if (is.na(hit)) NULL else hit
               }
             )
             
             
             
             
      )
    )
    
    
  })
  
  
  mapping_counter <- reactiveVal(0)
  
  observeEvent(input$add_mapping, {
    
    i <- mapping_counter() + 1
    mapping_counter(i)
    
    cols_a <- names(data_into_cube())
    cols_b <- names(retrieve_new_cube_file())
    
    insertUI(
      selector = "#mapping_container",
      where = "beforeEnd",
      ui = div(
        id = paste0("map_row_", i),
       
        fluidRow(
          
          #row with input fields that will appear after pressing "add mapping" button       
          column(5,
                 selectInput(
                   paste0("map_a_", i),
                   label = NULL,
                   choices = c("-- skip --" = "", cols_a)
                 )
          ),
          
          column(5,
                 selectInput(
                   paste0("map_b_", i),
                   label = NULL,
                   choices = c("-- skip --" = "", cols_b)
                 )
          ),
          
          column(2,
                 actionButton(paste0("remove_", i), "✖")
          )
        )
      )
    )
  })
  
  observe({
    
    lapply(seq_len(mapping_counter()), function(i) {
      
      observeEvent(input[[paste0("remove_", i)]], {
        
        removeUI(selector = paste0("#map_row_", i))
      }, ignoreInit = TRUE)
    })
  })
  
  #remove all mapping when a new cube file is selected
  remove_mapping <- function(){
    
    lapply(seq_len(mapping_counter()), function(i){
      removeUI(selector = paste0("#map_row_", i))
    })
    
    mapping_counter(0)
  }
  
  output$mapping_builder_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(5, strong("Cube A")) ,
        column(5, strong("Cube B")) ,
        column(2, "")
      ),
      div(id = "mapping_container")
    )
  })
  
  merge_mapping <- reactive({
    
    n <- mapping_counter()
    
    maps <- lapply(seq_len(n), function(i) {
      
      a <- input[[paste0("map_a_", i)]]
      b <- input[[paste0("map_b_", i)]]
      
      if (is.null(a) || a == "" || is.null(b) || b == "") {
        return(NULL)
      }
      
      data.frame(a = a, b = b)
    })
    
    do.call(rbind, maps)
  })
  
  merged_data <- eventReactive(input$run_merge, {
    
    req(data_into_cube(), retrieve_new_cube_file())
    
    map <- merge_mapping()
    print(map)
    validate(
      need(nrow(map) > 0, "Please define at least one mapping")
    )
    
    #make sure coordinate uncertainty column is called like this to feed into dplyr in merge function
    cubeA2merge <- data_into_cube() %>% 
      rename("coordinateUncertainty" = input$coordinate_uncertainty_col)
    cubeA2merge_stay <<- cubeA2merge
    
    cubeB2merge <- retrieve_new_cube_file() %>% 
      rename("coordinateUncertainty" = input$cubeB_uncertainty_col, "count" = input$cubeB_count_col)
    cubeB2merge_stay <<- cubeB2merge
   
    merged_cube <- merge_cubes(
      cubeA2merge,
      cubeB2merge,
      map
    )
    
    merged_cube <- merged_cube %>% 
      rename(!!input$coordinate_uncertainty_col := "coordinateUncertainty")
    
    return(merged_cube)
    
  })
  
  output$merged <- renderTable({

    validate(
       need(input$new_cube != "", "Please upload a new cube to merge with the one you just created!")
     )

    merged_data()
  })
  
  output$downloadMerged <- downloadHandler(
    filename = "merged_cube.csv",
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE, quote=F)
    }
  )

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "processed_data.csv",
    content = function(file) {
      req(input$file1)
      req(data_into_cube())
      write.csv(data_into_cube(), file, row.names = FALSE, quote = F)
    }
  )

 
}

# Create Shiny app ----
shinyApp(ui, server)
