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
    # tags$div(
    #   tags$img(src = "Cubit-logo.png", height = "40px", style = "margin-right:10px;"),
    # )
  ),

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Select a file ----
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c(
        "text/csv",
        ".csv"
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
          "Grid 100km" = "100km",
          "Grid 10km" = "10km",
          "Grid 1km" = "1km"
        )
      )
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
    nav_panel(
      "Cube data",
      uiOutput("cube_config_ui"),
      conditionalPanel(
        # JavaScript expression to evaluate whether the configuration of the cube is finished
        condition = "output.config_done == true",
        # return created cube based on the config once it's done
        tableOutput(outputId = "processed"),
        downloadButton("downloadData", "Download") ,
        
        
      )
    ),
    # Merge with another cube
   # nav_panel(
      # "Merge Cubes original",
      # fileInput(
      #   "new_cube",
      #   "Choose CSV File",
      #   multiple = TRUE,
      #   accept = c(
      #     "text/csv",
      #     "text/comma-separated-values,text/plain",
      #     ".csv"
      #   )
      # ),
      # 
      # 
      # 
      # tableOutput(outputId = "merged"),
      # 
      # downloadButton("downloadMerged", "Download")
    #),
    
    nav_panel(
      "Merge Cubes",
      
      fileInput(
        "new_cube",
        "Choose second cube (CSV)",
        accept = c(".csv")
      ),
      
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
  options(shiny.maxRequestSize = 30 * 1024^2)
  #display generic error message instead of full error trace
  #options(shiny.sanitize.errors = TRUE)

  # read uploaded file
  retrieve_file <- reactive({
    req(input$file1)


    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    # every occurrences must have corresponding coordinates
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


    if (input$grid_source == "preset") {
      # load pre-built grid (e.g. EEA grid 10 km)
      target_grid <- get_corresponding_preset_grid(input$preset_choice)
    }

    if (input$grid_source == "custom") {
      req(input$file_grid)
      target_grid <- st_read(input$file_grid$datapath)
    }

    # Set the seed for reproducibility
    # if (input$seed) {
    #   set.seed(as.integer(input$seed) )
    # }
    #
    # assign GBIF species key for your specie e.g., Cakile maritima
    # specieskey <- "3048831" # automate specieskey extraction from GBIF
    # define data layer projection
    grid_crs <- st_crs(4326)

    # join user-defined columns for aggregating with the necessary eeacellcode that will be defined from the coordinates
    aggregate_cols <- c("eeacellcode", input$aggregate_cols)
    
    if (isFALSE(input$use_custom_uncertainty)){

      if (isTRUE(input$coordinate_uncertainty_col)) {
        if (isTRUE(input$coordinate_uncertainty_na)) {
          corrected_uncertainty <- assess_uncertainty(retrieve_file(),
                                  coord_uncertainty_col = input$coordinate_uncertainty_col, 
                                  default_na = input$coordinate_uncertainty_na)
        } else {
          corrected_uncertainty <- assess_uncertainty(retrieve_file(), 
                                   coord_uncertainty_col = input$coordinate_uncertainty_col)
        }
        # if the user didn't choose anything use defaults
      } else {
        corrected_uncertainty <- assess_uncertainty(retrieve_file())
      }
    } else {
      
      
        corrected_uncertainty <- assess_uncertainty(retrieve_file(), coord_uncertainty_col = input$coordinate_uncertainty_col, special_rule=input$custom_uncertainty)
      
        #I need to check for user input for custom coord uncertainty and if it does not exist return error
        #print('return error')
      }
        
   
    # main function for cubing data
    floppydatacube <- floppydisk2cube(data_in = corrected_uncertainty,
                                     aggregate_columns = aggregate_cols, 
                                     target_grid = target_grid,
                                     grid_crs = grid_crs)
    #Will this cause issues in the server? Do I really need this to be global?
    data_cube <<- floppydatacube

    return(floppydatacube)
  })

  config_done <- reactiveVal(FALSE)

  # create panel for cube configuration based on data in uploaded file
  output$cube_config_ui <- renderUI({
    req(retrieve_file())

    if (config_done()) {
      return(NULL)
    }

    # get columns from uploaded file
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
        choices =c("None" = "", cols),
        multiple = FALSE,
        selected = {
          hit <- grep("coordinateUncertainty", cols, value = TRUE)[1]
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
        #fluidRow(
          
          # column(
          #   5,
          #   selectInput(
          #     paste0("map_a_", i),
          #     "Cube A column",
          #     choices = c("-- skip --" = "", cols_a)
          #   )
          # ),
          # 
          # column(
          #   5,
          #   selectInput(
          #     paste0("map_b_", i),
          #     "Cube B column",
          #     choices = c("-- skip --" = "", cols_b)
          #   )
          # ),
          # 
          # column(
          #   2,
          #   actionButton(paste0("remove_", i), "✖")
          # )
          
          
        #)
        
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
    
    validate(
      need(nrow(map) > 0, "Please define at least one mapping")
    )
    
    by_a <- map$a
    by_b <- map$b
    print(map)
    merge_cubes(
      data_into_cube(),
      retrieve_new_cube_file(),
      by_a,
      by_b,
      
    )
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
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  
  # output$merged <- renderTable({
  #   validate(
  #     need(input$new_cube != "", "Please upload a new cube to merge with the one you just created!")
  #   )
  # 
  #   merged_cube <- merge_cubes(retrieve_new_cube_file(), data_into_cube())
  # 
  #   return(merged_cube)
  # })


  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "processed_data.csv",
    content = function(file) {
      req(input$file1)
      req(data_into_cube())
      write.csv(data_into_cube(), file, row.names = FALSE, quote = F)
    }
  )

  # output$downloadMerged <- downloadHandler(
  #   filename = "merged_data.csv",
  #   content = function(file) {
  #     req(input$new_cube)
  #     req(retrieve_new_cube_file())
  #     write.csv(merge_cubes(retrieve_new_cube_file(), data_into_cube()), file, row.names = FALSE, quote = F)
  #   }
  # )
}

# Create Shiny app ----
shinyApp(ui, server)
