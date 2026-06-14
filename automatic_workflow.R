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
library(R.utils)
#library(data.table)
library(LaF)

source('utils.R')
source('config.R')

proj_dir <- dirname(input_file_datapath)

huge_file <- input_file_datapath
model <- detect_dm_csv(huge_file, sep="\t", header=TRUE)
df.laf <- laf_open(model)

n_lines <- determine_nlines(huge_file)

index_file <- 1


while (index_file < n_lines){
  print(index_file)
  goto(df.laf, index_file)
  df <- next_block(df.laf,nrows=1e5)
  
  # every occurrence must have corresponding coordinates
  df_filt <- filter_missing_coords(df)
  
  if (input_grid_source == "preset") {
    # load pre-built grid (e.g. EEA grid 10 km)
    target_grid <- get_corresponding_preset_grid(input_preset_choice)
  }
  
  if (input_grid_source == "custom") {
    
    target_grid <- st_read(input_file_grid_datapath)
  }
  
  # define data layer projection
  grid_crs <- st_crs(4326)
  
  # join user-defined columns for aggregating with the necessary eeacellcode that will be defined from the coordinates
  aggregate_cols <- c("eeacellcode", input_aggregate_cols)
  
  if (isFALSE(input_use_custom_uncertainty)){
    
    if (isTRUE(input_coordinate_uncertainty_col)) {
      if (isTRUE(input_coordinate_uncertainty_na)) {
        corrected_uncertainty <- assess_uncertainty(df_filt,
                                                    coord_uncertainty_col = input_coordinate_uncertainty_col, 
                                                    default_na = input_coordinate_uncertainty_na)
      } else {
        corrected_uncertainty <- assess_uncertainty(df_filt, 
                                                    coord_uncertainty_col = input_coordinate_uncertainty_col)
      }
      # if the user didn't choose anything use defaults
    } else {
      corrected_uncertainty <- assess_uncertainty(df_filt)
    }
  } else {
    
    
    corrected_uncertainty <- assess_uncertainty(df_filt, coord_uncertainty_col = input_coordinate_uncertainty_col, special_rule=input_custom_uncertainty)
    
   
  }
  
  
  # main function for cubing data
  floppydatacube_cur <- floppydisk2cube(data_in = corrected_uncertainty,
                                    aggregate_columns = aggregate_cols, 
                                    target_grid = target_grid,
                                    grid_crs = grid_crs,
                                    seed=input_seed)
  
  if(exists('floppydatacube_all')){
    floppydatacube_all <- bind_rows(floppydatacube_all, floppydatacube_cur)
  } else {
    floppydatacube_all <- floppydatacube_cur
  }

  index_file <- index_file + 1e5

}

# Source - https://stackoverflow.com/a/8538071
# Posted by Josh O'Brien, modified by community. See post 'Timeline' for change history
# Retrieved 2026-06-11, License - CC BY-SA 3.0

format(Sys.time(), "%S")

save(floppydatacube_all, file = "floppydatacube_all.RData")

merged_chunks <- floppydatacube_all %>% group_by(across(all_of(aggregate_cols))) %>%
  summarise(
    coordinateUncertaintyInMeters =
      min(coordinateUncertaintyInMeters, na.rm = TRUE),
    
    count =
      sum(count, na.rm = TRUE),
    
    .groups = "drop"
  )


write.csv(merged_chunks, output_file_datapath, row.names = FALSE, quote = F )
