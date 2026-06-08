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
library(data.table)

source('utils.R')
source('config.R')

#args = commandArgs(trailingOnly=FALSE, asValues=TRUE)
#print(args)

proj_dir <- dirname(input_file_datapath)

files2process <- list.files(proj_dir, pattern='.temp.csv', full.names=TRUE)
print(files2process)
for (file in files2process){
  
df <- read.csv(
  file, 
  header = input_header,
  sep = input_sep,
  #quote = input_quote
)

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
floppydatacube <- floppydisk2cube(data_in = corrected_uncertainty,
                                  aggregate_columns = aggregate_cols, 
                                  target_grid = target_grid,
                                  grid_crs = grid_crs,
                                  seed=input_seed)

print(file)

#outputfilename should be args
output_name <- gsub('temp', 'temp_cube', file)
print(output_name)
write.csv(floppydatacube, output_name, row.names = FALSE, quote = F )
}


