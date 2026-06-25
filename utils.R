#coonvert a list of species occurrences with coordinates to a data cube
floppydisk2cube <- function(data_in,
                            aggregate_columns,
                            target_grid,
                            grid_crs,
                            seed, y_col='decimalLatitude', x_col='decimalLongitude') {
  
  data_in[[y_col]] <- as.numeric(data_in[[y_col]])
  data_in[[x_col]] <- as.numeric(data_in[[x_col]])
  
  #new coordinates will be created based on randomization found in Oldoni et al.2020
  data_2 <- assign_occurrence_within_uncertainty_circle(data_in, seed, as.character(strsplit(grid_crs$input,':')[[1]][2]), y_col = y_col, x_col = x_col)
  # convert data to a vector layer
  occ <- st_as_sf(data_2, coords = c("x", "y"), crs = grid_crs)

  # project vector layer to EEA grid, in this example EPGS:3035
  occ_proj <- st_transform(occ, crs(target_grid))

  # intersect spatial occurrences with target grid
  occ_eea <- st_intersection(occ_proj, target_grid)

  occ_dat <- as.data.frame(occ_eea)
  
  # create eeacellcode column in data frame
  names(occ_dat)[grep("CellCode", names(occ_dat))] <- "eeacellcode"

  # aggregate occurrences and keep min of uncertainty column

  occ_agg <- as.data.frame(occ_dat %>% group_by(across(all_of(c(aggregate_columns, "eeacellcode")))) %>%
                                  summarise(across('coordinateUncertainty', min), count=n(), .groups="drop")
                            )
  occ_agg$coordinateUncertainty <- as.integer(occ_agg$coordinateUncertainty)
  
  colnames(occ_agg)[which(colnames(occ_agg) == "n")] <- "count"
  
 

  return(occ_agg)
}


filter_missing_coords <- function(data, y_col = "decimalLatitude", x_col = "decimalLongitude") {
  # changed to user-defined columns of coordinates
  data_filt <- data[complete.cases(data[, c(y_col, x_col)]), ]

  return(data_filt)
}

"%nin%" <- Negate("%in%")



check_req_fields <- function(data, req_fields=c("decimalLatitude", "decimalLongitude")) {
  # checks for required fields
  #req_fields argument expects a character vector
  missing_fields <- c()
  for (f in req_fields) {
    if (f %nin% names(data)) {
      missing_fields <- c(missing_fields, f)
    }
  }
  if (!is.null(missing_fields)){
    return(paste("Missing", paste(missing_fields, collapse=', ') , ".", "Please select a dataset containing the following information: ", paste(req_fields, collapse=', '), ""))
  }
}

# load preset grids
#grid_10km <- st_read("eea_grid/Grid_ETRS89-LAEA_10K.shp")
grid_100km <- st_read("eea_grid/Grid_ETRS89-LAEA_100K.shp")


get_corresponding_preset_grid <- function(km) {
  if (km == "10km") {
    
    return(grid_10km)
  } else if (km == "100km") {
    return(grid_100km)
    
  } else if (km == "1km") {
    # not implemented right now because it takes ages to load
    return(grid_1km)
   
  }
}

merge_cubes <- function(new_cube, processed_cube, map_df, col_min) {
  # merge processed cube with a new one e.g. downloaded from GBIF
  
  names(new_cube)[match(map_df$b, names(new_cube))] <- map_df$a
  
  agg_cols <- map_df$a[map_df$a != col_min]
  
  #make sure coordinate uncertainty column is called like this to feed into dplyr
  processed_cube <- processed_cube %>% 
    rename_at(col_min, ~'coordinateUncertaintyInMeters')
  new_cube <- new_cube %>% 
    rename_at(col_min, ~'coordinateUncertaintyInMeters')
  
  
  merged_cube <- bind_rows(processed_cube, new_cube) %>% group_by(across(all_of(agg_cols))) %>%
    summarise(
      coordinateUncertaintyInMeters =
        min(coordinateUncertaintyInMeters, na.rm = TRUE),
      
      count =
        sum(count, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  merged_cube <- merged_cube %>% 
    rename_at('coordinateUncertaintyInMeters', ~col_min)
  
  # delete unnecessary count columns
  merged_cube <- merged_cube %>% select(c(map_df$a, count))
  

  return(merged_cube)
}

get_uncertainty_time_period <- function(time_periods, value, default_na){
  
  for (period in time_periods[[1]]){
    
    years <- str_trim(strsplit(period, ',')[[1]][1])
    year_min <- as.integer(str_trim(strsplit(years, '-')[[1]][1]))
    year_max <- as.integer(str_trim(strsplit(years, '-')[[1]][2]))
    
    period_default_na <- as.integer(str_trim(strsplit(period, ',')[[1]][2]))
    
    if (between(value[1], year_min, year_max)){
      return(period_default_na)
    }
      
  }
  #if a corresponding time period was not found return the previously established default
  return(as.integer(default_na))
  
}

assess_uncertainty <- function(data, default_na = 1000, special_rule=NA) {
  if (is.na(special_rule) ){
    
     data <- data %>% mutate(coordinateUncertainty = ifelse(is.na(coordinateUncertainty), 
                                                                       default_na, coordinateUncertainty))
      
  } else {
    
    time_periods <- strsplit(special_rule, ';')
    
    data <- data %>% mutate(coordinateUncertainty = ifelse(is.na(coordinateUncertainty), 
                                                                   mapply(function(y) get_uncertainty_time_period(time_periods, y, default_na), year), coordinateUncertainty))
    
  }
  
  return(data)
}

assign_occurrence_within_uncertainty_circle <- function(geodata_df, seed, data_projection="4326", y_col = "decimalLatitude", x_col = "decimalLongitude") {
  #gets a random point from within the circle created from the uncertainty values
  #is necessary for random grid allocation
  

  coordinates(geodata_df) <-as.formula(paste("~", x_col, "+", y_col))
  proj4string(geodata_df) <- CRS(paste("+init=epsg",data_projection, sep=":"))
  geodata_df <- spTransform(geodata_df, CRS("+init=epsg:3035"))
  colnames(geodata_df@coords) <- c("x", "y")
  
  nrow_geodata_df <- nrow(geodata_df)
  
  # Set the seed for reproducibility
  set.seed(as.integer(seed) )
  
  geodata_df@data <-
    geodata_df@data %>%
    mutate(random_angle = runif(nrow_geodata_df, 0, 2*pi))
  geodata_df@data <-
    geodata_df@data %>%
    mutate(random_r = sqrt(runif(
      nrow_geodata_df, 0, 1)) * coordinateUncertainty)
  geodata_df@data <-
    geodata_df@data %>%
    mutate(x = geodata_df@coords[, "x"],
           y = geodata_df@coords[, "y"])
  geodata_df@data <-
    geodata_df@data %>%
    mutate(x = x + random_r * cos(random_angle),
           y = y + random_r * sin(random_angle))
  # x` and `y` are the new coordinates while in `@coords` we keep track of the original coordinates:
  geodata_df@data <-
    geodata_df@data %>%
    select(-c(random_angle, random_r))
  
  return(geodata_df)
}


