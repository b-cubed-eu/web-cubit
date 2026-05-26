#coonvert a list of species occurrences with coordinates to a data cube
floppydisk2cube <- function(data_in,
                            aggregate_columns,
                            uncertainty_columns = c("coordinateUncertaintyInMeters"),
                            target_grid,
                            grid_crs,
                            seed) {
  
  #new coordinates will be created based on randomization found in Oldoni et al.2020
  data_2 <- assign_occurrence_within_uncertainty_circle(data_in, seed)
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
                                  summarise(across('coordinateUncertaintyInMeters', min), count=n(), .groups="drop")
                            )
  occ_agg$coordinateUncertaintyInMeters <- as.integer(occ_agg$coordinateUncertaintyInMeters)
  
  colnames(occ_agg)[which(colnames(occ_agg) == "n")] <- "count"

  return(occ_agg)
}


filter_missing_coords <- function(data) {
  # might change to user-defined columns of coordinates
  i_lat <- grep("decimalLatitude", names(data))
  i_long <- grep("decimalLongitude", names(data))

  if (i_lat + 1 != i_long) {
    stop("Longitude column must be immediately after Latitude")
  }

  data_filt <- data[complete.cases(data[, c("decimalLatitude", "decimalLongitude")]), ]

  return(data_filt)
}

"%nin%" <- Negate("%in%")

Cubit_error_message <- "Please select a dataset containing the following information: 
countryCode, scientificName, decimalLatitude, decimalLongitude, year, specieskey."


check_req_fields <- function(data) {
  # check for countryCode, scientificName, decimalLatitude, decimalLongitude, year

  req_fields <- c("countryCode", "scientificName", "decimalLatitude", "decimalLongitude", "year", "speciesKey")

  for (f in req_fields) {
    if (f %nin% names(data)) {
      return(paste("countryCode column not found. ", Cubit_error_message, ""))
    }
  }


  # must be before missing coords
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

merge_cubes <- function(new_cube, processed_cube, map_df) {
  # merge processed with a new cube e.g. downloaded from GBIF
  #to do: keep min of coordinate uncertainty

  names(new_cube)[match(map_df$b, names(new_cube))] <- map_df$a
  
  merged_cube <- merge(processed_cube, new_cube, 
                      by=map_df$a,
                      all.x = T, all.y = T)
  
  #create a new column with the total of both cube's occurrences for each cell
  merged_cube <- merged_cube %>% mutate(count = as.integer(rowSums(across(c(count.x, count.y)))))

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

assess_uncertainty <- function(data, coord_uncertainty_col = "coordinateUncertaintyInMeters", default_na = 1000, special_rule=NA) {
  if (is.na(special_rule) ){
    
      if (coord_uncertainty_col %in% names(data)) {
        
        data <- data %>% mutate(coordinateUncertaintyInMeters = ifelse(is.na(coordinateUncertaintyInMeters), 
                                                                       default_na, coordinateUncertaintyInMeters))
      } else {
        data$coordinateUncertaintyInMeters <- default_na
      }
    
  } else {
    
    time_periods <- strsplit(special_rule, ';')
    
    data <- data %>% mutate(coordinateUncertaintyInMeters = ifelse(is.na(coordinateUncertaintyInMeters), 
                                                                   mapply(function(y) get_uncertainty_time_period(time_periods, y, default_na), year), coordinateUncertaintyInMeters))
    
  }
  
  return(data)
}

assign_occurrence_within_uncertainty_circle <- function(geodata_df, seed){
  #gets a random point from within the circle created from the uncertainty values
  #is necessary for random grid allocation
  

  coordinates(geodata_df) <- ~decimalLongitude+decimalLatitude
  proj4string(geodata_df) <- CRS("+init=epsg:4326")
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
      nrow_geodata_df, 0, 1)) * coordinateUncertaintyInMeters)
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

# need to add a way to download metadata on the generation of the cube (see deliverable 2.1 - section 3.4)
