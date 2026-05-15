#coonvert a list of species occurrences with coordinates to a data cube
floppydisk2cube <- function(data_in,
                            aggregate_columns,
                            uncertainty_columns = c("coordinateUncertaintyInMeters"),
                            target_grid,
                            grid_crs) {
  # convert data to a vector layer
  occ <- st_as_sf(data_in, coords = c("decimalLongitude", "decimalLatitude"), crs = grid_crs)

  # project vector layer to EEA grid, in this example EPGS:3035
  occ_proj <- st_transform(occ, crs(target_grid))
  
  #create buffer based on coordinate uncertainty - randomized grid allocation
  #occ_proj <- occ_proj %>% st_buffer(., .$coordinateUncertaintyInMeters) 
  #right now it creates problems with the coord uncertainty in the cube

  # intersect spatial occurrences with target grid
  occ_eea <- st_intersection(occ_proj, target_grid)

  occ_dat <- as.data.frame(occ_eea)

  # create eeacellcode column in data frame
  names(occ_dat)[grep("CellCode", names(occ_dat))] <- "eeacellcode"

  # aggregate occurrences and keep min of uncertainty column

  occ_agg <- as.data.frame(occ_dat %>% group_by(across(all_of(c('species', 'eeacellcode')))) %>%
                                  summarise(across('coordinateUncertaintyInMeters', min), count=n(), .groups="drop")
                            )

  # occ_agg
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
grid_10km <- st_read("eea_grid/Grid_ETRS89-LAEA_10K.shp")
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

merge_cubes <- function(new_cube, processed_cube) {
  # merge processed with a new cube e.g. downloaded from GBIF
  # which information needs to be kept for each, should the new cube have information on which cube presented the information
  # should it be possible to merge cubes with different temporal resolution (e.g. year vs month)
  # merge by species, year, grid cell, countryCode

  # need to do a function with required fields in the new cube
  #rows2add <- new_cube %>% select(eeacellcode, specieskey, countrycode, year, count)

  merged_cube <- merge(processed_cube, new_cube, 
                       by = c("eeacellcode", "specieskey", "countrycode", "year"), 
                       all.x = T, all.y = T)

  merged_cube <- merged_cube %>% mutate(count = as.integer(rowSums(across(c(count.x, count.y)))))

  # delete unnecessary count columns
  merged_cube <- merged_cube %>% select(eeacellcode, specieskey, countrycode, year, count)

  print("done")

  return(merged_cube)
}

assess_uncertainty <- function(data, coord_uncertainty_col = "coordinateUncertaintyInMeters", default_na = 1000) {
  if (coord_uncertainty_col %in% names(data)) {
    data <- data %>% mutate(coordinateUncertaintyInMeters = ifelse(is.na(coordinateUncertaintyInMeters), 
                                                                   default_na, coordinateUncertaintyInMeters))
  } else {
    data$coordinateUncertaintyInMeters <- default_na
  }

  return(data)
}


# need to add a way to download metadata on the generation of the cube (see deliverable 2.1 - section 3.4)
