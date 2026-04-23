#Function to coonvert a list of species occurrences with coordinates to a data cube
floppydisk2cube <- function(data_in, target_grid, specieskey, grid_crs){
  
  # convert data to a vector layer
  occ <- st_as_sf(data_in, coords = c("decimalLongitude", "decimalLatitude"), crs = grid_crs)
  
  # project vector layer to EEA grid, in this example EPGS:3035
  occ_proj <- st_transform(occ, crs(target_grid))
  
  # intersect spatial occurrences with target grid
  occ_eea <- st_intersection(occ_proj, target_grid)
  
  occ_dat <- as.data.frame(occ_eea)
  
  # create data frame with the following fields
  colnames_in <- c("eeacellcode", "specieskey", "species", "countrycode", "year")
  occ_df <- setNames(data.frame(matrix(ncol = length(colnames_in), nrow = nrow(occ_dat))), colnames_in)
  
  # fill-in data frame
  occ_df$eeacellcode <- occ_dat$CellCode
  occ_df$specieskey <- specieskey
  occ_df$countrycode <- occ_dat$countryCode
  occ_df$year <- occ_dat$year
  
  # aggregate occurrences
  occ_agg <- as.data.frame(occ_df %>% group_by(eeacellcode, specieskey, countrycode, year) %>% 
                             summarise(total_count=n(),
                                       .groups = 'drop'))
  #occ_agg    
  colnames(occ_agg)[which(colnames(occ_agg) == 'total_count')] <- 'count'
  
  return(occ_agg)
}

# data_in <- read.csv('b3/Rana_temporaria_withdates.csv', header=T, sep='\t', fill=T)
# # load grid (e.g. EEA grid 10 km)
# target_grid <- st_read('b3/hackathon-projects-2024/projects/10/input/eea_grid/Grid_ETRS89-LAEA_10K.shp')
# # assign GBIF species key for your specie e.g., Cakile maritima
# specieskey <- "2426805" # automate specieskey extraction from GBIF
# # define data layer projection
# grid_crs <- st_crs(4326)
# 
# grep('Latitude', names(data_in))
# grep('Longitude', names(data_in))
# 
# occ_agg <- floppydisk2cube(data_filt, target_grid, specieskey, grid_crs)
# 
# filter_missing_coords <- function(data){
#   
#   # i_lat <- grep('decimalLatitude', names(data))
#   # i_long <- grep('decimalLongitude', names(data))
#   # 
#   # if (i_lat + 1 != i_long){
#   #   stop("Longitude column must be immediately after Latitude")
#   # }
#   
#   data_filt <- data[complete.cases(data[ , c('decimalLatitude','decimalLongitude')]),]
#   
#   return(data_filt)
#   
# }
# 
# data_filt2 <- filter_missing_coords(data_in)
# occ_agg2 <- floppydisk2cube(data_filt2, target_grid, specieskey, grid_crs)
