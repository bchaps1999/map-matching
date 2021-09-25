#Stage 2 - manually check point placement on map

#Load libraries
library(mapview)
library(sf)
library(tidyverse)

options(digits = 7)

#Define paths
data_path <- r"(path_goes_here)"
scripts_path <- r"(path_goes_here)"

#Define file names
roads_shapefile_name <- r"(/file_name_goes_here)"

#Set working directory
setwd(scripts_path)

#Read data and convert to sf
#File locations need to match export location from stage 1

#All possible matches for each camera
matches <- read_csv(file = paste(data_path,
                                 r"(/datapartial/matches.csv)",
                                 sep = "")) %>% 
  select(point_id, match_id, everything())

#points with automatically matched coordinates
map_matched_points <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_points.csv)",
  sep = ""
)) %>% 
  #Found on map is automatically 1
  mutate(found_on_map = 1)

#Original monthly data without matched coordinates
point_months <- read_csv(file = paste(
  data_path,
  r"(/datapartial/point_months.csv)",
  sep = ""
))

#Shapefile with only road segments that were possible matches
road_matches <- read_sf(paste(
  data_path,
  r"(/datapartial/road_matches.shp)",
  sep = ""
)) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Shapefile with all road segments
roads <- read_sf(paste(data_path,
                       roads_shapefile_name,
                       sep="")) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Create road segment ID
roads$road_id <- 1:nrow(roads)

#Pulls the final match ID for specific point
#Most functions operate with match IDs, this function makes the conversion
get_match_id <- function(select_point_id, df = map_matched_points) {
  #select_point_id = ID of point being examined
  df %>% 
    filter(point_id %in% select_point_id) %>% 
    select(match_id) %>% 
    pull()
}

#Function to map individual point and selected match
map_match <-  function(select_match_id, df = matches) {
  #select_match_id = match ID for point (produced by get_match_id)
  
  #Need to create two separate sf objects to map both new and old coords
  #sf for new coordinates
  new <- df %>% 
    filter(match_id %in% select_match_id) %>% 
    mutate(new_point = st_as_sfc(paste("POINT (",mm_long," ",mm_lat,")",sep=""))) %>% 
    st_as_sf(crs = 4326) %>%
    st_transform(3857)
  
  #sf for old coordinates
  old <- df %>% 
    filter(match_id %in% select_match_id) %>% 
    mutate(new_point = st_as_sfc(paste("POINT (",long," ",lat,")",sep=""))) %>% 
    st_as_sf(crs = 4326) %>% 
    st_transform(3857)
  
  #Store point id
  map_point_id <- new %>% 
    st_set_geometry(NULL) %>% 
    select(point_id) %>% 
    pull()
  
  #Select only road segments with matching point id
  roads <- road_matches %>% 
    filter(point_id %in% map_point_id)
  
  #Map both new and old coordinates with all possible road segment matches
  mapview(roads) + mapview(new, col.regions = "red") + mapview(old)
  
}

#Pull Google Maps link for easy access
get_match_link <- function(select_match_id, df = matches) {
  
  df %>% 
    filter(match_id == select_match_id) %>% 
    select(link) %>% 
    pull()
  
}

#Function to replace bad match with better one
replace_match <- function(old_id, replacement_id,
                          df_matches = matches, 
                          df_map_matched_points = map_matched_points){
  #old_id = match ID that needs to be replaced
  #replacement_id = match ID to replace it
  
  #Isolate row for better match
  replacement <- df_matches %>% 
    filter(match_id == replacement_id)
  
  #Remove bad match and add replacement
  df_map_matched_points %>% 
    filter(match_id != old_id) %>% 
    rbind(., replacement) %>% 
    assign("map_matched_points", ., envir = .GlobalEnv)
  
}

#Function to replace coordinates - same as process from stage 1
replace_coords <- function(check_id, new_lat, new_long) {
  
  #Create buffer around each point
  new_matches <- map_matched_points %>% 
    #Select coordinates to replace
    filter(point_id == check_id) %>%  
    #Replace coordinates
    mutate(lat = new_lat, long = new_long,
           new_point = st_as_sfc(paste("POINT (",long," ",lat,")",sep=""))) %>% 
    st_as_sf(crs = 4326) %>%
    st_transform(3857) %>% 
    #Create buffer
    st_buffer(., dist = 10) %>% 
    select(point_id, lat, long, location, location_edit, 
           min_date, max_date) %>% 
    #Identify road segments within buffer
    st_join(roads, st_intersects) %>% 
    #Replace buffer coordinates with point coordinates as sf object
    as.data.frame() %>%
    select(point_id, lat, long, location, location_edit, 
           osmname, min_date, max_date, road_id, osmwayid) %>% 
    mutate(geom = st_as_sfc(paste("POINT (",long," ",lat,")",sep="")),
           #Similarity between recorded street name, street names in buffer
           string_sim = NA) %>% 
    st_as_sf(crs = 4326) %>% 
    st_transform(3857)
  
  #Create blank index
  new_matches$match_id <- NA
  
  #Create copy of data that uses road segment sfc instead of point sfc
  new_road_matches <- new_matches %>% 
    as.data.frame() %>% 
    left_join(as.data.frame(roads)) %>% 
    select(-geom) %>% 
    st_as_sf()
  
  #Create empty objects for measuring distance to nearest road segments
  new_matches$dist_to_nearest <- vector(mode = "numeric", 
                                        length = nrow(new_matches))
  mm_points <- list()
  
  #Find closest point on each road segment to point, record distance and new coords
  for (i in 1:nrow(new_matches)) {
    nearest <-
      st_nearest_points(new_matches[i, ], new_road_matches[i, ])
    new_matches$dist_to_nearest[i] <- st_length(nearest)
    mm_points[i] <- st_cast(nearest, "POINT")[2]
  }
  new_matches$mm_point <- mm_points
  
  #Score new_matches based on string similarity and relative distance
  match_scores <- new_matches %>% 
    as.data.frame() %>% 
    select(-geom) %>% 
    mutate(scaled_dist = NA,
           scaled_string_sim = NA,
           score = NA,
           mm_point = st_as_sfc(mm_point))
  
  #Convert to WGS 1984 coords for each match
  match_coordinates <- match_scores %>% 
    st_as_sf(crs = 3857) %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(mm_long = X, mm_lat = Y)
  
  #Add coordinates cols to matches
  match_scores <- match_scores %>% 
    select(-mm_point) %>% 
    bind_cols(match_coordinates) %>% 
    as.data.frame() %>% 
    #Add Google Maps link
    mutate(link = paste("https://www.google.com/maps/place/",
                        mm_lat,",",mm_long,sep=""))
  
  #Data with only lowest score for each point
  new_map_matched_point <- match_scores %>%
    group_by(lat, long, location) %>% 
    arrange(dist_to_nearest) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(found_on_map = 1)
  
  map_matched_points %>% 
    filter(point_id != check_id) %>% 
    rbind(new_map_matched_point) %>% 
    assign("map_matched_points", ., envir = .GlobalEnv)
  
}

#Function to mark found point, 1 if found, 0 if not
#found_on_map is already 1 in data, so only needs to run if not found
point_found <- function(point_found_id,
                         point_status, 
                         df_map_matched_points = map_matched_points) {
  #point_found_id = id of point being verified
  #point_status = whether or not point was found
  
  #Copy coordinates data
  df <- df_map_matched_points
  
  #Update found_on_map with point_status
  df %>%
    mutate(
      found_on_map = ifelse(
        point_id == point_found_id,
        point_status,
        found_on_map
      )
    ) %>% 
    assign("map_matched_points", ., envir = .GlobalEnv)
  
}

#------------------------------------------------------------------------------

#Manual check workflow:

#point 1 (example - camera found nearby)
get_match_link(get_match_id(select_point_id = 1))
replace_coords(check_id = 1, new_lat = 1, new_long = 1)

#point 2 (example - camera not found)
get_match_link(get_match_id(select_point_id = 2))
point_found(point_found_id = 2, point_status = 0)

#TODO: Repeat for all other points in data

#------------------------------------------------------------------------------


#Export map-matched data
#Map matched points with only bare minimum for identification
mm_slim <- map_matched_points %>% 
  select(point_id, mm_lat, mm_long)

#All map-matched points
map_matched_points %>% 
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_points_final.csv",
                         sep = ""))

#Monthly data with map-matched coordinates
point_months %>% 
  left_join(mm_slim, by = "point_id") %>%
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_point_months_final.csv",
                         sep = ""))