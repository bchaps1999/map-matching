#Stage 2 - manually check entity placement on map

#Load libraries
library(mapview)
library(sf)
library(tidyverse)

options(digits = 7)

#Define paths
data_path <- r"(path_goes_here)"
scripts_path <- r"(path_goes_here)"

#Set working directory
setwd(scripts_path)

#Read data and convert to sf
#File locations need to match export location from stage 1

#All possible matches for each camera
matches <- read_csv(file = paste(data_path,
                                 r"(/datapartial/matches.csv)",
                                 sep = "")) %>% 
  select(entity_id, match_id, everything())

#Entities with automatically matched coordinates
map_matched_entities <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_entities.csv)",
  sep = ""
)) %>% 
  #Found on map is automatically 1
  mutate(found_on_map = 1)

#Original monthly data without matched coordinates
entity_months <- read_csv(file = paste(
  data_path,
  r"(/datapartial/entity_months.csv)",
  sep = ""
))

#Shapefile with only roads that were possible matches
road_matches <- read_sf(paste(
  data_path,
  r"(/datapartial/road_matches.shp)",
  sep = ""
)) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Shapefile with all roads
#TODO: Replace file name for road shapefile location
roads <- read_sf(paste(data_path,
                       r"(/file_name)",
                       sep="")) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Create road ID
roads$road_id <- 1:nrow(roads)

#Pulls the final match ID for specific entity
#Most functions operate with match IDs, this function makes the conversion
get_match_id <- function(select_entity_id, df = map_matched_entities) {
  #select_entity_id = ID of entity being examined
  df %>% 
    filter(entity_id %in% select_entity_id) %>% 
    select(match_id) %>% 
    pull()
}

#Function to map individual entity and selected match
map_match <-  function(select_match_id, df = matches) {
  #select_match_id = match ID for entity (produced by get_match_id)
  
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
  
  #Store entity id
  map_entity_id <- new %>% 
    st_set_geometry(NULL) %>% 
    select(entity_id) %>% 
    pull()
  
  #Select only roads with matching entity id
  roads <- road_matches %>% 
    filter(entity_id %in% map_entity_id)
  
  #Map both new and old coordinates with all possible road matches
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
                          df_map_matched_entities = map_matched_entities){
  #old_id = match ID that needs to be replaced
  #replacement_id = match ID to replace it
  
  #Isolate row for better match
  replacement <- df_matches %>% 
    filter(match_id == replacement_id)
  
  #Remove bad match and add replacement
  df_map_matched_entities %>% 
    filter(match_id != old_id) %>% 
    rbind(., replacement) %>% 
    assign("map_matched_entities", ., envir = .GlobalEnv)
  
}

#Function to replace coordinates - same as process from stage 1
replace_coords <- function(check_id, new_lat, new_long) {
  
  #Create buffer around each point
  new_matches <- map_matched_entities %>% 
    #Select coordinates to replace
    filter(entity_id == check_id) %>%  
    #Replace coordinates
    mutate(lat = new_lat, long = new_long,
           new_point = st_as_sfc(paste("POINT (",long," ",lat,")",sep=""))) %>% 
    st_as_sf(crs = 4326) %>%
    st_transform(3857) %>% 
    #Create buffer
    st_buffer(., dist = 10) %>% 
    select(entity_id, lat, long, location, location_edit, 
           min_date, max_date) %>% 
    #Identify road segments within buffer
    st_join(roads, st_intersects) %>% 
    #Replace buffer coordinates with point coordinates as sf object
    as.data.frame() %>%
    select(entity_id, lat, long, location, location_edit, 
           osmname, min_date, max_date, road_id, osmwayid) %>% 
    mutate(geom = st_as_sfc(paste("POINT (",long," ",lat,")",sep="")),
           #Similarity between recorded street name, street names in buffer
           string_sim = NA) %>% 
    st_as_sf(crs = 4326) %>% 
    st_transform(3857)
  
  #Create blank index
  new_matches$match_id <- NA
  
  #Create copy of data that uses road sfc instead of entity sfc
  new_road_matches <- new_matches %>% 
    as.data.frame() %>% 
    left_join(as.data.frame(roads)) %>% 
    select(-geom) %>% 
    st_as_sf()
  
  #Create empty objects for measuring distance to nearest roads
  new_matches$dist_to_nearest <- vector(mode = "numeric", 
                                        length = nrow(new_matches))
  mm_points <- list()
  
  #Find closest point on each road to entity, record distance and new coords
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
  
  #Data with only lowest score for each entity
  new_map_matched_entity <- match_scores %>%
    group_by(lat, long, location) %>% 
    arrange(dist_to_nearest) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(found_on_map = 1)
  
  map_matched_entities %>% 
    filter(entity_id != check_id) %>% 
    rbind(new_map_matched_entity) %>% 
    assign("map_matched_entities", ., envir = .GlobalEnv)
  
}

#Function to mark found entity, 1 if found, 0 if not
#found_on_map is already 1 in data, so only needs to run if not found
entity_found <- function(entity_found_id,
                         entity_status, 
                         df_map_matched_entities = map_matched_entities) {
  #entity_found_id = id of entity being verified
  #entity_status = whether or not entity was found
  
  #Copy coordinates data
  df <- df_map_matched_entities
  
  #Update found_on_map with entity_status
  df %>%
    mutate(
      found_on_map = ifelse(
        entity_id == entity_found_id,
        entity_status,
        found_on_map
      )
    ) %>% 
    assign("map_matched_entities", ., envir = .GlobalEnv)
  
}

#Manual check workflow:

#Entity 1 (example - camera found nearby)
get_match_link(get_match_id(select_entity_id = 1))
replace_coords(check_id = 1, new_lat = 1, new_long = 1)

#Entity 2 (example - camera not found)
get_match_link(get_match_id(select_entity_id = 2))
entity_found(entity_found_id = 2, entity_status = 0)

#TODO: Repeat for all other entities in data

#Export map-matched data
#Map matched entities with only bare minimum for identification
mm_slim <- map_matched_entities %>% 
  select(entity_id, mm_lat, mm_long)

#All map-matched entities
map_matched_entities %>% 
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_entities_final.csv",
                         sep = ""))

#Monthly data with map-matched coordinates
entity_months %>% 
  left_join(mm_slim, by = "entity_id") %>%
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_entity_months_final.csv",
                         sep = ""))