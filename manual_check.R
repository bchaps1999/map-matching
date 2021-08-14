#Stage 2- Helper functions for manually checking entity placement on map

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

#Load data
#Full matches data
matches <- read_csv(file = paste(data_path,
                                 r"(/datapartial/matches.csv)",
                                 sep = "")) %>% 
  select(entity_id, match_id, everything())
#Individual entities with mm coordinates
map_matched_entities <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_entities.csv)",
  sep = ""
)) %>% 
  mutate(found_on_streetview = 1)
#Monthly data for individual entities
entity_months <- read_csv(file = paste(
  data_path,
  r"(/datapartial/entity_months.csv)",
  sep = ""
))
#Shapefile for road matches
road_matches <- read_sf(paste(
  data_path,
  r"(/datapartial/road_matches.shp)",
  sep = ""
)) %>% 
  st_as_sf() %>% 
  st_transform(3857)
#Shapefile for all roads
roads <- read_sf(paste(data_path,
                       r"(/dataraw/maps/sp-shapefile/uber-osm-sp.shp)",
                       sep="")) %>% 
  st_as_sf() %>% 
  st_transform(3857)
#Add road ID
roads$road_id <- 1:nrow(roads)

#Get ID for match based on entity ID
#select_entity_id = ID for entity that is being checked
get_match_id <- function(select_entity_id, df = map_matched_entities) {
  
  df %>% 
    filter(entity_id %in% select_entity_id) %>% 
    select(match_id) %>% 
    pull()
  
}

#Function to map individual entity and selected match
#select_match_id = ID for match being checked
map_match <-  function(select_match_id, df = matches) {
  
  #Dataframe with map-matched coordinates of selected match IDs
  new <- df %>% 
    filter(match_id %in% select_match_id) %>% 
    mutate(new_point = st_as_sfc(paste("POINT (",mm_long," ",mm_lat,")",
                                       sep=""))) %>% 
    st_as_sf(crs = 4326) %>%
    st_transform(3857)
  
  #Dataframe with original coordinates of selected match IDs
  old <- df %>% 
    filter(match_id %in% select_match_id) %>% 
    mutate(new_point = st_as_sfc(paste("POINT (",long," ",lat,")",
                                       sep=""))) %>% 
    st_as_sf(crs = 4326) %>% 
    st_transform(3857)
  
  #Extract entity IDs for selected matches
  map_entity_id <- new %>% 
    st_set_geometry(NULL) %>% 
    select(entity_id) %>% 
    pull()
  
  #Select roads with matching entity ids
  roads <- road_matches %>% 
    filter(entity_id %in% map_entity_id)
  
  #Map both new and old entity locations for comparison
  mapview(roads) + mapview(new, col.regions = "red") + mapview(old)
  
}

#Pull Google Maps link from dataframe
#select_match_id = ID for match being checked
get_match_link <- function(select_match_id, df = matches) {
  
  df %>% 
    filter(match_id == select_match_id) %>% 
    select(link) %>% 
    pull()
  
}

#Function to replace inaccurate match with better one based on match_ID
replace_match <- function(old_id, replacement_id,
                          df_matches = matches, 
                          df_map_matched_entities = map_matched_entities){
  
  #Isolate replacement row
  replacement <- df_matches %>% 
    filter(match_id == replacement_id)
  
  #Remove bad match and add replacement
  df_map_matched_entities %>% 
    filter(match_id != old_id) %>% 
    rbind(., replacement)
  
}

#Function to replace inaccurate coordinates with better ones 
replace_coords <- function(check_id, new_lat, new_long) {
  
  #Create buffer around each point
  new_matches <- map_matched_entities %>% 
    #Select entity with inaccurate coordinates based on entity ID
    filter(entity_id == check_id) %>%  
    #Replace coordinates
    mutate(lat = new_lat, long = new_long,
           new_point = st_as_sfc(paste("POINT (",long," ",lat,")",sep=""))) %>% 
    st_as_sf(crs = 4326) %>%
    st_transform(3857) %>% 
    #Create buffer
    st_buffer(., dist = 10) %>% 
    #(Assumes these are the variables in your data)
    select(entity_id, launch_month, lat, long, location, location_edit) %>% 
    #Identify road segments within buffer
    st_join(roads, st_intersects) %>% 
    #Replace buffer coordinates with point coordinates as sf object
    as.data.frame() %>%
    select(entity_id, lat, long, location, location_edit, 
           road_name, min_date, max_date, road_id, osmwayid) %>% 
    #Regenerate sfc with new coordinates
    mutate(geom = st_as_sfc(paste("POINT (",long," ",lat,")",sep="")),
           #Similarity between recorded street name, street names in buffer
           string_sim = NA) %>% 
    st_as_sf(crs = 4326) %>% 
    st_transform(3857)
  
  #This section of code repeats the map-matching from the first script
  #Just for the entity being replaced to ensure proper road name selection
  
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
    mutate(found_on_streetview = 1)
  
  #Remove old row from data and add new row with updated coordinates
  map_matched_entities %>% 
    filter(entity_id != check_id) %>% 
    rbind(new_map_matched_entity)
  
}

#Function to mark found entity
#entity_found_id = entity ID of entity that is being checked
#entity_status = 1 if entity found, 0 if not
entity_found <- function(entity_found_id,
                         entity_status, 
                         df_map_matched_entities = map_matched_entities) {
  
  #Copy coordinates data
  df <- df_map_matched_entities
  #Check if found_on_streetview column has already been added
  #(Assumes checking will be conducted with Google Maps Street View)
  if (!"found_on_streetview" %in% colnames(df)) {
    #Add column if not
    df$found_on_streetview <- 0
    
  }
  
  #Update found_on_streetview with entity_status
  df %>%
    mutate(
      found_on_streetview = ifelse(
        entity_id == entity_found_id,
        entity_status,
        found_on_streetview
      )
    )
  
}

#Export data for condensed entities and monthly observations
mm_slim <- map_matched_entities %>% 
  select(entity_id, mm_lat, mm_long)

map_matched_entities %>% 
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_entities_final.csv",
                         sep = ""))
entity_months %>% 
  left_join(mm_slim, by = "entity_id") %>%
  write_csv(.,
            file = paste(data_path, "/datapartial/mm_entity_months_final.csv",
                         sep = ""))