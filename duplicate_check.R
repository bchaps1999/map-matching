#Stage 3 - Checking entities for duplicates

#NOTE: Assumes "direction" exists for entities, can be modified to go without

#Load libraries
library(sf)
library(stringdist)
library(lubridate)
library(geosphere)
library(ggpubr)
library(scales)
library(tidyverse)

options(digits = 7)

#Define paths
data_path <- r"(path_goes_here)"
scripts_path <- r"(path_goes_here)"

#Set working directory
setwd(scripts_path)

#Load data
#Map-matched entities
entities <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_entities_final.csv)",
  sep = ""
)) %>% 
  filter(found_on_map == 1) %>% 
  select(-link, -found_on_map, -match_id)
#Map-matched monthly data
entity_months <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_entity_months_final.csv)",
  sep = ""
))

#Read shapefile for roads
roads <- read_sf(paste(data_path,
                       r"(/dataraw/maps/sp-shapefile/uber-osm-sp.shp)",
                       sep="")) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Add road ID for each unique road segment
roads$road_id <- 1:nrow(roads)

#Create dataframe with every possible pair of entities
check_duplicates <- expand.grid(1:nrow(entities),1:nrow(entities)) %>%
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct() %>% 
  rename(id1 = V1, id2 = V2) %>% 
  filter(id1 != id2) %>% 
  #Add other variables for both entities in each pair
  left_join(entities, by = c("id1" = "entity_id")) %>% 
  left_join(entities, by = c("id2" = "entity_id")) 

#Compute linear distance between points
check_duplicates$distance <- 
  distGeo(check_duplicates[, c("mm_long.x", "mm_lat.x")], 
          check_duplicates[, c("mm_long.y", "mm_lat.y")])

#Mark duplicates if distance <10m and string similarity >0.9
check_duplicates <- check_duplicates %>%
  filter(!is.na(location.x), !is.na(location.y)) %>% 
  mutate(
    string_sim_to = stringsim(to.x, to.y),
    duplicate = ifelse(osmname.x == osmname.y & distance < 25 &
                         (string_sim_to > 0.5 | is.na(string_sim_to)), 
                       1, 0)
  )

#Create unique ID for each pair
check_duplicates$pair_id <- 1:nrow(check_duplicates)

#Create vectors of duplicate IDs
duplicates <- check_duplicates %>% 
  filter(duplicate == 1) %>% 
  select(id1, id2)

#Find overlapping duplicate pairs (ie 29-30 and 30-31)
ids <- data.frame(id1 = vector(), 
                  id2 = vector())

#Identify all pairs that include each entity ID
for (i in 1:nrow(entities)) {
  entity_id <- entities$entity_id[i]
  #Find duplicates with ID in first column
  col1 <- duplicates %>% 
    filter(id1 == entity_id) %>% 
    select(id2) %>% 
    pull()
  #Find duplicates with ID in second column
  col2 <- duplicates %>% 
    filter(id2 == entity_id) %>% 
    select(id1) %>% 
    pull()
  #Combine all unique duplicates in one vector
  new_ids <- data.frame(id2 = unique(c(col1, col2))) %>% 
    #Add ID for camera being checked
    mutate(id1 = entity_id) %>% 
    select(id1, id2)
  #Add duplicate pairs to dataframe 
  ids <- rbind(ids, new_ids)
}

#Create copy of ids
joined_ids <- ids

#Join ids dataframe to itself multiple times
#This allows for adjacent pairs to be combined into a duplicate group
for (i in 3:10) {
  joined_ids <- joined_ids %>% 
    left_join(ids, by = ("id2" = "id1")) %>% 
    rename(id2 =i)
}

#Identify distinct groups
joined_ids <- joined_ids %>% 
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct()

#Create empty dataframe
all_duplicates <- data.frame(id2 = vector(), id1 = vector())

#Convert duplicate groups from row to column
for (i in 1:nrow(joined_ids)) {
  duplicate_ids <- as.numeric(as.vector(joined_ids[i,])) %>% 
    unique()
  #First ID in group
  id1 <- duplicate_ids[1]
  #Add first ID to each ID in group
  new_duplicates <- data.frame(id2 = duplicate_ids) %>% 
    mutate(id1 = id1)
  #Combine with other groups
  all_duplicates <- rbind(all_duplicates, new_duplicates)
}

#Arrange each row such that first column contains lowest ID
final_duplicates <- all_duplicates %>% 
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  #Identify distinct pairs
  distinct() %>% 
  #Make the first column the new ID
  rename(new_entity_id = 1,
         old_id = 2) %>% 
  #Remove pairs where new ID = old ID
  filter(new_entity_id != old_id)

#Remove rows where the new ID is the old ID somewhere else
#Allows all entities in a duplicate group to have the same new ID
final_duplicates <- final_duplicates %>% 
  filter(!new_entity_id %in% .$old_id)

#Update entity IDs for duplicates
entities <- entities %>% 
  left_join(final_duplicates, by = c("entity_id" = "old_id")) %>% 
  mutate(new_entity_id = ifelse(is.na(new_entity_id), 
                                entity_id, new_entity_id)) %>% 
  arrange(new_entity_id)

#Reorder IDs for continuity by comparing ID to previous ID
for (i in 2:nrow(entities)) {
  if (entities$new_entity_id[i] == entities$new_entity_id[i-1]) {
    entities$entity_id[i] <- entities$entity_id[i-1]
  }
  else {
    entities$entity_id[i] <- entities$entity_id[i-1] + 1
  }
}

#Consolidate entities with same ID
final_entities <- entities %>% 
  select(-new_entity_id) %>% 
  group_by(entity_id) %>% 
  mutate(
    #Take average for coordinates
    mm_lat = mean(mm_lat),
    mm_long = mean(mm_long),
    #Find min and max for launch and last month
    launch_month = min(launch_month),
    last_month = max(last_month),
    #Find total number of tickets
    num_tickets = sum(num_tickets)
  ) %>% 
  slice(1) %>% 
  ungroup()

#Save data
#Final data for entities
final_entities %>%
  write_csv(.,
            file = paste(data_path, "/datafinal/entities.csv",
                         sep = ""))
#Final monthly data for entities
entities %>% 
  select(entity_id, mm_lat, mm_long, location, road_id) %>% 
  left_join(select(entity_months, -entity_id)) %>%
  group_by(entity_id, month_id) %>% 
  mutate(mm_lat = mean(mm_lat),
         mm_long = mean(mm_long),
         launch_month = min(launch_month),
         last_month = max(last_month),
         num_tickets = sum(num_tickets)) %>%
  slice(1) %>%
  ungroup() %>% 
  write_csv(.,
            file = paste(data_path, "/datafinal/entity_months.csv",
                         sep = ""))
#Shapefile for roads that are matched to a camera
roads %>% 
  as.data.frame() %>% 
  select(road_id, geometry) %>% 
  inner_join(final_entities) %>% 
  st_as_sf() %>% 
  st_write(.,
           paste(data_path, "/datafinal/entity_matched_roads.shp",
                 sep = ""))