#Stage 3 - Checking entities for duplicates

#(Assumes direction exists for entities, can be modified to go without)

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
entities <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_entities_final.csv)",
  sep = ""
)) %>% 
  filter(found_on_streeview == 1) %>% 
  select(-link, -found_on_streetview, -match_id)

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

#Check every possible pair of entities for duplicates
check_duplicates <- expand.grid(1:nrow(entities),1:nrow(entities)) %>%
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct() %>% 
  rename(id1 = V1, id2 = V2) %>% 
  filter(id1 != id2) %>% 
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

check_duplicates$pair_id <- 1:nrow(check_duplicates)

#Create vectors of duplicate IDs
duplicates <- check_duplicates %>% 
  filter(duplicate == 1) %>% 
  select(id1, id2)


#Find overlapping duplicate pairs
ids <- data.frame(id1 = vector(), 
                  id2 = vector())

for (i in 1:nrow(entities)) {
  entity_id <- entities$entity_id[i]
  col1 <- duplicates %>% 
    filter(id1 == entity_id) %>% 
    select(id2) %>% 
    pull()
  col2 <- duplicates %>% 
    filter(id2 == entity_id) %>% 
    select(id1) %>% 
    pull()
  new_ids <- data.frame(id2 = unique(c(col1, col2))) %>% 
    mutate(id1 = entity_id) %>% 
    select(id1, id2)
  ids <- rbind(ids, new_ids)
}

joined_ids <- ids

for (i in 3:10) {
  joined_ids <- joined_ids %>% 
    left_join(ids, by = ("id2" = "id1")) %>% 
    rename(id2 =i)
  
}

joined_ids <- joined_ids %>% 
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct()

all_duplicates <- data.frame(id2 = vector(), id1 = vector())
for (i in 1:nrow(joined_ids)) {
  
  duplicate_ids <- as.numeric(as.vector(joined_ids[i,])) %>% 
    unique()
  id1 <- duplicate_ids[1]
  new_duplicates <- data.frame(id2 = duplicate_ids) %>% 
    mutate(id1 = id1)
  all_duplicates <- rbind(all_duplicates, new_duplicates)
}

final_duplicates <- all_duplicates %>% 
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct() %>% 
  rename(new_entity_id = 1,
         old_id = 2) %>% 
  filter(new_entity_id != old_id)

final_duplicates <- final_duplicates %>% 
  filter(!new_entity_id %in% .$old_id)

#Update entity IDs for duplicates
entities <- entities %>% 
  left_join(final_duplicates, by = c("entity_id" = "old_id")) %>% 
  mutate(new_entity_id = ifelse(is.na(new_entity_id), entity_id, new_entity_id)) %>% 
  arrange(new_entity_id)

#Reorder IDs for continuity
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
  mutate(mm_lat = mean(mm_lat),
         mm_long = mean(mm_long),
         launch_month = min(launch_month),
         last_month = max(last_month),
         num_tickets = sum(num_tickets)) %>% 
  slice(1) %>% 
  ungroup()

# Save data
final_entities %>%
  write_csv(.,
            file = paste(data_path, "/datafinal/entities.csv",
                         sep = ""))

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

roads %>% 
  as.data.frame() %>% 
  select(road_id, geometry) %>% 
  inner_join(final_entities) %>% 
  st_as_sf() %>% 
  st_write(.,
           paste(data_path, "/datafinal/entity_matched_roads.shp",
                 sep = ""))