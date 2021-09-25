#Stage 3 - Checking points for duplicates

#NOTE: Assumes "direction" exists for points, can be modified to go without

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
#Map-matched points
points <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_points_final.csv)",
  sep = ""
)) %>% 
  filter(found_on_map == 1) %>% 
  select(-link, -found_on_map, -match_id)
#Map-matched monthly data
point_months <- read_csv(file = paste(
  data_path,
  r"(/datapartial/mm_point_months_final.csv)",
  sep = ""
))

#Read shapefile for road segments
roads <- read_sf(paste(data_path,
                       r"(/dataraw/maps/sp-shapefile/uber-osm-sp.shp)",
                       sep="")) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Add road segment ID for each unique road segment
roads$road_id <- 1:nrow(roads)

#Create dataframe with every possible pair of points
check_duplicates <- expand.grid(1:nrow(points),1:nrow(points)) %>%
  apply(., 1, sort) %>% 
  t() %>% 
  as.data.frame() %>% 
  distinct() %>% 
  rename(id1 = V1, id2 = V2) %>% 
  filter(id1 != id2) %>% 
  #Add other variables for both points in each pair
  left_join(points, by = c("id1" = "point_id")) %>% 
  left_join(points, by = c("id2" = "point_id")) 

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

#Identify all pairs that include each point ID
for (i in 1:nrow(points)) {
  point_id <- points$point_id[i]
  #Find duplicates with ID in first column
  col1 <- duplicates %>% 
    filter(id1 == point_id) %>% 
    select(id2) %>% 
    pull()
  #Find duplicates with ID in second column
  col2 <- duplicates %>% 
    filter(id2 == point_id) %>% 
    select(id1) %>% 
    pull()
  #Combine all unique duplicates in one vector
  new_ids <- data.frame(id2 = unique(c(col1, col2))) %>% 
    #Add ID for camera being checked
    mutate(id1 = point_id) %>% 
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
  rename(new_point_id = 1,
         old_id = 2) %>% 
  #Remove pairs where new ID = old ID
  filter(new_point_id != old_id)

#Remove rows where the new ID is the old ID somewhere else
#Allows all points in a duplicate group to have the same new ID
final_duplicates <- final_duplicates %>% 
  filter(!new_point_id %in% .$old_id)

#Update point IDs for duplicates
points <- points %>% 
  left_join(final_duplicates, by = c("point_id" = "old_id")) %>% 
  mutate(new_point_id = ifelse(is.na(new_point_id), 
                                point_id, new_point_id)) %>% 
  arrange(new_point_id)

#Reorder IDs for continuity by comparing ID to previous ID
for (i in 2:nrow(points)) {
  if (points$new_point_id[i] == points$new_point_id[i-1]) {
    points$point_id[i] <- points$point_id[i-1]
  }
  else {
    points$point_id[i] <- points$point_id[i-1] + 1
  }
}

#Consolidate points with same ID
final_points <- points %>% 
  select(-new_point_id) %>% 
  group_by(point_id) %>% 
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
#Final data for points
final_points %>%
  write_csv(.,
            file = paste(data_path, "/datafinal/points.csv",
                         sep = ""))
#Final monthly data for points
points %>% 
  select(point_id, mm_lat, mm_long, location, road_id) %>% 
  left_join(select(point_months, -point_id)) %>%
  group_by(point_id, month_id) %>% 
  mutate(mm_lat = mean(mm_lat),
         mm_long = mean(mm_long),
         launch_month = min(launch_month),
         last_month = max(last_month),
         num_tickets = sum(num_tickets)) %>%
  slice(1) %>%
  ungroup() %>% 
  write_csv(.,
            file = paste(data_path, "/datafinal/point_months.csv",
                         sep = ""))
#Shapefile for road segments that are matched to a camera
roads %>% 
  as.data.frame() %>% 
  select(road_id, geometry) %>% 
  inner_join(final_points) %>% 
  st_as_sf() %>% 
  st_write(.,
           paste(data_path, "/datafinal/point_matched_roads.shp",
                 sep = ""))