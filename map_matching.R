#Stage 1 - automated matching of points to street map

#Load libraries - install if necessary
library(janitor)
library(sf)
library(stringdist)
library(lubridate)
library(geosphere)
library(ggpubr)
library(tidyverse)

#Set number of digits
options(digits = 7)

#TODO: Create "dataraw", "datapartial", and "datafinal" folders in directory

#Define paths for data and script locations
data_path <- r"(path_goes_here)"
scripts_path <- r"(path_goes_here)"

#Set file names
observations_file_name <- r"(/file_name_goes_here)"
other_observations_file_name <- r"(/file_name_goes_here)"
roads_shapefile_name <- r"(/file_name_goes_here)"

#Primary data set should include the following vars:
# "lat" = latitude of observation, CRS = 4326
# "long" = longitude of observation, CRS = 4326
# "location" = description of observation location, such as street address
# "year" = year of observation
# "month" = month of observation
# "day" = day of observation

#Other data sets should include similar variables
#Rename variables and adjust script for missing variables as necessary

#Set working directory
setwd(scripts_path)

#Load data for primary variable of interest
observations <- read_csv(file = paste(data_path,
                                observations_file_name,
                                sep="")) %>% 
  #Convert coordinates and date variables
  mutate(lat = as.numeric(as.character(lat)),
         long = as.numeric(as.character(long)),
         date = ymd(paste(year, month, day, sep="-")),
         location = as.character(location)) #Description of location or address

#Load other data, ie dependent variable
other_observations <- read_csv(file = paste(data_path,
                                      other_observations_file_name,
                                      sep="")) %>% 
  #Convert coordinates and date variables
  mutate(lat = as.numeric(as.character(lat)), #Latitude (CRS = 4326)
         long = as.numeric(as.character(long)), #Longitude (CRS = 4326)
         date = ymd(paste(year, month, day, sep="-")),
         location = as.character(location)) #Description of location or address

#Find year for first observation
min_year <- min(year(min(other_observations$date)), 
                year(min(observations$date)))
#Find minimum of last years of both data sets
max_year <- min(year(max(other_observations$date)), 
                year(max(observations$date)))

#Data with observations per point per month
point_months <- observations %>% 
  #Remove missing observations
  filter(!is.na(lat),!is.na(long), !is.na(date)) %>%
  #Create unique ID for each month based on min year
  mutate(month = month(date),
         year = year(date),
         month_id = (year - min_year)*12 + month) %>% 
  #Identify unique points per month
  group_by(lat, long, location, month_id) %>%
  summarize(num_observations = n(),
            .groups = "keep") %>% 
  ungroup()

#Total observations per point, launch of point
points <- point_months %>% 
  group_by(lat, long, location) %>%
  summarize(num_observations = sum(num_observations),
            #This assumes first and last recorded observation are important
            #May not be true for your analysis
            launch_month = min(month_id),
            last_month = max(month_id),
            .groups = "keep") %>% 
  ungroup()

#Free memory
rm(observations)
rm(other_observations)

#Create new variables
points <- points %>%
  #Simplify location description
  mutate(
    #Convert to lowercase and remove periods
    location_edit = tolower(str_remove_all(location, "\\.")),
    
    #TODO: Replace abbreviations with full word
    #TODO: If possible, extract direction from location
    #TODO: If possible, extract "to" and "from" parts of direction
    
    #Remove all non-alphanumeric characters - this line should be last
    location_edit = str_replace_all(location_edit, "[^[:alnum:]]", "")
  ) 

#Create index for points
num_points <- nrow(points)
points$point_id <- 1:num_points

#Add new variables to monthly data
point_months <- point_months %>% 
  left_join(select(points, -num_observations)) %>% 
  select(-location_edit) 

#Convert points to sf object using coordinates
points <- points %>% 
  mutate(geom = st_as_sfc(paste("POINT (",long," ",lat,")",sep=""),
                          crs = 4326)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 3857)

#Read shapefile for road segments
roads <- read_sf(paste(data_path,
                       roads_shapefile_name,
                       sep="")) %>% 
  #TODO: If necessary, change road segment name variable to "road_name"
  #Create edited version of road segment name variable
  mutate(road_name_edit = tolower(road_name), 
         road_name_edit = str_remove_all(road_name_edit, " ")) %>% 
  st_as_sf() %>% 
  st_transform(3857)

#Add road segment ID for each unique road segment
roads$road_id <- 1:nrow(roads)

#Create buffer around each point
matches <- st_buffer(points, dist = 25) %>% 
  #Identify road segments within buffer
  st_join(roads, st_intersects) %>% 
  select(point_id, launch_month, last_month, lat, long, location, 
         location_edit, road_name, road_name_edit, road_id, num_observations
         # Add if they exist: direction, from, to
         ) %>% 
  #Replace buffer coordinates with point coordinates as sf object
  as.data.frame() %>% 
  select(-geom) %>% 
  mutate(geom = st_as_sfc(paste("POINT (",long," ",lat,")",sep="")),
         #Similarity between recorded street name, street names in buffer
         string_sim = stringsim(.$location_edit,
                                .$road_name_edit,
                                method = "lv")) %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(3857)

#Create index for each possible match for later examination
matches$match_id <- 1:nrow(matches)

#Create copy of data that uses road segment sfc instead of point sfc
road_matches <- matches %>% 
  as.data.frame() %>% 
  left_join(as.data.frame(roads)) %>% 
  select(-geom) %>% 
  st_as_sf()

#Create empty objects for measuring distance to nearest road segments
matches$dist_to_nearest <- vector(mode = "numeric", 
                                  length = nrow(matches))
mm_points <- list()

#Find closest point on each road segment to point, record distance and new coords
for (i in 1:nrow(matches)) {
  nearest <-
    st_nearest_points(matches[i, ], road_matches[i, ])
  matches$dist_to_nearest[i] <- st_length(nearest)
  mm_points[i] <- st_cast(nearest, "POINT")[2] %>% st_transform(crs = 4326)
}
matches$mm_point <- as.character(mm_points)

#Score matches based on string similarity and relative distance
match_scores <- matches %>% 
  as.data.frame() %>% 
  select(-geom) %>% 
  mutate(scaled_dist = scale(dist_to_nearest/max(dist_to_nearest)),
         scaled_string_sim = scale(1 - string_sim),
         score = (scaled_dist + scaled_string_sim)/2,
         #Extract long and lat from coordinates
         #Should be a way to do it with sf, but could not make it work
         mm_long = str_extract(mm_point, "\\-.*"),
         mm_lat = str_extract(mm_point, "\\ .*"),
         mm_long = str_remove(mm_long, "\\,.*"),
         mm_lat = str_remove(mm_lat, "\\)"),
         mm_long = as.numeric(mm_long),
         mm_lat = as.numeric(mm_lat),
         #Add Google Maps link
         link = paste("https://www.google.com/maps/place/",
                      mm_lat,",",mm_long,sep="")) %>% 
  select(-mm_point)

#Data with only lowest score for each point
map_matched_points <- match_scores %>%
  group_by(point_id) %>% 
  arrange(score) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(point_id, launch_month, last_month, mm_lat, mm_long, lat, long, 
         location, road_name, road_id, link, match_id, num_observations
         # Add if they exist: direction, from, to
         )

#Save data
#Possible matches
write_csv(match_scores,
          file = paste(data_path, "/datapartial/matches.csv",
                       sep = ""))
#Map matched points
write_csv(map_matched_cameras,
          file = paste(data_path, "/datapartial/mm_cameras.csv",
                       sep = ""))
#Monthly data for points
point_months %>% 
  write_csv(., file = paste(data_path, "/datapartial/camera_months.csv",
                            sep = ""))
#Shapefile for possible road segment matches
road_matches %>%
  st_write(., paste(data_path, "/datapartial/road_matches.shp",
                    sep = ""), append = FALSE)