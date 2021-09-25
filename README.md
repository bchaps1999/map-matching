# Map-matching geospatial data using R

## Background

<img src="https://github.com/bchaps1999/map-matching/blob/master/images/Sao_Paulo.png" align="right" width="500" alt="A map of central São Paulo, Brazil">

When conducting analyses involving vehicles or roads, geospatial data is only of limited use if it is not accurately matched to a map of the area. For example, if we want to know the effects of traffic camera placement on accidents, then it is necessary that we know exactly where each camera is located and on what road. A high level of accuracy is important especially with the treatment variable to limit attenuation bias, but it is also important for the response variable if we want to produce precise coefficients.

This repository contains generalized versions of scripts that match points to a road map based on the coordinates and a description of the location. These scripts were originally written to match traffic cameras to an OpenStreetMap road map, as part of a project that sought to analyze the impact of traffic cameras on accidents. In the context of that project, a data set containing almost 45 million traffic tickets from São Paulo, Brazil was used to identify unique traffic cameras in the city. Although the data included the coordinates of each camera along with a location description, the desired analysis required knowledge of the exact camera location, specifically the road name. To match the cameras to a road map, I wrote a series of R scripts, which are included here in a generalized form. These generalized scripts refers to "observations" and "points", which would be "tickets" and "cameras" in the context of our project. The scripts accomplish the following tasks:

1. `map_matching.R` — Automatically match points to a road in their proximity that has a similar location description
2. `manual_check.R` — Ease the manual checking of point locations through several helper functions
3. `duplicate_check.R` — Check for points in the data that may be considered duplicates

## Description

#### 1. `map_matching.R`

<img src="https://github.com/bchaps1999/map-matching/blob/master/images/map_matching.png" align="right" width="400" alt="An example of map-matching">

The first script identifies all roads within a certain radius of each point, and then gives each road a score based on the distance from the camera and the similarity between the road name and the point location description. The road with the best score is selected as the match for a specific point, and the closest point on that road is then selected as the new map-matched coordinates for that point. An example of this is shown in the image at right, where the red dots are the original traffic camera locations and the blue dots use the map-matched coordinates.

#### 2. `manual_check.R`

<img src="https://github.com/bchaps1999/map-matching/blob/master/images/map_match_function.png" align="right" width="400" alt="An example of the map-match function">

The second script includes a handful of helper functions that support the manual checking process. In the case of the São Paulo project, traffic cameras were easily visible on Google Maps Street View, which allows the location of each camera to be easily verified. Although each camera still needed to be individually checked, the functions accelerated this process by automating tasks like replacing bad coordinates with better ones. However, depending on the type of point, manual checking may either be unnecessary or impossible for other projects. The image at right shows the output of the `map_match` function for one of the cameras in the data: it shows the original camera location (blue), the map-matched camera location (red), and the possible road segment matches.

#### 3. `duplicate_check.R`

The third script is likely to be the least adaptable to other projects, as it addresses a more specific issue that existed with the traffic cameras data: some cameras should not be considered a unique point and should instead be combined with other cameras. This could happen with cameras in the same location that target different lanes, as well as with cameras that are actually the same but register as different due to slight changes in coordinates or the location descriptions. I have still included this script here, however, as some users may still find it relevant for their project.

## Usage

To walk you through how these scripts should be implemented, let's use as an example the project for which this code was initially created. In that project, we were analyzing the impact of traffic cameras on driver behavior in São Paulo, specifically through changes in the number of accidents. For that project, our treatment variable was the "launch" of a camera at a given location. As I previously mentioned, this information came from a data set from São Paulo with millions of traffic tickets, where each observation was a different ticket. By selecting only for tickets that were recorded by cameras, and then grouping together tickets with the same coordinates and location description, the resulting data featured all the unique locations where tickets were recorded by cameras; in other words, the tickets could be summarized to gain a full list of cameras in São Paulo. We assume that your treatment variable data will also be in the form of individual observations, from which information can be extracted about the location of individual points or entities. If not, the first few chunks of code in `map_matching.R` will need to be adjusted. 

Within the context of the São Paulo example, we will begin by creating a new R project in a new R directory. Download all scripts included in this repository into that directory and create three new folders: "dataraw", "datapartial", and "datafinal". All of your raw data should be located in the "dataraw" folder, and the scripts will automatically put the intermediate and final data files into the "datapartial" and "datafinal" folders, respectively. The aforementioned tickets data would go in the "dataraw" folder, as would a data set for the outcome variable, which would be the accidents data for our project, and a road map of the relevant area. 

To properly implement the scripts, you must have data in the proper format with the relevant variables. Although any data file types can work, the scripts are currently set up for .csv files. The data for your observations should have some kind of coordinates and the date, and a description of the location would also be benficial. In order to define dates relative to a minimum and maximum, you should also have the data for your outcome variable in a similar format. (We assume that your observations will have some kind of date component, but if this is not the case for your data, the scripts will need to be adjusted.) For the road map, we recommend using the shapefiles (.shp) produced by OpenStreetMap (OSM) or Uber Movement. With these two components, we can then match the points to a nearby road segment. 

Here's what the first few rows of our tickets data look like:

| vehicle | location | year | month | day | hour | type | issuer | lat | long | vehicle_type | date |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2 | Av. JacuPÃªs./N. Trab. (S. Mateus/S. Miguel) a menos 20 m da R Jaime Ribeiro Wright | 2015 | 12 | 23 | 17:00 | speeding up to 20% of limit | camera | -23.573583 | -46.44551412 | car | 2015-12-23 |

Starting with `map_matching.R`, users should alter the scripts to best suit their variables and needs. Path, file, and variable names are defined at the top of each script for easy alteration. However, simply plugging in the appropriate names will not guarantee that the script will work properly. Please go through the code chunk by chunk to verify the assumptions we needed to make when generalizing our scripts. 

## Acknowledgements

I would like to thank Professor Gabriel Kreindler of Harvard University for bringing me onto the team and for all his guidance and support throughout the process. Special thanks also to Professor Peter Christensen of the University of Illinois Urbana-Champaign and Professor Renato Schwambach Vieira of the Catholic University of Brasília.

*This project was supported by the Harvard University Data Science Initiative Faculty Special Projects Fund for the project A Toolkit for Precise Geographic Data on Urban Roads: Application to Measuring the Impact of Automated Cameras on Speeding and Road Crashes*
