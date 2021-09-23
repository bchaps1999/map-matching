# Map-matching geospatial data using R

## Background

<img align="right" src="https://github.com/bchaps1999/map-matching/blob/master/images/Sao_Paulo.png" width="500" alt>
<em>A map of central São Paulo, Brazil - blue dots are traffic cameras</em>

When conducting analyses involving vehicles or roads, geospatial data is only of limited use if it is not accurately matched to a map of the area. For example, if we want to know the effects of traffic camera placement on accidents, then it is necessary that we know exactly where each camera is located and on what road. A high level of accuracy is important especially with the treatment variable to limit attenuation bias, but it is also important for the response variable if we want to produce precise coefficients.

This repository contains generalized versions of scripts that match points to a road map based on the coordinates and a description of the location. These scripts were originally written to match traffic cameras to an OpenStreetMap road map, as part of a project that sought to analyze the impact of traffic cameras on accidents. In the context of that project, a data set containing millions of traffic tickets from São Paulo, Brazil was used to identify unique traffic cameras in the city. Although the data included the coordinates of each camera along with a location description, the desired analysis required knowledge of the exact camera location, specifically the road name. To match the cameras to a road map, I wrote a series of R scripts, which are included here in a generalized form:

1. `map_matching.R` — Automatically match points to a road in their proximity that has a similar location description
2. `manual_check.R` — Ease the manual checking of entity locations through several helper functions
3. `duplicate_check.R` — Check for points in the data that may be considered duplicates

## Description

The first script identifies all roads within a certain radius of each entity, and then gives each road a score based on the distance from the camera and the similarity between the road name and the entity location description. The road with the best score is selected as the match for a specific entity, and the closest point on that road is then selected as the new map-matched coordinates for that entity. 

The second script includes a handful of helper functions that support the manual checking process. In the case of the São Paulo project, traffic cameras were easily visible on Google Maps Street View, which allows the location of each camera to be easily verified. Although each camera still needed to be individually checked, the functions accelerated this process by automating tasks like replacing bad coordinates with better ones. However, depending on the type of entity, manual checking may either be unnecessary or impossible for other projects. 

The third script is likely to be the least adaptable to other projects, as it addresses a more specific issue that existed with the traffic cameras data: some cameras should not be considered a unique entity and should instead be combined with other cameras. This could happen with cameras in the same location that target different lanes, as well as with cameras that are actually the same but register as different due to slight changes in coordinates or the location descriptions. I have still included this script here, however, as some users may still find it relevant for their project.

## Usage

## Acknowledgements

I would like to thank Professor Gabriel Kreindler of Harvard University for bringing me onto the team and for all his guidance and support throughout the process. Special thanks also to Professor Peter Christensen of the University of Illinois Urbana-Champaign and Professor Renato Schwambach Vieira of the Catholic University of Brasília.

*This project was supported by the Harvard University Data Science Initiative Faculty Special Projects Fund for the project A Toolkit for Precise Geographic Data on Urban Roads: Application to Measuring the Impact of Automated Cameras on Speeding and Road Crashes*
