#Map-matching geospatial data using R: a case study in São Paulo
When conducting analyses involving vehicles or roads, geospatial data is only of limited use if it is not accurately matched to a map of the area. For example, if we want to know the effects of traffic camera placement on accidents, then it is necessary that we know exactly where each camera is located and on what road. A high level of accuracy is important especially with the treatment variable to limit attenuation bias, but it is also important for the response variable if we want to produce precise coefficients.

This past summer, I joined a team of researchers seeking to answer the following question: how does driver behavior change in response to traffic cameras? In this case, the treatment we were looking at was the launch of a traffic camera on a road, and the response was the change in the number of accidents on that road. We already had the data, which included all the traffic tickets issued in the city of São Paulo, Brazil between the years of 2014 and 2018, as well as all vehicle accidents over a similar time period. Although both the tickets and the accident data were geocoded, they needed to be placed on a road map of São Paulo in order to be of any significant value. My task was to match the observations, starting with the cameras, to a road map from OpenStreetMap (OSM) such that we knew exactly where each was located relative to the flow of traffic. 

To do so, I wrote a series of R scripts. This repository contains generalized versions of those scripts that can be adopted to other projects:

1. `map_matching.R` -- Automatically match cameras to a road in their proximity that had a similar location description
2. `manual_check.R` -- Ease the manual checking of camera locations through several helper functions
3. `duplicate_check.R` -- Check for cameras in the data that may be considered duplicates

Note that the generalized versions of the scripts refer to "entities" and "observations", while the project-specific scripts would instead refer to "cameras" and "tickets". This README is an excerpt from a longer Medium article I wrote about the process and scripts. Please reference that article for more detailed descriptions of the code and map-matching process.
