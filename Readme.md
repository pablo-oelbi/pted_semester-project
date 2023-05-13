# Proposal for Semester Project

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS23                                     |
|:---------------|:---------------------------------------- |
| **Data:**      | GPS trajectory data from POSMO           |
| **Working Title:**     | Commuting Mode Choice based on distance to the next train station                |
| **Student 1:** | Marijana Maric                           |
| **Student 2:** | Pablo Bischofberger                      |

## Abstract 
<!-- (50-60 words) -->
This study examines the relationship between the distance from home to the train station and the transportation mode used. The analysis of trajectory data seeks to identify patterns and factors influencing the choice of transportation. Statistical methods, such as regression models and clustering techniques, are employed. The study is constrained by the small sample size, inaccurate GPS data, and unaccounted for confounding variables.

## Research Questions
<!-- (50-60 words) -->
The purpose of this research is to investigate the relationship between the distance from home to the train station and the mode of transportation used to get to the station. The study will use trajectory data to examine this relationship and identify potential patterns and factors that influence the choice of transportation mode. To achieve this aim, the following methodological research questions will be addressed:
1. What statistical method should be used to analyse the relationship between the distance between home and station and the mode of transport used to get to the station in the trajectory data?
2. How should the distance between home and station be measured to accurately analyse the relationship between the distance and the mode of transport used?
3. How should the means of transport be categorised to effectively analyse the relationship between the distance and the means of transport used?

## Results / products
<!-- What do you expect, anticipate? -->
The analysis of the trajectory data is expected to provide valuable insights into the relationship between the distance from home to the train station and the mode of transportation used. It is expected that the analysis will reveal patterns and trends in the data, such as temporal or spatial variations in the relationship between distance and mode used. Finally, it is expected that the analysis will highlight potential confounding variables that may influence the relationship between distance and mode used.

## Data
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->
The data will consist of GPS trajectory data collected from individuals traveling to nearest train stations. The dataset will include information on the mode of transportation used (e.g., walking, biking, e-bike, car, public transit), as well as the distance between the individual's home and the train station and in what time this is made.

## Analytical concepts
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->
Statistical methods such as regression models, correlation analysis, and clustering techniques are appropriate for the present work. For the spatial analysis, the calculation of distances in kilometers and meters between residence and stations is used. For this purpose, the spatial clustering and visualization of trajectories is created. Categorization of modes is done by grouping modes into broader categories (e.g. modes with active Co2 emissions, modes with passive Co2 emissions, so that both categories are disjoint) for easier analysis.

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->
For data manipulation and cleaning using dplyr and tidyr and for the geospatial analysis sf and sp packages are being used.
Statistical modeling and hypothesis testing using base R functions and specialized packages (e.g., ggplot2, lme4).
For the visualization of results ggplot2 and other relevant packages are applied.

## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->
Limited sample size or biased sampling could affect the generalizability of results. Furthermore inaccurate GPS data or missing information on transportation modes could impact the analysis. Moreover unaccounted confounding variables (e.g., weather conditions, individual preferences) could influence the relationship between distance and mode used.

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->
