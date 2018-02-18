# statvis_reprise

This repository contains the code for the core functionalities of statvis. This repository does not contain code for calculating statistics and visualizing inferred results.

## What is `statvis`?
`statvis` was the result of a the Spatial Statistics and Visualization Group for the study project entitled "Depth Imaging"
at the Institute for Geoinformatics, WWU Münster in WS 2017/18.

### Members of the group:
Aws Dib, Pramit Ghosh, Zhihao Liu, Monica Magán da Fonseca and Zhendong Yuan

### Major tasks & ideas of this group:
The major tasks of this group could be categorized into the following major categories:
* Importing & Cleaning raw data
* Spatial modelling and calculating visitors' trajectories
* Processing the trajectories and calculating appropriate statistics to visualize the visitors' movement and inferring conclusions regarding the exhibits and the visitors

## This repository
This repository contains code for a part of the above tasks. A bit more details of the tasks which were addressed in this repository are listed below. This repository contains the code which was developed by Pramit Ghosh only. The code for the other parts can be found in the other repositories in this organization (depthimaging).

* Import JSON from cameras into a statistical computing environment
  * Remove unwanted header from file so that it is a syntactically correct JSON file
  * Implement recursive directory traversal for the 3 cameras
* Set up a conceptual reference frame for the area
  * Set up a global reference system into which all coordinates from local coordinate systems of individual cameras have to be mapped
* Processing to take care of visitor trajectories and movements
  * Recognizing individual visitors from spatio-temporal data only (not focussing on joint distance measurements)
* Statistical Analysis
  * Finding regions where visitors' stopped
* Visualization
  * Visualization of the floorplan, visitors' trajectories and summaries of the tracks in R
  * Interactive animation for selected visitors' trajectories embedded in HTML
