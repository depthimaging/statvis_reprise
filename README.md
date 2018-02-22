# statvis_reprise

The aim of `statvis` is to provide an environment for importing, analyzing and visualizing trajectories of visitors in a closed environment (such as a museum) in order to calculate relevant statistical measures of visitors as well as points of interests (such as exhibits in a museum) in the environment under study. The data could be potentially collected using various sensors. For the purpose of this study, Microsoft Kinect was used to collect the data from the site (Horse Museum, Münster Zoo, Münster). From the statistical measures, subjective inferences can be drawn such as the number of visitors in the museum in a certain temporal window, the distribution of the number of visitors over a time (a week, a day etc.) popularity of each point of interest, which points of interest attract visitors the most and so on.

This repository contains the code for the core initial functionalities of statvis including the reading of files from disk, setting up a reference system, combining duplicate tracks etc. A more detailed list of the tasks performed by this repository can be found further below. This repository does not contain code for calculating statistics and visualizing inferred results.

## What is `statvis`?
`statvis` was the result of a the Spatial Statistics and Visualization Group for the study project entitled "Depth Imaging"
at the Institute for Geoinformatics, WWU Münster in the Winter Semester, 2017/18.

### Members of the group:
Aws Dib, Pramit Ghosh, Zhihao Liu, Monica Magán da Fonseca and Zhendong Yuan

### Major tasks & ideas of this group:
The major tasks of this group could be categorized into the following major categories:
* Importing & Cleaning raw data
* Spatial modelling and calculating visitors' trajectories
* Processing the trajectories and calculating appropriate statistics to visualize the visitors' movement and inferring conclusions regarding the exhibits and the visitors

## This repository
This repository (`statvis_reprise`) contains code for a part of the above tasks. A bit more details of the tasks which were addressed in this repository are listed below. This repository contains only the code which was developed by Pramit Ghosh for the purpose of the study project. The code for the other parts can be found in the other repositories in this Github organization (depthimaging).

* Import JSON from cameras into a statistical computing environment
  * Implement recursive directory traversal for reading the JSON files from disk
  * Remove unwanted header from file so that it is a syntactically correct JSON file
* Set up a conceptual reference frame for the area
  * Set up a global reference system into which all coordinates from local coordinate systems of individual cameras have to be mapped
* Processing to take care of visitor trajectories and movements
  * Recognizing individual visitors from spatio-temporal data only (not focussing on joint distance measurements)
* Statistical Analysis
  * Finding regions where visitors stopped
* Visualization
  * Visualization of the floorplan, visitors' trajectories and summaries of the tracks in R
  * Interactive animation for selected visitors' trajectories embedded in HTML

### Usage

*Note:* This repository contains a set of R scripts which are interlinked with each other using `source()`. Variables created in the environment by a script is often used by another script in the future. Thus, since each script individually is not useful all the scripts need to be run together in the correct order using the wrapper script provided (`main.R`).

In order to use these scripts, please follow the steps outlined below:

### Technologies & External Dependencies
This repository is written in R. Thus R needs to be installed on the system. R may be downloaded from the official website: <https://www.r-project.org/>. The scripts need the following R packages as external dependencies:

`jsonlite`, `sp`, `spacetime`, `trajectories`, `Hmisc`, `animation`

If any of the above packages are not installed, please use the following command to install it:

`install.packages("<package name>")`

### Input data
This repository takes JSON file (having a specific structure) containing data regarding visitors' trajectories in a closed environment including coordinates, timestamps and ancilliary data. The specific structure of the JSON file should be the same as those JSONs inside the `Experiments/` directory in this repository.

The JSON files on which the analysis has to be done should be in present in the directory `Experiments`.

### Running the scripts
Once the JSON files are available at the correct path, the wrapper script `main.R` may be run. It may take some time to finish for all the tracks depending on the number of tracks present in total in all the JSON files. The last line in `main.R` uses the function `track_details()` to provide summaries of individual tracks (including an interactive animated web-based interface) after all the analysis is complete. `track_details()` takes a track number as the input. Thus, if details about track #2 is required the call to the function should be:

`track_details(2)`

Alternatively, if the scripts are to be run in a more modular manner the modules (as mentioned in `main.R`) may be run individually but sequentially. Thus, the following set of commands may be used in the R terminal

`source("import.R")`
`source("analyzetracks.R")`
`source("analyzestats.R")`

to achieve the same results. Then, the summaries of a track may be viewed using the `track_details()` function in the manner described above.
