# statvis_reprise

Scripts for statistics &amp; visualization group - modified and restructured after abandoning previous repository pramitghosh/statvis. Details about the statvis project below:


## statvis
Depth Imaging - Spatial Statistics &amp; Visualization Group
for the Study Project: Depth Imaging at Institute for Geoinformatics, WWU Münster in WS 2017/18

### Members of the group:
Aws Dib, Pramit Ghosh, Zhihao Liu, Monica Magán da Fonseca and Zhendong Yuan

### Major tasks & ideas of this group:
Listed below are some major responsibilities of this group. Some of the tasks are optional and subject to availibility of time.

* Import JSON from cameras into a statistical computing environment (R) - DONE (Pramit)
  * Remove unwanted header from file so that it is a syntactically correct JSON file - DONE (Pramit)
  * Implement recursive directory traversal for the 3 cameras - DONE (Pramit)
* Clean the data values in the JSONs
  * Eliminate calibration errors - Yuan (phantom, calibration etc.)
* Set up a conceptual reference frame for the area
  * Set up a global reference system into which all coordinates from local coordinate systems of individual cameras have to be mapped - Liu (spatial modelling, closed/open boundary segments)
* Processing to take care of visitor trajectories and movements
  * Recognizing individual visitors from spatio-temporal data only (won't focus on joint distance measurements): one person can be there at only one position at a particular instance of time - Pramit
  * Try and remove occlusions by integrating views of the same persons from other cameras - Monica
* Statistical Analysis - Aws (& others, if necessary)
  * Calculate values such as number of visitors in a day
  * Popular or most-visited exhibits according to the time in front of the exhibits
  * Sequence of visiting the exhibits
  * Facial emotions, posture etc.
* Visualization (in Processing) - Aws
  * Visualizing the generated statistics in the form on numbers and plots
  * Creating animated trajectories of visitors
  * Heatmap of the floorplan
