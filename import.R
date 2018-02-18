# This file reads JSONs, extracts metadata and calls appropriate functions
# for cleaning the JSONs as required. Additionally, it implements the Coordinate Reference system
# and converts local camera coordinates to global coordinates by calling appropriate functions. It
# also calls functions to create objects of class Tracks by calling a function.

#Load required libraries
library('jsonlite')

# Inititalize list of lists for the cameras - can be made dynamic
# so that the number of elements (corresponding to each camera) in the outer list
# is generated based on the number of cameras present in the environment
json_data = list(c1 = list(), c2 = list(), c4 = list())

#Recursively read files with extension .json from the directory "Experiments"
files = list.files(path = "Experiments/", pattern = "*.json", recursive = TRUE, full.names = TRUE)

#Go through each files read
for(filename_w_path in files)
{
  #import json values as text from json file
  json_value = readLines(filename_w_path, n = -1, warn = FALSE)
  
  #Read metadata of tracks from JSON char string with position numbers.
  
  #read the camera number and store it in cid
  cid = as.numeric(substr(json_value, 12, 13))
  
  #Other values can also be read potentially
  # height = as.numeric(substr(json_value, 104, 107))
  # global_x = as.numeric(substr(json_value, 125, <end position here>)).
  # global_y = as.numeric(substr(json_value, 147, <end position here>))
  # global_z = as.numeric(substr(json_value, 169, <end position here>))
  # tilt = as.numeric(substr(json_value, 185, <end position here>))
  
  #Rectify JSON syntax
  
  #remove the extra header in the data to make it compatible with the JSON format
  #replace the 1st occurence of square brackets and anything inside the square brackets with an empty string ("") effectively deleting it
  sample_json = sub("*\\[.*?\\] *", "", json_value)
  #read the variable as a valid json
  
  #Creates a data structure from the JSON text
  sample_json = jsonlite::fromJSON(sample_json)
  sample_json = sample_json$bodies_data
  
  #Since there is no camera 3 in this case
  #This is not needed if the camera numbers are sequential
  if(cid == 4) cid = 3
  
  #Goes through each data frame
  for(i in 1:length(sample_json))
  {
    tailpos = length(json_data[[cid]]) + 1
    
    json_data[[cid]][tailpos] = sample_json[i]
    
    #converting "time" to POSIX time
    # op <- options(digits.secs=6)
    json_data[[cid]][[tailpos]]$time = strptime(json_data[[cid]][[tailpos]]$time, format = "%H:%M:%OS")#, format = "%H:%M:%OS")
    
    #Write Camera ID as an attribute
    json_data[[cid]][[tailpos]]$camera = cid
    
    #Find starting & ending times
    # print("Start time: ")
    # start = head(json_data[[cid]][[tailpos]]$time, 1)
    # print(start)
    # print("End time: ")
    # end = tail(json_data[[cid]][[tailpos]]$time, 1)
    # print(end)
    # #Find the duration
    # print("Duration: ")
    # print(end-start)
  }

}
print("Finished reading and parsing JSONs!")

#Creates spatial reference system
#and converts local camera coordinates in visitors' trajectories
#to global coordinates
source("loc2glob.R")
globalized_json = loc2glob(json_data)
print("Finished converting local coordinates from cameras to global coordinates!")

#Creates objects of class Tracks defined in the package "trajectories"
source("trajectory.R")
globalized_tracks = create_trajectories(globalized_json)
print("Finished creating objects of class Tracks to represent visitors' trajectories!")

#Cleans tracks and discards tracks with significant errors
#"outline_Yuan.R" by Yuan
source("outline_Yuan.R")
#globalized_tracks_cleaned = outline(globalized_tracks)
# print("Finished cleaning tracks for various errors!")

