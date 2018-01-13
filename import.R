#Note: Some comments in this code are OUTDATED!!
library('jsonlite')

#initialize a list of 3 lists
#each sub-lists for each camera
json_data = list(c1 = list(), c2 = list(), c4 = list())

files = list.files(path = "Experiments/", pattern = "*.json", recursive = TRUE, full.names = TRUE)

for(filename_w_path in files)
{
  #import json values as text from json file
  json_value = readLines(filename_w_path, n = -1, warn = FALSE)
  
  # ============ The following lines for each camera ==========================
  #read meta information from JSON char string with position numbers.
  #Can be made dynamic to get the position numbers using grep - but can also result in pitfalls!
  #For now since the JSON structure is consistent, this is enough.
  cid = as.numeric(substr(json_value, 12, 13))
  # height = as.numeric(substr(json_value, 104, 107))
  # global_x = as.numeric(substr(json_value, 125, <end position here>)).
  # global_y = as.numeric(substr(json_value, 147, <end position here>))
  # global_z = as.numeric(substr(json_value, 169, <end position here>))
  # tilt = as.numeric(substr(json_value, 185, <end position here>))
  
  #Combine camera information into a dataframe
  #The other variables mentioned above have to be added too.
  # cinfo = data.frame(cid, height)
  # ==============================================================================
  
  
  
  #remove the extra header in the data to make it compatible with the JSON format
  #replace the 1st occurence of square brackets and anything inside the square brackets with an empty string ("") effectively deleting it
  sample_json = sub("*\\[.*?\\] *", "", json_value)
  #read the variable as a valid json
  sample_json = jsonlite::fromJSON(sample_json)
  sample_json = sample_json$bodies_data
  
  #Since there is no camera 3; a makeshift solution
  if(cid == 4) cid = 3
  
  # columns = c()
  # startpos = 0
  #Note: going through each directory on the disk: c1, c2 and c4 might have to be implemented to import data recursively
  #go through each of the data frames in sample_json and store them in the correct sub-list of json_data according to the camera
  for(i in 1:length(sample_json))
  {
    # columns = c(columns, names(sample_json[i]))
    
    tailpos = length(json_data[[cid]]) + 1
    
    # if(i == 1) startpos = tailpos
    
    #In the next line: "json_data$c1" might have to be made dynamic by changing it to json_data[1], for example
    #This can be done after implementing recursive directory traversal (see "Note" above)
    json_data[[cid]][tailpos] = sample_json[i]
    #access by: json_data$c1[[i]]$x, json_data$c1[[i]]$time etc.
    #converting "time" to POSIX time
    # op <- options(digits.secs=6)
    json_data[[cid]][[tailpos]]$time = strptime(json_data[[cid]][[tailpos]]$time, format = "%H:%M:%OS")#, format = "%H:%M:%OS")
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
  # c_name = paste("c", cid, sep = "")
  # names(json_data[[cid]][startpos:length(json_data[[cid]])]) = c(columns)
  # columns = NA
}
print("Finished reading and parsing JSONs!")

source("loc2glob.R")
globalized_json = loc2glob(json_data)
print("Finished converting local coordinates from cameras to global coordinates!")

source("trajectory.R")
globalized_tracks = create_trajectories(globalized_json)
print("Finished creating objects of class Tracks to represent visitors' trajectories!")

#"outline_Yuan.R" by Yuan
source("outline_Yuan.R")
#globalized_tracks_cleaned = outline(globalized_tracks)
print("Finished cleaning tracks for various errors!")

