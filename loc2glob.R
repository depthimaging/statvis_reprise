#This file contains code to create a global spatial reference system
#Additionally, this converts the local coordinates to the defined global system

#Create a global coordinate reference system
#which is to be used throughout
source("coordinates.R")

#This function converts local camera coordinates to global coordinates
#This takes as input a data structure (list) generated from the JSON and returns another
#data data structure of the same structure (list). This data structure is a list of data frames.
#The list elements correspond to each camera and the data frames inside these list elements
#correspond to the trajectories
loc2glob = function(local_json)
{
  #Custom functions for each camera based on its orientation.
  #The x-axis and the y-axis is defined according to camera specifications (in this case Kinect)
  #These values are needed to be manipulated based on the orientation
  #(i.e. the direction the camera is pointing at with respect to the global x- and y-axes)
      C = cam[1,]
      for(tracks in 1:length(local_json$c1))
      {
        c1x = C$x + local_json$c1[[tracks]]$y
        c1y = C$y + local_json$c1[[tracks]]$x
        local_json$c1[[tracks]]$x = c1x
        local_json$c1[[tracks]]$y = c1y
      }

      C = cam[3,]
      # The following code for Camera #4 is working now!
      for(tracks in 1:length(local_json$c4))
      {
        c4x = C$x + local_json$c4[[tracks]]$y
        c4y = C$y + local_json$c4[[tracks]]$x
        local_json$c4[[tracks]]$x = c4x
        local_json$c4[[tracks]]$y = c4y
      }
    
      C = cam[2,]
      for(tracks in 1:length(local_json$c2))
      {
        c2x = C$x - local_json$c2[[tracks]]$x
        c2y = C$y + local_json$c2[[tracks]]$y
        local_json$c2[[tracks]]$x = c2x
        local_json$c2[[tracks]]$y = c2y
      }
      
  return(local_json)
}
