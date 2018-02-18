#This file contains code to create a data structure (a list),
#convert the raw data in the read JSONs into objects of the class
#and write them as elements of the list

#Loads required package dependencies including "trajectories"
#which contain the definition of the class
library("sp")
library("spacetime")
library("trajectories")

i = 1
#This function creates the objects and puts it in the list
#It takes as input a list of data frames in the structure returned by loc2glob()
create_trajectories = function(global_json)
{
  #Initialize empty list
  cam_trajs = list()
  
  #For each camera in the list of data frames...
  for(cams in global_json)
  {
    # print(i)
    j = 1
    
    #Initialize an empty list for tracks from a particular camera
    each_cam = list()
    
    #For each tracks in that particular camera...
    for(tracks in cams)
    {
      #Create objecs of class Track
      
      #Create a SpatialPointsDataFrame (from sp) object with coordinates (coords = )
      #and data for each of these coordinates (data = ).
      sp_obj = SpatialPointsDataFrame(coords = data.frame(tracks$x, tracks$y), data = subset(tracks, select = -c(1:2)))
      
      #Create a stidf object with an explicit time attribute (time = ). The data (data = ) is retained
      stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
      
      #Finally create an object of class Track (from trajectories) from the STIDF object created above
      track_obj = Track(stidf_obj)
      
      #Append this Track object to the list for that particular camera (each_cam)
      each_cam = c(each_cam, list(track_obj))
      
      j = j+1
    }
    
    #Append the list for a camera (containing all the tracks observed by that camera)
    #to the list cam_trajs which contains trajectories as list elements from all the cameras
    cam_trajs = c(cam_trajs, each_cam)
    i = i+1
  }

  return(cam_trajs)
}
