#This script contains a function to find coordinates from tracks from two different cameras
#which are suspected to be duplicates. This situation occurs when a same person is being tracked
#by two cameras simultaneously.

#This function takes as argument two inputs (tA and tB)
#which represents two trajectories (objects of class Track)
duplicate_trackpoints = function(tA, tB)
{
  #Store the two tracks (tA and tB) into two variables t1 and t2
  #such that t1 starts before t2. This ensures that the first observation in t1
  #is always before (or at the same time) as the first observation of t2.
  if(TrackSummary(tA)$tmin <= TrackSummary(tB)$tmin)
  {
    t1 = tA
    t2 = tB
  } else {
    t1 = tB
    t2 = tA
  }
  
  #Initialize empty variables and data structures
  index = 0
  ijd = data.frame()
  
  #Continue only if there is a temporal overlap between t1 and t2
  #Thus, if the last observation in t1 (which always starts before t2)
  #is after t2 starts (first observation in t2), only then further processing is necessary.
  #If not, then it simply means that there is no temporal overlap and duplicate points
  #are impossible. Doing this helps since it saves a lot of time by not indulging in
  #unnecessary computations
  if(TrackSummary(t1)$tmax > TrackSummary(t2)$tmin)
  {
    i2 = 1
    i1 = 1
    
    #Find the index from which overlap starts between
    #the tracks t1 and t2 and store it in i1
    while(t1@sp$time[i1] < t2@sp$time[i2])
    {
      i1 = i1+1
    }
    
    #Creates a 2D 2-column vector (index) with each row containing the corresponding
    #observation indices i and j based on the temporal information
    #in tracks t1 and t2
    i = 0
    j = 0
    while(i1 <= dim(t1@data)[1] && i2 <= dim(t2@data)[1])
    {
      i = c(i, i1)
      j = c(j, i2)
      i1 = i1+1
      i2 = i2+1
    }
    i = i[-c(1)]
    j = j[-c(1)]
    index = cbind(i,j)
    
    #Initialize variables to store spatial and temporal distances to 0
    sdist = 0
    tdist = 0
    
    #For all potentially duplicate observations with indices in "index"...
    for(ij in 1:length(index[,1]))
    {
      #Calculate the spatial distance
      t_sdist = calc_spatial_dist(t1@sp@coords[index[ij,1],], t2@sp@coords[index[ij,2],])
      sdist[ij] = as.numeric(t_sdist)
      
      #Calculate the temporal distance
      t_tdist = calc_temporal_dist(t1@sp@data$time[index[ij,1]], t2@sp@data$time[index[ij,2]])
      tdist[ij] = as.difftime(t_tdist)
    }
    
    #Calculate if any of the spatial and temporal distances cross the thresholds
    #and store it in boolean variables sD (space) and tD (time)
    sD = cross_spatial_treshold(sdist, treshold = 1)
    tD = cross_temporal_treshold(tdist)
    
    #Calculate the temporal distances and store it in "tdist" for future use
    tdist = as.difftime(tdist, units = "secs")
    
    #If the spatial and temporal theshold requirements are satisfied
    #store TRUE in stD, FALSE otherwise
    stD = sD & tD
    
    #Store various variables used in a compact data frame which is to be returned
    #It can be thought of as a table with columns corresponding to index (from both the tracks),
    #distances (spatial and temporal) along with the boolean values to check if they satisfied
    #the threshold requirements. stD is also written which is calculated as sD AND tD
    ijd = data.frame(index, sdist, sD, tdist, tD, stD)    
  }
  return(ijd)
  # return(ijd$stD)
} #else return(data.frame(FALSE))

