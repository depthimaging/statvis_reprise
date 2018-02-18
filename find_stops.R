#This script find the regions where a visitor has stopped and plots it

#Load dependencies
library(sp)

#This is a very low-level helper function which calculates the points in a track where
#a visitor has stopped. It acts as a helper function to the function demarcate_stops()
#which calls it
find_diffs_it = function(xyb)
{
  flag = TRUE
  breaklist = c()
  x = 1
  dimx = dim(xyb)[1]
  while(x < dimx)
  {
    z = 1
    while(xyb[x,3] == xyb[x+1,3] && x < dimx)
    {
      x = x+1
      if(x >= dimx)
      {
        flag = FALSE
      }
    }
    if(!flag) break
    breaklist = append(breaklist, x)
    x = x+1
    if(x >= dimx) flag = FALSE
    while(x+1 <= dimx && xyb[x,3] == xyb[x+1,3])
    {
      x = x+1
      if(x >= dimx)
      {
        flag = FALSE
      }
    }
    if(flag)
    {
      x = x+1
      breaklist = append(breaklist, x)
    } else {
      breaklist = append(breaklist, dimx)
    }
  }
  # return(breaklist[1:length(breaklist)-1])
  return(breaklist[1:length(breaklist)])
}

#This function takes an argument egtrack which is a single object of class Track.
#Instead, it can also take an alternative set of arguments, track_no and track_bunch
#where track_bunch is a list of tracks and track_no is the index of the particular track
#which is under consideration. If both track_no and track_bunch contain non-NULL values, the
#argument egtrack is overlooked (egtrack is then calculated inside the function). This function
#marks a point as a stop point, if the total distance traversed by the visitor within a given
#temporal duration (based on a number of successive observations) is less than a spatial threshold.
demarcate_stops = function(egtrack, track_no = NULL, track_bunch = NULL)
{
  if(!is.null(track_no) && !is.null(track_bunch))
    egtrack = track_bunch[[track_no]]
  
  length_egtrack = dim(egtrack)[1]
  bool_stop = vector(length = length_egtrack-1)
  
  #Define a spatial buffer of 0.5m
  dist_window_m = 0.5
  
  i = 2
  while(i < length_egtrack-1)
  {
    #Define a Temporal window corresponding to the duration between three successive observations
    
    #Total distance covered in 3 successive (previous, present and the next) observations
    sum_dist = egtrack@connections$distance[[i-1]] + egtrack@connections$distance[[i]] + egtrack@connections$distance[[i+1]]
    
    #If this distance is less than the spatial buffer (dist_window_m), then the
    #middle observation is declared as a stop
    if(sum_dist <= dist_window_m)
    {
      bool_stop[i] = TRUE
    }
    
    i = i+1
  }
  

  egtrack_coords = coordinates(egtrack)
  
  bpds = as.data.frame(cbind(egtrack_coords, bool_stop))
  bpts = find_diffs_it(bpds)
  
  #Plot the trajectory on the floor layout and include
  #the starting and ending time of the plots close to the respective points
  #as labels
  master_layout()
  title(main = paste("Track # ", track_no, "\n\n"))
  plot(egtrack, type = 'b', ylab = "y direction (in m)", xlab = "x direction (in m)", add = TRUE)
  text(head(coordinates(egtrack), 1), paste("Start:", format(head(time(egtrack), 1), "%T")), pos = 1, cex = 0.75, offset = 1.25)
  text(tail(coordinates(egtrack), 1), paste("End:", format(tail(time(egtrack), 1), "%T")), pos = 1, cex = 0.75, offset = 1.25)
  
  #If bpts is not NULL
  if(!is.null(bpts))
  {
    #The number of points has to be half the length of bpts.
    #This is so because bpts contain the breakpoints of the start and ending indices
    #of stops. Thus, if there are N stops made in a particular track, there will be 2*N
    #entries in bpts where each pair of successive entries correspond to the start and end indices
    #of stopped portions
    no_of_stop = length(bpts)/2
  } else no_of_stop = 0
  
  #If there were stops in the track
  if(no_of_stop > 0)
  {
    bpts_mat = matrix(data = bpts, ncol = 2, byrow = TRUE, dimnames = list(c(), c("start", "end")))
    bpts_df = as.data.frame(bpts_mat)
    duration = vector(length = dim(bpts_df)[1])
    bbox = list()

    affected_bbox = list()
    
    #Go through each portion declared as stops...
    for(stops in 1:dim(bpts_df)[1])
    {
      #Calculate the bounding box (affected_bbox) for each of the stopped portions
      #from the set of points that were declared as stops previously
      duration[stops] = difftime(egtrack@endTime[bpts_df$end[stops]], egtrack@endTime[bpts_df$start[stops]], units = "secs")
      tmp_pts = coordinates(egtrack)
      tmp_sequence = seq(from = bpts_df$start[stops], to = bpts_df$end[stops])
      affected_bbox[[stops]] = bbox(tmp_pts[tmp_sequence, ])
      
      #Draw/plot rectangles corresponding to the bounding box of the subset of coordinate points
      #in the stop region
      rect(xleft = affected_bbox[[stops]][1,1], ybottom = affected_bbox[[stops]][2,1], xright = affected_bbox[[stops]][1,2], ytop = affected_bbox[[stops]][2,2], col = rgb(1,0,0,0.1), border = TRUE, lwd = 2)
    }

    bpts_df = cbind(bpts_df, as.data.frame(duration))
  } else bpts_df = NA
  return(bpts_df)
}

