#This script is the dynamic version of "intercamera_static.R". It finds same persons from different
#cameras which are as separate tracks in globalized_tracks.

#This function looks at the last
#few points in the trajectories of each track (endA and startB) and find out the probable future points
#after the visitor
#is lost in the track first track.
#This is to be done by calculating the past direction and speed.
#Next, it looks at tracks whose
#starting point is after the time of the last-point in the first track.
#It find out if the first point(s) is close
#(spatio-temporally) to the expected points with a spatial and temporal buffer (fuzzySpace and
#fuzzyTime). If yes, then chances are the two tracks are of the same person. If arguments for
#the spatial and temporal buffer are not supplied, a default value of 0.1m and 1 second for space
#and time respectively is used.
pickup_person = function(endA, startB, fuzzySpace = 0.1, fuzzyTime = 1)
{
  #Find the last point in the first track and the first point in the second track
  tA_end = tail(endA@endTime, 1)
  tB_start = head(startB@endTime, 1)
  
  #Calculate the time elapsed between these two events
  extra_duration = difftime(tB_start, tA_end)
  
  #Look at the last five points in the first track
  #to get a general idea of the visitor's direction of movement
  #and speed. This is later used as a trend to "predict" that visitors future position
  time_pts = 1:5
  
  #If the time elapsed (between the two events) is less than 5 seconds,
  #only then calculation is carried out.
  #If the time elapsed is greater than 5 seconds the predictability of the visitor's
  #position is likely to become too uncertain which would result in erronous inferences.
  #Additionally, a check (extra_duration > 0) is kept to make sure there is no temporal
  #overlap between the two tracks
  if(extra_duration > 0 && extra_duration < as.difftime(5, units = "secs"))
  {
    #Create a linear regression model of the x- and y-coordinates from the last few
    #observations in the first track. Also calculate the number of observations to be predicted
    #during the duration of time (extra_duration) between the two events based on the frame rate
    #of the camera (here 0.5 seconds for Kinect)
    tA_coords = data.frame(tail(endA@sp@coords, 5))
    linreg_x = lm(tA_coords$tracks.x ~ time_pts)
    linreg_y = lm(tA_coords$tracks.y ~ time_pts)
    num_extra_steps = ceiling(as.numeric(extra_duration) / 0.5)
    
    #Make a prediction based on the linear regression model for the x and the y coordinates
    #for every 0.5 seconds after the first track ends until the time the 2nd track starts
    predict_x = predict.lm(object = linreg_x, newdata = data.frame(time_pts = c(5:(5+num_extra_steps))))
    predict_y = predict.lm(object = linreg_y, newdata = data.frame(time_pts = c(5:(5+num_extra_steps))))
    
    #The last x- and y-coordinates of the point predicted by the Linear Regression model
    #This is for the time when the second track starts
    pred_x = tail(predict_x, 1)
    pred_y = tail(predict_y, 1)
    
    #Calculate the median speed of the visitor of the 1st track in the last 5 observations
    med_speed = median(tail(endA@connections$speed, 5))
    
    #Calculate the distance that is to be covered between the two events
    #This is the spatial distance between the point where the 1st track ends
    #and the 2nd track starts
    extra_dist = as.numeric(calc_spatial_dist(c(pred_x, pred_y), tail(tA_coords, 1)))
    
    #This is the time the visitor should take to cover that distance (extra_dist)
    #if he/she continues to walk at his/her median speed for the last 5 observations
    #(i.e. the last 2.5 seconds)
    should_take = as.difftime(extra_dist/med_speed, units = "secs")
    
    #Calculate the differences in space and time
    #between the predicted values (position through the predicted values of the linear
    #regression model and the time through the predicted time from the past trends of
    #direction of walking and speed) and the actual values (the starting spatio-temporal position)
    #of the 2nd track
    diff_space = as.numeric(calc_spatial_dist(c(pred_x, pred_y), head(startB@sp@coords, 1)))
    diff_time = abs(should_take - extra_duration)
    
    #If this spatio-temporal difference is less than the defined thresholds
    #for space (fuzzySpace) and time (fuzzyTime) as passed as arguments to this function
    #then store it in a data frame (same_person) containing other observations
    #such as start time, end time, coordinates, speed etc. It also contains a column
    #"decision" which stores the decision representing whether the two tracks are to be 
    #declared from the same person as a boolean variable (based on whether the spatio-temporal)
    #thesholds are respected. This is the data frame returned from this function.
    if(diff_space < fuzzySpace && diff_time < as.difftime(fuzzyTime, units = "secs"))
    {
      same_person = data.frame(endcoord = tail(endA@sp@coords, 1), endtime = tA_end, startcoord = head(startB@sp@coords, 1), starttime = tB_start, speed = med_speed, decision = TRUE)
    } else 
    {
      same_person = data.frame(endcoord = tail(endA@sp@coords, 1), endtime = tA_end, startcoord = head(startB@sp@coords, 1), starttime = tB_start, speed = med_speed, decision = FALSE)
    }
    return(same_person)
  }
  #In case the temporal conditions are not satisfied, further calculations done above
  #are abandoned and NULL is returned (instead of a data frame)
  return(NULL)
}

#Additionally, the concept of open and closed spaces can be incorporated
#(if Liu finishes and delivers in the required format)
#and check if the person is lost in a closed space or not. If a person is
#lost in an open space/boundary then chances are he has moved out of the study area.