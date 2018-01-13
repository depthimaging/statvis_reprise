#This script is the dynamic version of "intercamera_static.R". It finds same persons from different
#cameras which are as separate tracks in globalized_tracks. What is to be done is: look at the last
#few points in the trajectories of each track and find out the probable future points after the track
#is lost. This is to be done by calculating the general direction and speed. Next, look at tracks whose
#starting point is after the time of the last-point. Find out if the first point(s) is close
#(spatio-temporally) to the expected points (with a fuzzy spatial and temporal buffer) using a
#treshold. If yes, then chances are the two tracks are of the same person. This is to be carried out
#pair-wise on all the tracks. Additionally, the concept of open and closed spaces can be incorporated
#(if Liu finishes it in time) and check if the person is lost in a closed space or not. If a person is
#lost in an open space/boundary then chances are he has moved out of the study area.

pickup_person = function(endA, startB, fuzzySpace = 0.1, fuzzyTime = 1)
{
  tA_end = tail(endA@endTime, 1)
  tB_start = head(startB@endTime, 1)
  extra_duration = difftime(tB_start, tA_end)
  time_pts = 1:5
  if(extra_duration > 0 && extra_duration < as.difftime(5, units = "secs"))
  {
    tA_coords = data.frame(tail(endA@sp@coords, 5))
    linreg_x = lm(tA_coords$tracks.x ~ time_pts)
    linreg_y = lm(tA_coords$tracks.y ~ time_pts)
    num_extra_steps = ceiling(as.numeric(extra_duration) / 0.5)
    
    predict_x = predict.lm(object = linreg_x, newdata = data.frame(time_pts = c(5:(5+num_extra_steps))))
    predict_y = predict.lm(object = linreg_y, newdata = data.frame(time_pts = c(5:(5+num_extra_steps))))
    
    pred_x = tail(predict_x, 1)
    pred_y = tail(predict_y, 1)
    
    med_speed = median(tail(endA@connections$speed, 5))
    extra_dist = as.numeric(calc_spatial_dist(c(pred_x, pred_y), tail(tA_coords, 1)))
    should_take = as.difftime(extra_dist/med_speed, units = "secs")
    
    diff_space = as.numeric(calc_spatial_dist(c(pred_x, pred_y), head(startB@sp@coords, 1)))
    diff_time = abs(should_take - extra_duration)
    
    if(diff_space < fuzzySpace && diff_time < as.difftime(fuzzyTime, units = "secs"))
    {
      same_person = data.frame(endcoord = tail(endA@sp@coords, 1), endtime = tA_end, startcoord = head(startB@sp@coords, 1), starttime = tB_start, speed = med_speed, decision = TRUE)
    } else 
    {
      same_person = data.frame(endcoord = tail(endA@sp@coords, 1), endtime = tA_end, startcoord = head(startB@sp@coords, 1), starttime = tB_start, speed = med_speed, decision = FALSE)
    }
    return(same_person)
  }
  return(NULL)
}