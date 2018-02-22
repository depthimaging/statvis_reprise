#This script contains code and functions for prettier and better visualization of the results

#Loading required dependencies
library(Hmisc)
library(animation)

#This function, once called, plots the floor plan and layout of the region under study
#(the Horse Museum in the Muenster Zoo). It plots the locations and extent of the exhibits,
#the cameras as well as the approximate extent of the room. The coordinate values plotted
#are from the values that were defined previously in "coordinates.R"
master_layout = function()
{
  plot(cam, xlim = c(-1, 4.7), ylim = c(-0.5, 6), xlab = "x direction (in m)", ylab = "y direction (in m)", col = "red", pch = 18, cex = 1.5)
  text(x = cam[1,], "Camera #1", pos = 1)
  text(x = cam[2,], "Camera #2", pos = 4, srt = 90)
  text(x = cam[3,], "Camera #4", pos = 1)
  
  axis(side = 3)
  axis(side = 4)
  grid()
  minor.tick(nx = 10, ny = 10)
  
  rect(xleft = er_s2[1], ybottom = er_s1[2], xright = er_e4[1], ytop = er_e3[2], col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = eb_s[1], ybottom = eb_s[2]-1, xright = eb_e[1], ytop = eb_e[2], col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = p1_e[1], ybottom = p1_s[2], xright = p1_s[1], ytop = p1_e[2]+0.2, col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = p2_e[1], ybottom = p2_s[2], xright = p2_s[1], ytop = p2_e[2]+0.2, col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = p3_e[1], ybottom = p3_s[2], xright = p3_s[1], ytop = p3_e[2]+0.65, col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = pc_e[1], ybottom = pc_e[2], xright = pc_s[1], ytop = pc_e[2]+0.2, col = rgb(0.9,0.9,0.9, alpha = 0.4))
  rect(xleft = tv_e[1], ybottom = tv_e[2], xright = tv_s[1], ytop = tv_e[2]+0.65, col = rgb(0.9,0.9,0.9, alpha = 0.4))
}

#This plots the
#Spatial Reference System and the locations of the cameras and exhibits
master_layout()
title(main = "A Spatial Reference System\nshowing the positions of cameras and exhibits\n\n")

#Interactive animated plotting of visitor trajectory
#This function, once called, creates an interactive animated web-based visualization of the trajectories
#of the visitors. This function takes as argument (required_track) the track number which is to be animated.
#The generated HTML contains buttons to step through, play/pause, increase/decrease the speed,
#loop through the track etc. which the user can use to interact with the visualization
plot_animated_track = function(required_track)
{
  master_layout()
  title(main = paste("Trajectory of the visitor in Track #", required_track, "\n\n"))
  
  sel_track = merged_tracks_d[[required_track]]
  
  ani.record(reset = TRUE)
  oopts = ani.options(interval = 0.5)
  track_coords = coordinates(sel_track)
  for(pts in 1:length(sel_track))
  {
    points(track_coords[pts,1], track_coords[pts,2])
    ani.record()
  }
  # ani.replay()
  saveHTML(ani.replay())
}

#This function plots 4 plots together on the same page for a particular track passed as an
#argument (required_track) to this function. The four plots are arranged in a grid. The plots represent
#the actual trajectory (top left), the distance traversed between each observation (top right),
#the speed of walking of the visitor between each observation (bottom right) and the time elapsed
#between each observation (bottom left) which are calculated from the data
track_summaries = function(required_track)
{
  traj_track = merged_tracks_d[[required_track]]
  par(mfrow = c(2,2))
  master_layout()
  title(main = paste("Trajectory and summary for Track #", required_track, "\n\n"))
  plot(traj_track, type = 'b', main = "Trajectory of the visitor", ylab = "y direction (in m)", xlab = "x direction (in m)", add = TRUE)
  text(head(coordinates(traj_track), 1), paste("Start:", format(head(time(traj_track), 1), "%T")), pos = 1, cex = 0.75, offset = 1.25)
  text(tail(coordinates(traj_track), 1), paste("End:", format(tail(time(traj_track), 1), "%T")), pos = 1, cex = 0.75, offset = 1.25)
  
  plot(traj_track@connections$distance, type = 'l', main = "Distance travelled between each observation", ylab = "Distance travelled (in meters)", xlab = "Observations over time")
  plot(traj_track@connections$duration, type = 'l', main = "Time interval between each observation", ylab = "Time (in seconds)", xlab = "Observations over time")
  plot(traj_track@connections$speed, type = 'l', main = "Speed of visitor between each observation", ylab = "Speed (in meters per second)", xlab = "Observations over time")
  par(mfrow = c(1,1))
}

#This is a wrapper function for plot_animated_track() and track_summaries() defined above
#This function takes a track number as an argument (required_track) which is then passed off
#to the other functions as their argument
track_details = function(required_track)
{
  plot_animated_track(required_track)
  track_summaries(required_track)
}
