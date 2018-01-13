#After running import.R...

#Loading required libraries
library("sp")
library("spacetime")
library("trajectories")

#Creating a trajectory object from the 1st track in Camera 1
subset_c1 = json_data$c1[[1]]
sp_class = SpatialPointsDataFrame(coords = data.frame(subset_c1$x, subset_c1$y), data = data.frame(subset_c1$time, subset_c1$is_happy))
stidf_class = STIDF(sp = sp_class, time = sp_class@data$subset_c1.time, data = data.frame(sp_class@data$subset_c1.is_happy))
track_class = Track(stidf_class)

#Plot the trajectory object
# stbox(track_class)
plot(track_class)
# plot(as(track_class, "STIDF"))

# #3D Space-time plot: Probably not useful in this context
# library("rgl")
# library("rglwidget")
# stcube(track_class)
# rglwidget(elementID = "plot3d")

#Custom smoothening function: need to find the inner working mechanism
smoothen = function(x, y, xout,...) {
  
  predict(smooth.spline(as.numeric(x), y), as.numeric(xout))
}

#Apply the smoothening function on the trajectory and plot it on the previous "rough" track
smoothed_track = approxTrack(track_class, FUN = smoothen, n = 200)
plot(smoothed_track, add = T, col = 'red')