library("sp")
library("spacetime")
library("trajectories")

i = 1
create_trajectories = function(global_json)
{
  cam_trajs = list()
  for(cams in global_json)
  {
    # print(i)
    j = 1
    each_cam = list()
    for(tracks in cams)
    {
      # print(j)
      sp_obj = SpatialPointsDataFrame(coords = data.frame(tracks$x, tracks$y), data = subset(tracks, select = -c(1:2)))
      stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
      
      track_obj = Track(stidf_obj)
      each_cam = c(each_cam, list(track_obj))
      
      j = j+1
    }
    cam_trajs = c(cam_trajs, each_cam)
    i = i+1
  }

  return(cam_trajs)
}
