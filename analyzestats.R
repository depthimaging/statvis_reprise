source("find_stops.R")


break_points = list()
for(track_id in 1:length(globalized_tracks))
{
  break_points[[track_id]] = demarcate_stops(track_id)
}
