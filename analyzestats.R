source("find_stops.R")

merged_tracks = s_merged[!is.na(s_merged)]
s_breaks = list()

#This is for printing original globalized_tracks!!
# for(track_id in 1:length(globalized_tracks))
# {
#   break_points[[track_id]] = demarcate_stops(track_id)
# }

for(track_id in 1:length(merged_tracks))
{
  #The following line for plotting globalized_tracks!
  # s_breaks[[track_id]] = demarcate_stops(track_id, globalized_tracks)
  s_breaks[[track_id]] = demarcate_stops(track_id, merged_tracks)
}
print("Finished finding stops!")
