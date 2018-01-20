source("find_stops.R")

#This is for printing original globalized_tracks!!
# for(track_id in 1:length(globalized_tracks))
# {
#   break_points[[track_id]] = demarcate_stops(track_id)
# }
stat_version = function(merged_tracks)
{
  breaks = list()
  for(track_id in 1:length(merged_tracks))
  {
    breaks[[track_id]] = demarcate_stops(track_id, merged_tracks)
    
    # #The following line for plotting globalized_tracks!
    # breaks[[track_id]] = demarcate_stops(track_id, globalized_tracks)
    # breaks[[track_id]] = demarcate_stops(track_id, merged_tracks_s)
    # breaks[[track_id]] = demarcate_stops(track_id, merged_tracks_d)
  }
  return(breaks)
}

merged_tracks_s = s_merged[!is.na(s_merged)]
merged_tracks_d = d_merged[!is.na(d_merged)]

null_filter = lapply(X = merged_tracks_d, FUN = is.null)
# merged_tracks_d = d_merged[!is.na(d_merged)]
merged_tracks_d = merged_tracks_d[null_filter == FALSE]
breaks = stat_version(merged_tracks_d)


print("Finished finding stops!")
