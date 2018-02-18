#This script contains code and functions to calculate some aspects such as finding the
#regions where visitors' have stopped. Thus, it acts as the starting point from where the calculation
#of the statistics can be started. This also acts as a wrapper for some other scripts

#Loading required scripts containing functions and code for calculation and visualization
source("floor_layout.R")
source("find_stops.R")

#This function calls demarcate_stops (in "find_stops.R") on each of the tracks in a set
#(merged_tracks) which is passed as an argument to this function
stat_version = function(merged_tracks)
{
  breaks = list()
  for(track_id in 1:length(merged_tracks))
  {
    #Call demarcate_stops() with the track_id (from the loop) and the set of tracks
    #(passed as merged_tracks to this function)
    breaks[[track_id]] = demarcate_stops(track_no = track_id, track_bunch = merged_tracks)
  }
  return(breaks)
}

#Remove tracks with NAs in merged tracks from the static (s_merged) and
#dynamic analysis (d_merged) and store it in two variables
merged_tracks_s = s_merged[!is.na(s_merged)]
merged_tracks_d = d_merged[!is.na(d_merged)]

#Apply NULL filter to the set of tracks after dynamic merging (merged_tracks_d)
#null_filter contains boolean values - TRUE if the track in that index has a NULL value,
#FALSE otherwise
null_filter = lapply(X = merged_tracks_d, FUN = is.null)

#Filter the set of merged tracks where "null_filter" is FALSE
merged_tracks_d = merged_tracks_d[null_filter == FALSE]

#Call stat_version with the required set of tracks as argument
#for which it will calculate the stops and plot the tracks as well as the stops

#To plot the set of original tracks uncomment the following line
# breaks_g = stat_version(globalized_tracks)

#To plot the set of tracks after static merge uncomment the following line
# breaks_s = stat_version(merged_tracks_s)

#Plots the set of tracks after dynamic merging (the final merge)
breaks = stat_version(merged_tracks_d)


print("Finished finding stops!")
