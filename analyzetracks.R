#Wrapper for scripts for analyzing tracks

source("camera_functions.R")
source("intercamera_static.R")
source("intercamera_dynamic.R")

dup_mat = matrix(nrow = length(globalized_tracks), ncol = length(globalized_tracks))
#Assuming one person can be tracked simulataneously by a max. of 2 cameras
visitor_ids = data.frame(track = 1:length(globalized_tracks), static_pid = 0, dynamic_pid = 0)

for(tno in 1:length(globalized_tracks))
{
  tnoc = 1
  while(tnoc <= length(globalized_tracks))
  {
    if(tnoc == tno) tnoc = tnoc+1
    if(tnoc > length(globalized_tracks)) break
    
    intercamera_static_return = duplicate_trackpoints(globalized_tracks[[tno]], globalized_tracks[[tnoc]])
    fname = paste("generated/dup_det", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_static_return, file = fname)
    if(is.null(intercamera_static_return)) dup_mat[tnoc,tno] = NA else
    {
      num_dup_pts = length(intercamera_static_return[intercamera_static_return$stD == TRUE])
      dup_mat[tnoc,tno] = num_dup_pts
      if(num_dup_pts == 0) visitor_ids[tno, 2] = tno else if(num_dup_pts > 0)
      {
        #The following line is not tested on real data
        visitor_ids[tno, 2] = min(tno,tnoc)
      } 
    }
    # dup_mat[tnoc, tno] = intercamera_return$stD
    
    intercamera_dynamic_return = pickup_person(globalized_tracks[[tno]], globalized_tracks[[tnoc]])
    fname = paste("generated/dynamic", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_dynamic_return, file = fname)
    #The following line is not tested on real data
    if(!is.null(intercamera_dynamic_return$decision) && as.logical(intercamera_dynamic_return$decision))
      visitor_ids[tno, 3] = min(tno, tnoc) else visitor_ids[tno, 3] = tno
    
    tnoc = tnoc+1
  }
}

print("Finished analysing tracks!")