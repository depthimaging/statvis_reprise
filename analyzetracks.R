#Wrapper for scripts for analyzing tracks

source("camera_functions.R")
source("intercamera_static.R")
source("intercamera_dynamic.R")

dup_mat = matrix(nrow = length(globalized_tracks), ncol = length(globalized_tracks))
dup_mat_dyn = matrix(nrow = length(globalized_tracks), ncol = length(globalized_tracks))
#Assuming one person can be tracked simulataneously by a max. of 2 cameras
visitor_ids = data.frame(track = 1:length(globalized_tracks), static_pid = 0, dynamic_pid = 0)
internal_static = data.frame(a = NULL, b = NULL)

for(tno in 1:length(globalized_tracks))
{
  tnoc = 1
  while(tnoc <= length(globalized_tracks))
  {
    if(tnoc == tno) tnoc = tnoc+1
    if(tnoc > length(globalized_tracks)) break
    
    # print(tno)
    # print(tnoc)
    
    intercamera_static_return = duplicate_trackpoints(globalized_tracks[[tno]], globalized_tracks[[tnoc]])
    fname = paste("generated/dup_det", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_static_return, file = fname)
    
    
    if(is.null(intercamera_static_return) || length(intercamera_static_return) <= 0) dup_mat[tnoc,tno] = NA else
    {
      with_duplicates = subset(intercamera_static_return, subset = stD == TRUE)
      num_dup_pts = dim(with_duplicates)[1]
      # print("Duplicates: ")
      # print(num_dup_pts)
      dup_mat[tnoc,tno] = num_dup_pts
      
      visitor_ids[tno, 2] = tno
      # if(num_dup_pts == 0 || is.na(num_dup_pts)) visitor_ids[tno, 2] = tno else 
      if(num_dup_pts > 10) #At least 10 points detected
      {
        visitor_ids[tno, 2] = min(tno,tnoc)
        internal_static = rbind(internal_static, c(tno, min(tno, tnoc)))
      } 
    }
    # dup_mat[tnoc, tno] = intercamera_return$stD
    # internal_static
    
    # intercamera_dynamic_return = pickup_person(globalized_tracks[[tno]], globalized_tracks[[tnoc]])
    intercamera_dynamic_return = pickup_person(globalized_tracks[[tno]], globalized_tracks[[tnoc]], fuzzySpace = 2, fuzzyTime = 5)
    # print(intercamera_dynamic_return$decision)
    
    fname = paste("generated/dynamic", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_dynamic_return, file = fname)
    
    if(is.null(intercamera_dynamic_return$decision) || intercamera_dynamic_return$decision == FALSE)
    visitor_ids[tno, 3] = tno else if(!is.null(intercamera_dynamic_return$decision) && intercamera_dynamic_return$decision == TRUE)
    {
      dup_mat_dyn[tnoc, tno] = min(tno, tnoc)
    }
    
    visitor_ids$dynamic_pid = visitor_ids$track  
    for(tno_s in 1:dim(dup_mat_dyn)[2])
    {
      for(tnoc_s in 1:length(dup_mat_dyn[, tno_s]))
      {
        if(!is.na(dup_mat_dyn[tnoc_s, tno_s]))
        {
          visitor_ids[tno_s, 3] = min(tno_s, tnoc_s)
          visitor_ids[tnoc_s, 3] = min(tno_s, tnoc_s)
        }
      }
    }
    # if(!is.null(intercamera_dynamic_return$decision) && intercamera_dynamic_return$decision == TRUE)
    # {visitor_ids[tno, 3] = min(tno, tnoc)} else {visitor_ids[tno, 3] = tno}
    # visitor_ids
    
    tnoc = tnoc+1
  }
}

# Merging tracks
source("merge3.R")

print("Finished analysing tracks!")