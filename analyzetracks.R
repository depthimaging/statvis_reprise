#This script's function is to analyze the tracks for various purposes
#and acts as a wrapper function for a number of important tasks
#such as merging trajectories

#Load other scripts containing important functions
source("camera_functions.R")
source("intercamera_static.R")
source("intercamera_dynamic.R")

#Initializes matrices for storing calculations for the static (dup_mat) and dynamic (dup_mat_dyn) analyses
#Since these are calculated pairwise for all the tracks, the number of rows and columns of these
#matrices are set accordinly (equal to the total number of tracks in globalized_tracks). Implicitly
#also assumes that a single person can be tracked simultaneously by a maximum of 2 cameras (which is
#a reasonable assumption)
dup_mat = matrix(nrow = length(globalized_tracks), ncol = length(globalized_tracks))
dup_mat_dyn = matrix(nrow = length(globalized_tracks), ncol = length(globalized_tracks))

#visitor_ids is a table with three columns - track number, static id and dynamic id
#The static id and dynamic id columns contain IDs of visitors as calculated by the repective
#analyses. Thus, if the static analysis of the tracks find that two tracks have duplicated points
#the IDs are updated in the static_pid column. Initially, all IDs initiated to 0
visitor_ids = data.frame(track = 1:length(globalized_tracks), static_pid = 0, dynamic_pid = 0)

#internal_static is a table with two columns which contain the indexes of tracks to be merged
#as detected from the static analyses. This can be thought of as a task list which is used for
#the actual merging where each row contain the indices of two tracks which are to be merged
internal_static = data.frame(a = NULL, b = NULL)

#For each track in globalized_tracks...
for(tno in 1:length(globalized_tracks))
{
  tnoc = 1
  #Go through each of the other tracks in globalized_json...
  while(tnoc <= length(globalized_tracks))
  {
    #Skip if trying to compare the same two tracks
    #Additional condition to ensure boundary indices
    if(tnoc == tno) tnoc = tnoc+1
    if(tnoc > length(globalized_tracks)) break
    
    #Perform the static analysis in "intercamera_static.R"
    #and store the result in a variable
    intercamera_static_return = duplicate_trackpoints(globalized_tracks[[tno]], globalized_tracks[[tnoc]])
    
    #Save the returned result in a RData format (.Rda) for archival and future use
    #in a directory "generated". The file names are generated automatically with the
    #prefix "dup_det" and the track indexes being compared (tno and tnoc) -
    #all separated by an underscore
    fname = paste("generated/dup_det", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_static_return, file = fname)
    
    #If the returned result is NULL or contain no significant values
    #update the position in dup_mat (corresponding to the combination of the tracks)
    #to NA
    if(is.null(intercamera_static_return) || length(intercamera_static_return) <= 0) dup_mat[tnoc,tno] = NA else
    {
      #If there are duplicates found...
      
      #Filter the track indices for which duplicates were found from another camera
      with_duplicates = subset(intercamera_static_return, subset = stD == TRUE)
      
      #Update the corresponding position in dup_mat with the number of duplicated observations
      #between the two tracks
      num_dup_pts = dim(with_duplicates)[1]
      dup_mat[tnoc,tno] = num_dup_pts
      
      #Update visitor_ids column for static analysis (static_pid) with the tno index
      visitor_ids[tno, 2] = tno
      
      #If there are more than 10 duplicate points between the two tracks
      #Theoretically, even 1 point should be taken into account, but given the
      #uncertainty in the measured positions and various other factors, at least 10 duplicated
      #points are to be detected to confirm that the two tracks belong to the same person
      if(num_dup_pts > 10)
      {
        #Update the corresponding entry in the static_pid column in visitor_ids
        #The min() function is used so that the lowest track number is chosen.
        #This is of course more of a convention and anything else such as max() could
        #also be used potentially
        visitor_ids[tno, 2] = min(tno,tnoc)
        
        #Additionally update internal_static with the track indices
        #Each row contains track indexes in globalized_tracks which are to be merged
        #later on
        internal_static = rbind(internal_static, c(tno, min(tno, tnoc)))
      } 
    }

    #Perform the dynamic analysis and store the result in a variable
    #Buffers of 2m and 5 seconds are used for space and time respectively.
    intercamera_dynamic_return = pickup_person(globalized_tracks[[tno]], globalized_tracks[[tnoc]], fuzzySpace = 2, fuzzyTime = 5)
    
    #Store the results in a directory "generated" with a prefix "dynamic"
    #using a similar consistent file naming convention in RData format
    fname = paste("generated/dynamic", tno, tnoc, sep = "_")
    fname = paste(fname, ".Rda", sep = "")
    saveRDS(object = intercamera_dynamic_return, file = fname)
    
    #If the decision value in the returned result is NULL or FALSE, then update
    #the dynamic_pid column in visitor_ids for that track (tno) with the track index (tno).
    #(Previously, they were all 0)
    if(is.null(intercamera_dynamic_return$decision) || intercamera_dynamic_return$decision == FALSE)
    visitor_ids[tno, 3] = tno else if(!is.null(intercamera_dynamic_return$decision) && intercamera_dynamic_return$decision == TRUE)
    {
      #Otherwise, if the returned decision is TRUE,
      #then update the pairwise matrix for dynamic analysis (dup_mat_dyn)
      #such that the corresponding position for two tracks contain the minimum track index
      dup_mat_dyn[tnoc, tno] = min(tno, tnoc)
    }
    
    #Manipulate the 3rd column for dynamic analysis (dynamic_pid) in the data frame visitor_ids as required
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
    
    tnoc = tnoc+1
  }
}

#Load code to implement the actual merging of the tracks which were detected
#by the detection procedures above
source("merge3.R")

print("Finished analysing tracks!")