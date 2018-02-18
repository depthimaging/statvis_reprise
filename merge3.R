#This script contains functions, codes and calls to other scripts which helps in
#performing the actual merging

#Define a class "merge_tracks" with 4 slots for the tracks
#Defining this class helps because now objects of this class can be returned
#from a function in a single variable!
setClass(Class = "merge_tracks", slots = c("tracks", "blacklist", "whitelist", "black_white"))

#This function returns an object of the class "merge_tracks" defined above.
#As input it takes the pairwise matrix from the static analysis (dup_mat)
static_merge = function(static_mat = dup_mat)
{
  #The overlap threshold defined earlier
  pts_overlap = 10
  
  #A copy of the pairwise matrix created so that the orignial matrix is not changed
  tmp_dup_mat = matrix(nrow = dim(static_mat)[1], ncol = dim(static_mat)[1])
  
  #Create empty vectors for containing whitelisted and blacklisted tracks
  blacklist = c()
  whitelist = c()
  black_white = data.frame(matrix(ncol = 2))
  
  #Traverse through the cells in dup_mat...
  for(i in 1:dim(static_mat)[1])
  {
    for(j in 1:dim(static_mat)[2])
    {
      #The pairiwise matrix from the static analysis is always symmetric. Thus position (i,j)
      #always contain the same value as position (j,i). This is so because, the values correspond to
      #overlaps between two whole tracks and thus, if Track A has an overlap with Track B this
      #automatically means Track B has an overlap with Track A.
      
      #In the upper triangle of the matrix (i < j),
      #if the cell contain NA values or a value less than the overlap threshold defined previously,
      #update that cell value to FALSE in the copy of the matrix (tmp_dup_mat). Else, change the value
      #to TRUE. This ensures that the duplicated matrix (tmp_dup_mat) always contain a boolean value
      if(i < j || is.na(static_mat[i,j]) || static_mat[i, j] < pts_overlap)
      {
        tmp_dup_mat[i,j] = FALSE
      } else
      {
        tmp_dup_mat[i,j] = TRUE
      }
    }
  }
  
  #Update the column names of the duplicated matrix to correspond to the
  #track numbers which is the same as the index
  colnames(tmp_dup_mat) = 1:dim(static_mat)[1]
  
  #Traverse through one of the axes of the pairwise boolean matrix from the static analysis...
  for(i in 1:dim(tmp_dup_mat)[1])
  {
    #If the index is not a blacklisted one, proceed
    if(!is.element(i, blacklist))
    {
      #Go through all the values for that index in the other axis...
      for(j in 1:dim(tmp_dup_mat)[1])
      {
        #if the cell contain a TRUE value - i.e. if the two tracks as
        #indexed in i and j were detected to be from the same person
        if(tmp_dup_mat[j, i] == TRUE)
        {
          #Add the j index to the blacklist
          blacklist = c(blacklist, j)
          
          #Add the i element to the whitelist if not already present
          whitelist = unique(c(whitelist, i))
          
          black_white = rbind(black_white, c(j,i))
          
          #Update the ith column in the pairwise boolean matrix
          #with the result from column-i OR column-j (i.e. OR operator)
          #This makes sure that the duplicates that might be present in column-j (with some other column)
          #are retained and added (along with those already present in i) to the duplicates for
          #column-i. Using OR ensures that none of the duplicates are "lost". This is necessary because
          #the j-th column has been already added to the blacklist and subsequent iterations
          #will skip that column.
          tmp_dup_mat[,i] = tmp_dup_mat[,i] | tmp_dup_mat[,j]
        }
      }
    }
  }
  
  #Finally, the matrix "tmp_dup_mat" is subsetted to contain only the whitelisted tracks
  dups = subset(x = as.data.frame(tmp_dup_mat), select = whitelist)
  
  black_white = black_white[-1,]
  
  #These vectors are merged together into an object of class "merge_tracks" defined previously
  #so that all of them can be returned as a variable from this function
  ret_obj = new("merge_tracks", tracks = dups, blacklist = blacklist, whitelist = whitelist, black_white = black_white)

  return(ret_obj)
}

# dynamic_merge

#Utility function for finding duplicate tracks for a current track
find_tracks = function(dyn_pid, visitor_matrix = visitor_ids)
{
  to_merge = c()
  while(length(to_merge) < 2) #not really a good idea - there might be multiple tracks linked dynamically (though rare!)
  {
    for(i in 1:dim(visitor_matrix)[1])
    {
      if(dyn_pid == visitor_matrix[i,3])
      {
        to_merge = c(to_merge, i)
      }
    }
  }
  return(to_merge)
}

#Utility function to list track indexes which are to be merged
#Takes the pairwise boolean matrix from dynamic analysis (dup_mat_dyn) as input
helper_dynamic_merge = function(dynamic_mat = dup_mat_dyn)
{
  #Discards duplicates
  dyn_dups = unique(na.omit(c(dynamic_mat)))
  
  #Initializes a data frame for storing duplicate pairs
  dyn_pairs = data.frame(matrix(ncol = 2, nrow = length(dyn_dups)))
  
  #Traverses through the duplicates...
  for(visitors in 1:length(dyn_dups))
  {
    #Finds (find_tracks()) and stores the duplicate track index
    #for each track in the same row of dyn_pairs
    dyn_pairs[visitors,] = find_tracks(dyn_dups[visitors])
  }
  return(dyn_pairs)
}

s_merged = globalized_tracks

#Joints data removed - can be integrated later on if needed without much effort
#Reason: Data in JointData column itself are dataframes
for(otracks in 1:length(s_merged))
{
  s_merged[[otracks]]@sp@data = s_merged[[otracks]]@sp@data[,-18]
}

#Call static_merge() and store results
res_static = static_merge()
static_bw = res_static@black_white

#Load the script "mergers.R" which performs the actual merge
#Contains functions for both the static and dynamic analysis
source("mergers.R")

#Static merging starts...

#Calls merged_static() to perform actual merging of tracks from the static analysis
#Merged resultant tracks are stored in a list s_merged
if(dim(static_bw)[1] > 0)
{
  for(static_pairs in 1:dim(static_bw)[1])
  {
    s_merged[[static_bw[static_pairs, 2]]] = merge_static(s_merged[[static_bw[static_pairs, 1]]], s_merged[[static_bw[static_pairs, 2]]])
    
    #Set one of the merged tracks to NA
    s_merged[[static_bw[static_pairs, 1]]] = NA
  }
}

#Dynamic merging starts...

#This is a bit trickier now because some tracks from globalized_tracks have already been merged
#Some of the tracks might have been referred to in dup_mat_dyn!

#If all the values in the pairwise matrix for dynamic analysis are NOT NA
if(!all(is.na(unlist(dup_mat_dyn))))
{
  #Initialize variables
  d_merged = list()
  length(d_merged) = length(s_merged)
  d_black = c()
  
  #Calls helper_dynamic_merge to create a table of pairs of Track indices
  #which are to be merged. This table is stored in dynamic_mlist
  dynamic_mlist = helper_dynamic_merge()
  
  #A copy of the list is kept for archival since the list will be modified and manipulated later on
  #Keeping a copy for archival now can help in looking at the indices later on for validation
  dynamic_mlist_archive = dynamic_mlist

  #For each of the pairs of tracks to be merged...
  for(dyn_row in 1:dim(dynamic_mlist)[1])
  {  
    #Check with the list for static merge if there are any conflicts
    if(is.element(dynamic_mlist[dyn_row, 1], static_bw[,1]))
      dynamic_mlist[dyn_row, 1] = static_bw[match(dynamic_mlist[dyn_row, 1], static_bw[,1]),2]
    if(is.element(dynamic_mlist[dyn_row, 2], static_bw[,1]))
      dynamic_mlist[dyn_row, 2] = static_bw[match(dynamic_mlist[dyn_row, 2], static_bw[,1]),2]
  }

  #Update column names
  colnames(dynamic_mlist) = c("dyn1", "dyn2")
  
  #Remove rows where both the indices (of the tracks to be merged)
  #refer to the same track. This is possible since some of the values might have changed
  #when resolving conflicts with the tracks which were already merged after the static analysis
  dynamic_mlist = subset(dynamic_mlist, subset = dyn1 != dyn2)

  #For all the pairs of tracks that are to be merged now...
  for(dyn_row in 1:dim(dynamic_mlist)[1])
  {
    #Call merge_dynamic to perform the actual merging
    #and store the combined track in dm_track
    dm_track = merge_dynamic(s_merged[[dynamic_mlist[dyn_row,1]]], s_merged[[dynamic_mlist[dyn_row,2]]])
    
    #If the merging resulted in a valid result
    if(!is.na(dm_track))
    {
      #Perform some blacklisting operations
      #This is necessary because now dynamic_mlist may contain inconsistent values
      #which are to be corrected
      
      #d_merged is incrementally containing the list of track objects after final merging
      d_merged_pos = min(dynamic_mlist[dyn_row,1], dynamic_mlist[dyn_row,2])
      d_black = c(d_black, dynamic_mlist[dyn_row,1], dynamic_mlist[dyn_row,2])
      d_merged[[d_merged_pos]] = dm_track
      
      for(each_row in 1:dim(dynamic_mlist)[1])
      {
        if(dynamic_mlist[each_row, 1] == d_merged_pos)
          dynamic_mlist[each_row, 2] = d_merged_pos
        if(dynamic_mlist[each_row, 2] == d_merged_pos)
          dynamic_mlist[each_row, 1] = d_merged_pos
      }
    }
  }
  
  #For the track slots which are empty after the dynamic merge
  #(i.e. the tracks which were not needed to be merged but were there in the results
  #of static merge in s_merged), populate them with the correct tracks after making sure
  #they were not blacklisted while performing dynamic merging...
  for(t in 1:length(d_merged))
  {
    if(is.null(d_merged[[t]]) && !is.element(t, d_black))
    {
      d_merged[[t]] = s_merged[[t]]
    }
  }
} else
{
  #If the pairwise matrix for dynamic merge contain only NAs
  #(i.e. there was no need for dynamic merging at all), populate d_merged
  #with the same results after static merging (s_merged)
  d_merged = s_merged
}

