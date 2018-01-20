
setClass(Class = "merge_tracks", slots = c("tracks", "blacklist", "whitelist", "black_white"))

static_merge = function(static_mat = dup_mat)
{
  pts_overlap = 10
  tmp_dup_mat = matrix(nrow = dim(static_mat)[1], ncol = dim(static_mat)[1])
  
  blacklist = c()
  whitelist = c()
  black_white = data.frame(matrix(ncol = 2))
  
  for(i in 1:dim(static_mat)[1])
  {
    for(j in 1:dim(static_mat)[2])
    {
      if(i < j || is.na(static_mat[i,j]) || static_mat[i, j] < pts_overlap)
      {
        tmp_dup_mat[i,j] = FALSE
      } else
      {
        tmp_dup_mat[i,j] = TRUE
      }
    }
  }
  colnames(tmp_dup_mat) = 1:dim(static_mat)[1]
  
  for(i in 1:dim(tmp_dup_mat)[1])
  {
    if(!is.element(i, blacklist))
    {
      for(j in 1:dim(tmp_dup_mat)[1])
      {
        if(tmp_dup_mat[j, i] == TRUE)
        {
          blacklist = c(blacklist, j)
          whitelist = unique(c(whitelist, i))
          black_white = rbind(black_white, c(j,i))
          tmp_dup_mat[,i] = tmp_dup_mat[,i] | tmp_dup_mat[,j]
        }
      }
    }
  }
  dups = subset(x = as.data.frame(tmp_dup_mat), select = whitelist)
  black_white = black_white[-1,]
  ret_obj = new("merge_tracks", tracks = dups, blacklist = blacklist, whitelist = whitelist, black_white = black_white)

  return(ret_obj)
}

# dynamic_merge

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

helper_dynamic_merge = function(dynamic_mat = dup_mat_dyn)
{
  dyn_dups = unique(na.omit(c(dynamic_mat)))
  dyn_pairs = data.frame(matrix(ncol = 2, nrow = length(dyn_dups)))
  
  for(visitors in 1:length(dyn_dups))
  {
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
res_static = static_merge()
static_bw = res_static@black_white

source("mergers.R")
for(static_pairs in 1:dim(static_bw)[1])
{
  s_merged[[static_bw[static_pairs, 2]]] = merge_static(s_merged[[static_bw[static_pairs, 1]]], s_merged[[static_bw[static_pairs, 2]]])
  s_merged[[static_bw[static_pairs, 1]]] = NA
}

#Dynamic merging starts...

dynamic_mlist = helper_dynamic_merge()
dynamic_mlist_archive = dynamic_mlist
for(dyn_row in 1:dim(dynamic_mlist)[1])
{
  if(is.element(dynamic_mlist[dyn_row, 1], static_bw[,1]))
    dynamic_mlist[dyn_row, 1] = static_bw[match(dynamic_mlist[dyn_row, 1], static_bw[,1]),2]
  if(is.element(dynamic_mlist[dyn_row, 2], static_bw[,1]))
    dynamic_mlist[dyn_row, 2] = static_bw[match(dynamic_mlist[dyn_row, 2], static_bw[,1]),2]
}

colnames(dynamic_mlist) = c("dyn1", "dyn2")
# dynamic_mlist = dynamic_mlist[dyn1 != dyn2]
dynamic_mlist = subset(dynamic_mlist, subset = dyn1 != dyn2)

d_merged = list()
length(d_merged) = length(s_merged)
d_black = c()
# for(sm_tracks in 1:length(s_merged))
# {
#   if(is.element(sm_tracks, unique(unlist(dynamic_mlist))))
#   {
#     for(dyn_row in 1:dim(dynamic_mlist)[1])
#     {
#       dm_track = merge_dynamic(s_merged[[dynamic_mlist[dyn_row,1]]], s_merged[[dynamic_mlist[dyn_row,2]]])
#       if(!is.na(dm_track))
#       {
#         d_merged = list(d_merged, list(dm_track))
#       }
#     }
#   } else
#   {
#     d_merged = list(d_merged, list(s_merged[[sm_tracks]]))
#   }
# }

for(dyn_row in 1:dim(dynamic_mlist)[1])
{
  dm_track = merge_dynamic(s_merged[[dynamic_mlist[dyn_row,1]]], s_merged[[dynamic_mlist[dyn_row,2]]])
  if(!is.na(dm_track))
  {
  #   s_merged[[dynamic_mlist[dyn_row,1]]] = dm_track
  #   s_merged[[dynamic_mlist[dyn_row,2]]] = dm_track
    d_merged_pos = min(dynamic_mlist[dyn_row,1], dynamic_mlist[dyn_row,2])
    d_black = c(d_black, dynamic_mlist[dyn_row,1], dynamic_mlist[dyn_row,2])
    # d_merged[[d_merged_pos]] = list(d_merged, list(dm_track))
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

for(t in 1:length(d_merged))
{
  if(is.null(d_merged[[t]]) && !is.element(t, d_black))
  {
    d_merged[[t]] = s_merged[[t]]
  }
}

