black = c()
white = c()

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

