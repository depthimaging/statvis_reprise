
# tA = globalized_tracks[[2]]
# tB = globalized_tracks[[7]]
# tA@time
# tB@time


duplicate_trackpoints = function(tA, tB)
{
  if(TrackSummary(tA)$tmin <= TrackSummary(tB)$tmin)
  {
    t1 = tA
    t2 = tB
  } else {
    t1 = tB
    t2 = tA
  }
  index = 0
  ijd = data.frame()
  if(TrackSummary(t1)$tmax > TrackSummary(t2)$tmin)
  {
    i2 = 1
    i1 = 1
    while(t1@sp$time[i1] < t2@sp$time[i2])
    {
      i1 = i1+1
    }
    i = 0
    j = 0
    while(i1 <= dim(t1@data)[1] && i2 <= dim(t2@data)[1])
    {
      i = c(i, i1)
      j = c(j, i2)
      i1 = i1+1
      i2 = i2+1
    }
    i = i[-c(1)]
    j = j[-c(1)]
    index = cbind(i,j)
  
    sdist = 0
    tdist = 0
    for(ij in 1:length(index[,1]))
    {
      t_sdist = calc_spatial_dist(t1@sp@coords[index[ij,1],], t2@sp@coords[index[ij,2],])
      sdist[ij] = as.numeric(t_sdist)
      t_tdist = calc_temporal_dist(t1@sp@data$time[index[ij,1]], t2@sp@data$time[index[ij,2]])
      tdist[ij] = as.difftime(t_tdist)
    }
    # sdist = sdist[-c(1)]
    # tdist = tdist[-c(1)]
    
    sD = cross_spatial_treshold(sdist)
    tD = cross_temporal_treshold(tdist)
    tdist = as.difftime(tdist, units = "secs")
    stD = sD & tD
    ijd = data.frame(index, sdist, sD, tdist, tD, stD)    
  }
  return(ijd)
  # return(ijd$stD)
} #else return(data.frame(FALSE))

