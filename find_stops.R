library(sp)

find_diffs_it = function(xyb)
{
  flag = TRUE
  breaklist = c()
  x = 1
  dimx = dim(xyb)[1]
  while(x < dimx)
  {
    z = 1
    while(xyb[x,3] == xyb[x+1,3] && x < dimx)
    {
      x = x+1
      if(x >= dimx)
      {
        flag = FALSE
      }
    }
    if(!flag) break
    breaklist = append(breaklist, x)
    x = x+1
    if(x >= dimx) flag = FALSE
    while(x+1 <= dimx && xyb[x,3] == xyb[x+1,3])
    {
      x = x+1
      if(x >= dimx)
      {
        flag = FALSE
      }
    }
    if(flag)
    {
      x = x+1
      breaklist = append(breaklist, x)
    } else {
      breaklist = append(breaklist, dimx)
    }
  }
  # return(breaklist[1:length(breaklist)-1])
  return(breaklist[1:length(breaklist)])
}

demarcate_stops = function(track_no, track_bunch)
{
  #In demarcate_stops!
  egtrack = track_bunch[[track_no]]
  length_egtrack = dim(egtrack)[1]
  bool_stop = vector(length = length_egtrack-1)
  dist_window_m = 0.5
  #Temporal window = 1.5 seconds
  i = 2
  while(i < length_egtrack-1)
  {
    sum_dist = egtrack@connections$distance[[i-1]] + egtrack@connections$distance[[i]] + egtrack@connections$distance[[i+1]]
    if(sum_dist <= dist_window_m)
    {
      bool_stop[i] = TRUE
    }
    i = i+1
  }
  
  # egtrack_coords = coordinates(egtrack)[1:dim(egtrack)-1,]
  egtrack_coords = coordinates(egtrack)
  
  bpds = as.data.frame(cbind(egtrack_coords, bool_stop))
  bpts = find_diffs_it(bpds)
  plot(egtrack, type = 'b', main = track_no)
  
  if(!is.null(bpts))
  {
    no_of_stop = length(bpts)/2
  } else no_of_stop = 0
  
  if(no_of_stop > 0)
  {
    bpts_mat = matrix(data = bpts, ncol = 2, byrow = TRUE, dimnames = list(c(), c("start", "end")))
    bpts_df = as.data.frame(bpts_mat)
    duration = vector(length = dim(bpts_df)[1])
    bbox = list()
    
    # plot(egtrack, type = 'b', main = track_no)
    # # plot(egtrack, type = 'b', ylim = c(0,6), xlim = c(-1,5))
    # # points(cam, col = "red", pch = 19)
    
    # plot(egtrack, type = 'b')
    affected_bbox = list()
    for(stops in 1:dim(bpts_df)[1])
    {
      # print(stops)
      duration[stops] = difftime(egtrack@endTime[bpts_df$end[stops]], egtrack@endTime[bpts_df$start[stops]], units = "secs")
      tmp_pts = coordinates(egtrack)
      tmp_sequence = seq(from = bpts_df$start[stops], to = bpts_df$end[stops])
      affected_bbox[[stops]] = bbox(tmp_pts[tmp_sequence, ])
      rect(xleft = affected_bbox[[stops]][1,1], ybottom = affected_bbox[[stops]][2,1], xright = affected_bbox[[stops]][1,2], ytop = affected_bbox[[stops]][2,2], col = rgb(1,0,0,0.1), border = TRUE, lwd = 2)
    }
    # bbox
    bpts_df = cbind(bpts_df, as.data.frame(duration))
  } else bpts_df = NA
  return(bpts_df)
}

