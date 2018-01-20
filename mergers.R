
merge_static = function(A, B)
{
  if(head(time(A), 1) > head(time(B), 1))
  {
    C = A
    A = B
    B = C
  }
  camA = head(unique(A@sp$camera),1)
  camB = head(unique(B@sp$camera),1)
  overlap = duplicate_trackpoints(A, B)
  overlap = subset(overlap, stD == TRUE)
  #Cases where one track is a temporal subset of another track might not work - testing needed!
  
  ioA = overlap$i
  ioB = overlap$j
  cA = coordinates(A)
  cB = coordinates(B)
  tA = time(A)
  tB = time(B)
  ctA = data.frame(cA, tA)
  ctB = data.frame(cB, tB)
  dA = A@sp
  dB = B@sp
  
 
  pre = ctA[1:head(ioA, 1)-1,]
  if(dim(na.omit(pre))[1] <= 0)
  {
    pre = ctB[1:head(ioB, 1)-1,]
    # pre_d = dB[1:head(ioB, 1)-1,]
    pre_d = dB@data[1:head(ioB, 1)-1,]
  } else  pre_d = dA@data[1:head(ioA, 1)-1,]
  
  
  post = ctB[tail(ioB, 1)+1:length(B),]
  if(dim(na.omit(post))[1] <= 0)
  {
    post = ctA[tail(ioA, 1)+1:length(A),]
    if(tail(ioA, 1)+1 < length(A))
      post_d = dA@data[tail(ioA, 1)+1:length(A),]
    else
      post_d = dA@data[length(A):length(A),]
  } else  post_d = dB@data[tail(ioB, 1)+1:length(B),]
  
  ctmidAB = data.frame(ctA[head(ioA, 1):tail(ioA, 1),], ctB[head(ioB, 1):tail(ioB, 1),])
  dmidA = dA[head(ioA, 1):tail(ioA, 1),]
  dmidB = dB[head(ioB, 1):tail(ioB, 1),]
  ctmid_x = vector(mode = "double", length = dim(ctmidAB)[1])
  ctmid_y = vector(mode = "double", length = dim(ctmidAB)[1])
  # ctmid_t = vector(mode = "list", length = dim(ctmidAB)[1])
  ctmid_t = ctmidAB[,3]
  
  ctmid = data.frame(cbind(ctmid_x, ctmid_y, ctmid_t))
  ctmid[,3] = ctmid_t
  
  dmidA = dmidA@data
  dmidB = dmidB@data
  dmid = data.frame(matrix(nrow = dim(dmidA)[1], ncol = dim(dmidB)[2]))
  colnames(dmid) = colnames(dmidA)
  dmid$time = dmidA$time
  
  avg_cx = (ctmidAB[,1] + ctmidAB[,4])/2
  avg_cy = (ctmidAB[,2] + ctmidAB[,5])/2
  
  ABi = 1
  for(location in 1:dim(ctmidAB)[1])
  {
    distA = calc_spatial_dist(cam[camA,], c(avg_cx[location], avg_cy[location]))
    distB = calc_spatial_dist(cam[camB,], c(avg_cx[location], avg_cy[location]))
    # print(distA - distB)
    pt_data = data.frame()
    
    if(distA > distB)
    {
      ctmid[location,] = ctmidAB[ABi,4:6]
      pt_data = dmidB[ABi,]
      pt_dataA = dmidA[ABi,]
      for(d in 4:6) if(pt_data[d] == "Unknown" && pt_dataA[d] != "Unknown") pt_data[d] = pt_dataA[d]
      
    } else
    {
      ctmid[location,] = ctmidAB[ABi, 1:3]
      pt_data = dmidA[ABi,]
      pt_dataB = dmidB[ABi,]
      for(d in 4:6) if(pt_data[d] == "Unknown" && pt_dataB[d] != "Unknown") pt_data[d] = pt_dataB[d]
    }
    dmid[location,] = pt_data
    ABi = ABi + 1
  }
  
  # print(dmid)
  
  
  
  pre = na.omit(pre)
  ctmid = na.omit(ctmid)
  post = na.omit(post)
  
  # pre = pre[!all(is.na(pre)),]
  # ctmid = ctmid[!all(is.na(ctmid)),]
  # post = post[!all(is.na(post)),]
  
  pre_d = na.omit(pre_d)
  dmid = na.omit(dmid)
  post_d = na.omit(post_d)
  
  # pre_d = pre_d[!all(is.na(pre_d)),]
  # dmid = dmid[!all(is.na(dmid)),]
  # post_d = post_d[!all(is.na(post_d)),]
  
  colnames(pre) = colnames(ctmid)
  colnames(post) = colnames(ctmid)
  
  rownames(pre) = 1:dim(pre)[1]
  rownames(ctmid) = (dim(pre)[1]+1):(dim(pre)[1]+dim(ctmid)[1])
  rownames(post) = (dim(pre)[1]+dim(ctmid)[1]+1):(dim(pre)[1]+dim(ctmid)[1]+dim(post)[1])
  rownames(pre_d) = rownames(pre)
  rownames(dmid) = rownames(ctmid)
  rownames(post_d) = rownames(post)
  
  ct = rbind(pre, ctmid, post)
  # ct = na.omit(ct)
  
  ad = rbind(pre_d, dmid, post_d)
  
  sp_obj = SpatialPointsDataFrame(coords = data.frame(ct[,1], ct[,2]), data = ad)
  stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
  st_merged = Track(stidf_obj)
  return(st_merged)
}


merge_dynamic = function(A, B)
{
  flag = TRUE
  dyn_merged = NA
  
  #The following line assumes that there is no temporal overlap between A and B
  if(head(time(A),1) > tail(time(B),1))
  {
    C = B
    B = A
    A = C
  } else if (head(time(B),1) > tail(time(A),1))
  {
    B = B
    A = A
  } else flag = FALSE
  
  if(flag)
  {
    cA = coordinates(A)
    cB = coordinates(B)
    tA = time(A)
    tB = time(B)
    ctA = data.frame(cA, tA)
    ctB = data.frame(cB, tB)
    dA = A@sp@data
    dB = B@sp@data
    
    # pre = ctA[1:head(ioA, 1)-1,]
    # if(dim(na.omit(pre))[1] <= 0)
    # {
    #   pre = ctB[1:head(ioB, 1)-1,]
    #   # pre_d = dB[1:head(ioB, 1)-1,]
    #   pre_d = dB@data[1:head(ioB, 1)-1,]
    # } else  pre_d = dA@data[1:head(ioA, 1)-1,]
    # 
    # 
    # post = ctB[tail(ioB, 1)+1:length(B),]
    # if(dim(na.omit(post))[1] <= 0)
    # {
    #   post = ctA[tail(ioA, 1)+1:length(A),]
    #   if(tail(ioA, 1)+1 < length(A))
    #     post_d = dA@data[tail(ioA, 1)+1:length(A),]
    #   else
    #     post_d = dA@data[length(A):length(A),]
    # } else  post_d = dB@data[tail(ioB, 1)+1:length(B),]
    
    pre = na.omit(ctA)
    # ctmid = na.omit(ctmid)
    post = na.omit(ctB)
    
    # pre_d = na.omit(dA)
    # # dmid = na.omit(dmid)
    # post_d = na.omit(dB)
    pre_d = dA[!all(is.na(dA)),]
    post_d = dB[!all(is.na(dB)),]
   
    colnames(pre) = c("ct_1", "ct_2", "t")
    colnames(post) = c("ct_1", "ct_2", "t")
    
    rownames(pre) = 1:dim(pre)[1]
    # rownames(ctmid) = (dim(pre)[1]+1):(dim(pre)[1]+dim(ctmid)[1])
    rownames(post) = (dim(pre)[1]+1):(dim(pre)[1]+dim(post)[1])
    
    rownames(pre_d) = rownames(pre)
    # rownames(dmid) = rownames(ctmid)
    rownames(post_d) = rownames(post)
    
    ct = rbind(pre, post)
    # ct = na.omit(ct)
    ad = rbind(pre_d, post_d)
    
    sp_obj = SpatialPointsDataFrame(coords = data.frame(ct[,1], ct[,2]), data = ad)
    stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
    dyn_merged = Track(stidf_obj)
  }
  return(dyn_merged)
}
