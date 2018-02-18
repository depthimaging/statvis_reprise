#Function for static merging where A and B are two tracks which are to be merged.
#The data for the overlapped portions of the two tracks are chosen from the camera which
#is the closest. The coordinates of the overlapped points are calculated as Arithmetic Mean
#of the positions reported by the two cameras
merge_static = function(A, B)
{
  #Swap tracks A and B if A starts after B.
  #This makes sure that A always start before B
  if(head(time(A), 1) > head(time(B), 1))
  {
    C = A
    A = B
    B = C
  }
  
  #Some variable naming conventions used as prefixes
  #ct = Coordinates & Time
  #c = Coordinates only
  #cx = x-coordinate
  #cy = y-coordinate
  #t = Time only
  #d = Data
  #io = Overlap
  
  #Some variable conventions used as postfixes
  #A = Track A only
  #B = Track B only
  #AB = Both Tracks A & B
  
  #Storing commonly used parameters in variables for easier access later on
  camA = head(unique(A@sp$camera),1)
  camB = head(unique(B@sp$camera),1)
  overlap = duplicate_trackpoints(A, B)
  overlap = subset(overlap, stD == TRUE)
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
  
  #pre represents the portion of Track A before overlap starts with Track B 
  pre = ctA[1:head(ioA, 1)-1,]
  if(dim(na.omit(pre))[1] <= 0)
  {
    pre = ctB[1:head(ioB, 1)-1,]
    # pre_d = dB[1:head(ioB, 1)-1,]
    pre_d = dB@data[1:head(ioB, 1)-1,]
  } else  pre_d = dA@data[1:head(ioA, 1)-1,]
  
  #post represents the portion of Track B after the portion of overlap with Track A
  post = ctB[tail(ioB, 1)+1:length(B),]
  if(dim(na.omit(post))[1] <= 0)
  {
    post = ctA[tail(ioA, 1)+1:length(A),]
    if(tail(ioA, 1)+1 < length(A))
      post_d = dA@data[tail(ioA, 1)+1:length(A),]
    else
      post_d = dA@data[length(A):length(A),]
  } else  post_d = dB@data[tail(ioB, 1)+1:length(B),]
  
  #mid is the portion where there is detected overlap
  ctmidAB = data.frame(ctA[head(ioA, 1):tail(ioA, 1),], ctB[head(ioB, 1):tail(ioB, 1),])
  dmidA = dA[head(ioA, 1):tail(ioA, 1),]
  dmidB = dB[head(ioB, 1):tail(ioB, 1),]
  ctmid_x = vector(mode = "double", length = dim(ctmidAB)[1])
  ctmid_y = vector(mode = "double", length = dim(ctmidAB)[1])
  ctmid_t = ctmidAB[,3]
  
  #Column-Binds the xy coordinates and time into a data frame
  ctmid = data.frame(cbind(ctmid_x, ctmid_y, ctmid_t))
  ctmid[,3] = ctmid_t
  
  dmidA = dmidA@data
  dmidB = dmidB@data
  dmid = data.frame(matrix(nrow = dim(dmidA)[1], ncol = dim(dmidB)[2]))
  colnames(dmid) = colnames(dmidA)
  dmid$time = dmidA$time
  
  #Calculates the Arithmetic Mean of the x and y coordinates
  avg_cx = (ctmidAB[,1] + ctmidAB[,4])/2
  avg_cy = (ctmidAB[,2] + ctmidAB[,5])/2
  
  ABi = 1
  #Finds the camera (out of the 2 cameras for each track) which is closest for each point of overlap.
  #The camera number is read from the attribute already in the track objects
  #and the distance is calculated by reading the coordinate for that particular camera number
  #which was defined previously
  for(location in 1:dim(ctmidAB)[1])
  {
    #Finds the distances to the cameras
    distA = calc_spatial_dist(cam[camA,], c(avg_cx[location], avg_cy[location]))
    distB = calc_spatial_dist(cam[camB,], c(avg_cx[location], avg_cy[location]))
    # print(distA - distB)
    pt_data = data.frame()
    
    #If camera of Track B is closer than the camera for Track A
    #use the data from Track B. For points with "unknown" data, choose the other camera
    if(distA > distB)
    {
      ctmid[location,] = ctmidAB[ABi,4:6]
      pt_data = dmidB[ABi,]
      pt_dataA = dmidA[ABi,]
      for(d in 4:6) if(pt_data[d] == "Unknown" && pt_dataA[d] != "Unknown") pt_data[d] = pt_dataA[d]
      
    } else
    {
      #If camera of Track A is closer than the camera for Track B
      #use the data from Track A. For points with "unknown" data, choose the other camera
      ctmid[location,] = ctmidAB[ABi, 1:3]
      pt_data = dmidA[ABi,]
      pt_dataB = dmidB[ABi,]
      for(d in 4:6) if(pt_data[d] == "Unknown" && pt_dataB[d] != "Unknown") pt_data[d] = pt_dataB[d]
    }
    dmid[location,] = pt_data
    ABi = ABi + 1
  }

  #Post-process the data
  
  #Remove NAs
  pre = na.omit(pre)
  ctmid = na.omit(ctmid)
  post = na.omit(post)
  
  pre_d = na.omit(pre_d)
  dmid = na.omit(dmid)
  post_d = na.omit(post_d)
  
  #Correct the column and row names which were modified after slicing
  colnames(pre) = colnames(ctmid)
  colnames(post) = colnames(ctmid)
  
  rownames(pre) = 1:dim(pre)[1]
  rownames(ctmid) = (dim(pre)[1]+1):(dim(pre)[1]+dim(ctmid)[1])
  rownames(post) = (dim(pre)[1]+dim(ctmid)[1]+1):(dim(pre)[1]+dim(ctmid)[1]+dim(post)[1])
  rownames(pre_d) = rownames(pre)
  rownames(dmid) = rownames(ctmid)
  rownames(post_d) = rownames(post)
  
  #Row-bind the pre, mid and post data frames in the correct order (coordinates and data)
  ct = rbind(pre, ctmid, post)
  ad = rbind(pre_d, dmid, post_d)
  
  #Re-create an object of class Track as done previously from the merged data frames
  sp_obj = SpatialPointsDataFrame(coords = data.frame(ct[,1], ct[,2]), data = ad)
  stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
  st_merged = Track(stidf_obj)
  return(st_merged)
}

#Function for dynamic merging with two tracks, A and B.
#Returns a merged object of class Track
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
  
  #If there is no temporal overlap
  if(flag)
  {
    #Define commonly-used variables for easier access
    cA = coordinates(A)
    cB = coordinates(B)
    tA = time(A)
    tB = time(B)
    ctA = data.frame(cA, tA)
    ctB = data.frame(cB, tB)
    dA = A@sp@data
    dB = B@sp@data

    #Post-processing
    #No "mid" is required (only "pre" and "post" is necessary)
    #because there is no overlap
    
    #Remove NAs
    pre = na.omit(ctA)
    post = na.omit(ctB)
    
    pre_d = dA[!all(is.na(dA)),]
    post_d = dB[!all(is.na(dB)),]
    
    #Correct row and column names
    colnames(pre) = c("ct_1", "ct_2", "t")
    colnames(post) = c("ct_1", "ct_2", "t")
    
    rownames(pre) = 1:dim(pre)[1]
    rownames(post) = (dim(pre)[1]+1):(dim(pre)[1]+dim(post)[1])
    
    rownames(pre_d) = rownames(pre)
    rownames(post_d) = rownames(post)
    
    #Bind the results
    ct = rbind(pre, post)
    ad = rbind(pre_d, post_d)
    
    #Re-create an object of class Track as done previously
    sp_obj = SpatialPointsDataFrame(coords = data.frame(ct[,1], ct[,2]), data = ad)
    stidf_obj = STIDF(sp = sp_obj, time = sp_obj@data$time, data = subset(data.frame(misc = sp_obj@data), select = -c(2)))
    dyn_merged = Track(stidf_obj)
  }
  return(dyn_merged)
}
