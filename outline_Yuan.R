library("fpc")

outline = function(globalized_tracks)
{
  #two tasks
  #1.set cluster attributes example: globalized_tracks[1]@data$'db_track12$cluster'
  #2.set is fro/phantom attributes true/false example
  globalized_tracks_cleaned = list()
  whichloop = 0
  
  for(track12 in globalized_tracks)
  {
    #using DBscan to cluster the dataset
    #chosing the right Eps: calculate the average distance of minpts' points
    isfro = FALSE
    db_track12 = dbscan(track12@sp@coords,eps=0.13,MinPts=5)
    # plot(db_track12,track12@sp@coords,type="b")
    
    #connect the same cluster like the bubble compare near one pair of points
    countflag = 0
    count = 0
    for(temp in 1:(length(db_track12$cluster)-1))
    {
      if(db_track12$cluster[temp] != db_track12$cluster[temp+1])
      {
        if(db_track12$cluster[temp+1] == 0)
        {
          countflag=temp+1
        }
        if(db_track12$cluster[temp+1] == count+1)
        {
          count=count+1
        } else {
          if(db_track12$cluster[temp+1] != 0)
          {
            db_track12$cluster[c(countflag:temp)]=count
          }
        }
        
      }
    }
    track12@data = cbind(track12@data,db_track12$cluster)
    #calculate the size of cluster using the avarege distance of cluster point
    # plot(db_track12,track12@sp@coords,type="b")
    if(count != 0)
    {
      for(clusterIndex in 1:count)
      {
        subtrack = track12@connections$distance[db_track12$cluster==clusterIndex]
        subtrack<-na.omit(subtrack)
        if(mean(subtrack) > 0.1)
        {
          isfro = TRUE
        }
      }
    }
    if(isfro == FALSE)
    {
      globalized_tracks_cleaned = c(globalized_tracks_cleaned,track12)
    }
    
    # calulate the center
    #colMeans(track12@sp@coords[db_track12$cluster==1])
    
    whichloop = whichloop+1
  }
  
  return(globalized_tracks_cleaned)
}