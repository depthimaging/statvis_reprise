source("coordinates.R")

loc2glob = function(local_json)
{
      C = cam[1,]
      for(tracks in 1:length(local_json$c1))
      {
        c1x = C$x + local_json$c1[[tracks]]$y
        c1y = C$y + local_json$c1[[tracks]]$x
        local_json$c1[[tracks]]$x = c1x
        local_json$c1[[tracks]]$y = c1y
      }

      C = cam[3,]
      # The following code for Camera #4 is working perfectly.
      # Since the data being used doesn't have data from camera 4, it is throwing an error for subscripts being out of bounds.
      # Just uncomment the following loop when there is data from camera #4
      # for(tracks in 1:length(local_json$c4))
      # {
      #   c4x = C$x + local_json$c4[[tracks]]$y
      #   c4y = C$y + local_json$c4[[tracks]]$x
      #   local_json$c4[[tracks]]$x = c4x
      #   local_json$c4[[tracks]]$y = c4y
      # }
    
      C = cam[2,]
      for(tracks in 1:length(local_json$c2))
      {
        c2x = C$x - local_json$c2[[tracks]]$x
        c2y = C$y + local_json$c2[[tracks]]$y
        local_json$c2[[tracks]]$x = c2x
        local_json$c2[[tracks]]$y = c2y
      }
      
  return(local_json)
}
