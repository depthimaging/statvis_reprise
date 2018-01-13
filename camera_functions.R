calc_spatial_dist = function(cA, cB)
{
  sq_dist = (cB[1] - cA[1])^2 + (cB[2] - cA[2])^2
  return(sqrt(sq_dist))
}

calc_temporal_dist = function(tA, tB)
{
  return(abs(difftime(tA, tB)))
}

cross_spatial_treshold = function(x, treshold = 0.1)
{
  d_bool = vector(length = length(x))
  i = 1
  for(entry in x)
  {
    if(entry <= treshold) d_bool[i] = TRUE
    i = i+1
  }
  return(d_bool)  
}

cross_temporal_treshold = function(x, treshold = as.difftime(1, units = "secs"))
{
  d_bool = vector(length = length(x))
  i = 1
  for(entry in x)
  {
    if(entry <= treshold) d_bool[i] = TRUE
    i = i+1
  }
  return(d_bool)
}
