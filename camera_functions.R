#This script contains a number of utility functions that were used
#in a number of other scripts. The utility functions defined here are
#quite generic and can be used for a variety of purposes

#This function calculates the Euclidean distance between any two given coordinates
#Takes as input two numeric arrays of length 2 representing the coordinates.
#The x-coordinate of the point is in position 1 of the array and
#the y-coordinate is in position 2.
calc_spatial_dist = function(cA, cB)
{
  sq_dist = (cB[1] - cA[1])^2 + (cB[2] - cA[2])^2
  return(sqrt(sq_dist))
}

#This function calculates the temporal distance between two timestamps
#in a standard time format (e.g. POSIX). It returns the absolute time difference
#and thus the order of the inputs (tA and tB) does not matter.
#The units of the difference are calculated automatically from the units
#of the inputs
calc_temporal_dist = function(tA, tB)
{
  return(abs(difftime(tA, tB)))
}

#This function returns a boolean result from a vector of distances, x.
#x is a vector containing differences in distances between two points.
#This function returns TRUE if ALL the values in that vector are less than or equal to
#the threshold value which is supplied as an argument to the function and FALSE otherwise.
#If no explicit
#threshold value is passed, a default value of 0.1m is used.
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

#This function is similar to the function above and is its temporal counterpart.
#This function also takes as input a vector x containing time differences between two timestamps
#as a difftime object. It returns TRUE if ALL the values in that vector are less than or equal to
#the threshold parameter passed as an argument and FALSE otherwise. Again, the default threshold value to be used
#in case no such argument is passed is 1 second. It is to be noted that the value passed
#to the argument "treshold" (treshold = ) has to be an object of class difftime.
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
