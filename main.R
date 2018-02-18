# Global wrapper

#Importing, cleaning & CRS
source("import.R")

#Analyzing
source("analyzetracks.R")

#Calculating statistics
source("analyzestats.R")

#Call track_details() with track number
#to generate track summaries and interactive animated plots
track_details(1)