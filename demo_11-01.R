#Demo for interactive_static
static = readRDS("generated/dup_det_3_4.Rda")
print("Spatial Threshold = 0.1m and Temporal Threshold = 1s")
static


#Demo for interactive_dynamic
dynamic = readRDS("generated/dynamic_2_3.Rda")
dynamic
print("Spatial Threshold = 0.1m and Temporal Threshold = 1s")
spatial_distance = calc_spatial_dist(c(dynamic$endcoord.tracks.x, dynamic$endcoord.tracks.y), c(dynamic$startcoord.tracks.x, dynamic$startcoord.tracks.y))
temporal_distance = calc_temporal_dist(dynamic$starttime, dynamic$endtime)
print("Spatial Distance: ")
print(spatial_distance)
print("Temporal Distance: ")
print(temporal_distance)

visitor_ids
