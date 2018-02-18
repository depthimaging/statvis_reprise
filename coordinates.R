#Coordinate values of cameras, exhibits and points of interest
#Values obtained from site survey

c1 = c(2.280, -0.080)
c2 = c(4.583, 4.050)
c4 = c(0.570, -0.080)
cam = as.data.frame(rbind(c1,c2,c4))
colnames(cam) = c("x", "y")

er_s2 = c(cam$x[[2]], cam$y[[2]] - 0.24)
er_s1 = c(er_s2[1] + 0.4, er_s2[2])
er_e3 = c(er_s2[1], er_s2[2] + 2.00)
er_e4 = c(er_s1[1], er_s1[2] + 2.00)

p1_s = c(er_e3[1] - 0.1 - 0.21, er_e3[2] + 0.15)
p1_e = c(p1_s[1] - 0.6, p1_s[2])

pc_s = c(p1_e[1] - 0.15, p1_e[2])
pc_e = c(pc_s[1] - 0.3, pc_s[2])

p2_s = c(pc_e[1] - 0.27 - 0.48, pc_e[2])
p2_e = c(p2_s[1] - 0.40, p2_s[2])

p3_s = c(p2_e[1] - 0.35 - 0.41, p2_e[2] - 0.45)
p3_e = c(p3_s[1] - 0.6, p3_s[2])

tv_s = c(p3_e[1] - 0.78, p3_e[2])
tv_e = c(tv_s[1] - 0.54, tv_s[2])

eb_s = c(0,0)
eb_e = c(cam$x[[1]] + 0.55, 0)

#Still need to model the walls, pillars and extent of the room

find_mids = function(x, y)
{
  return(c((x[1] + y[1])/2, (x[2] + y[2])/2))
}

er_m14 = find_mids(er_s1, er_e4)
er_m23 = find_mids(er_s2, er_e3)
er_m = find_mids(er_m14, er_m23)

p1_m = find_mids(p1_s, p1_e)

pc_m = find_mids(pc_s, pc_e)

p2_m = find_mids(p2_s, p2_e)

p3_m = find_mids(p3_s, p3_e)

tv_m = find_mids(tv_s, tv_e)

eb_m = find_mids(eb_s, eb_e)



