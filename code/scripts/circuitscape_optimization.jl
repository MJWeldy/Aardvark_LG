println("Loading Circuitscape")
using Circuitscape
println(Threads.nthreads())

#Distance
# println("running SA_distance 1/9")
# compute("data/SA_dist.ini")

#Gaussian DEM
#println("running SA_DEM 2/9")
#compute("data/SA_DEM.ini")

#TPI
#println("running SA_TPI 3/9")
#compute("data/SA_TPI.ini")

#VRM
#println("running SA_VRM 4/9")
#compute("data/SA_VRM.ini")

#MAP 1
#println("running SA_MAP_1 5/9")
#compute("data/SA_MAP_1.ini")

#MAP 1
#println("running SA_MAP_1 6/9")
#compute("data/SA_MAP_2.ini")

#Tmax_lin
#println("running SA_Tmax_lin 7/9")
#compute("data/SA_Tmax_lin.ini")

#Tmax_sq
#println("running SA_Tmax_sq 8/9")
#compute("data/SA_Tmax_sq.ini")

#Tmax_guas
println("running SA_Tmax_gaus 9/9")
compute("data/SA_Tmax_gaus.ini")