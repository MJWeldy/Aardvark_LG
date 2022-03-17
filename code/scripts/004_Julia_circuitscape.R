#Circuitscape analysis in Julia
#C:\Users\weldy\AppData\Local\Programs\Julia-1.6.2\bin

library(JuliaCall)
#Sys.setenv(JULIA_NUM_THREADS = 4)
julia_setup(JULIA_HOME = "C:/Users/weldy/AppData/Local/Programs/Julia-1.6.3/bin")
julia <- julia_setup()
julia_command("pwd()") #Working directory for Julia
julia_library("Pkg")   
julia_install_package_if_needed("Circuitscape")
julia_library("Circuitscape")   
#julia_command('Pkg.test("Circuitscape")')
julia_command('compute("data/dist.ini")')
julia_command('compute("data/LG.ini")')
julia_command('compute("data/LG.ini")')
julia_command(
  
  "Threads.nthreads()"
  
)
# # In command Prompt
# P:
# cd PROJECTS\Aardvark_LG
# julia --threads 6
# # In julia
# using Pkg
# Pkg.add("Circuitscape")
# using Circuitscape
# julia --threads 12 ./code/scripts/circuitscape_optimization.jl
#Kruger
# compute("data/dist.ini") #Done
# compute("data/LG_glm.ini") #Done
# compute("data/LG_ME.ini") #Done

# compute("data/SA_DEM.ini") #Done
# compute("data/SA_TPI.ini") #Done
# compute("data/SA_VRM.ini") #Done
# compute("data/SA_MAP_1.ini") #Done
# compute("data/SA_MAP_2.ini") #Done
# compute("data/SA_Tmax_lin.ini") #Done
# compute("data/SA_Tmax_sq.ini") #in process

# compute("data/SA_dist.ini")

# compute("data/SA_Tmax_gaus.ini")