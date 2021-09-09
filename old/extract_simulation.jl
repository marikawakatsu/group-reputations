using Distributed
workers = 1 + 12
nprocs() < workers && addprocs( workers - nprocs() )
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
@everywhere include("GroupReputations.jl")
@everywhere using .GroupReputations

simulation_title = "sweep_src-prob-rate"
extract_data(simulation_title)
