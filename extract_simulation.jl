using Distributed
workers = 1 + 12
nprocs() < workers && addprocs( workers - nprocs() )
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
@everywhere include("GroupReputations.jl")
@everywhere using .GroupReputations

simulation_title = "grp_scale-prob-rate-cost"
extract_data(simulation_title)
