using Distributed
workers = 32
nprocs() < workers && addprocs( workers - nprocs() )
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
@everywhere include("GroupReputations.jl")
@everywhere using .GroupReputations

simulation_title = "DISC-scale-prob-cost"
extract_data_DISC(simulation_title)
