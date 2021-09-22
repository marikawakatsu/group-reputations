using Distributed
workers = 32
nprocs() < workers && addprocs( workers - nprocs() )
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
@everywhere include("GroupReputations.jl")
@everywhere using .GroupReputations

simulation_title = "scale-bias-prob-cost"
extract_data(simulation_title)
