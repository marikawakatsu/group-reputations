using Distributed
"loading workers..." |> println
workers = 1 + 12
nprocs() < workers && addprocs( workers - nprocs() )

"loading module..." |> println
(@__DIR__) == pwd() || cd(@__DIR__)
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
@everywhere include("GroupReputations.jl")

"loading utils..." |> println
@everywhere begin
    using .GroupReputations
    const id = parse(Int64, ENV["SLURM_ARRAY_TASK_ID"])

    #Fixed parameters
    # Population size
    const N    = 50
    # Payoff parameters
    const b    = 1.0
    const c    = 0.2
    # Selection strength
    const w    = 1
    # Mutation rates
    const u_s  = 1/N
    const u_p  = 1/N
    const u_a  = 1/N
    # Game parameters
    const game_pars = [b, c, w, u_s, u_p, u_a]
end

"loading parameters..." |> println
# Simulation parameters to sweep
@everywhere begin
    # Strategies and groups
    all_strategies = [1,2,3]
    group_sizes = [0.5, 0.5]
    # Reputation type
    ind_reps_public = true
    grp_reps_public = true
    # Based or not on behavior
    ind_reps_base_values = [false,true]
    grp_reps_base_values = [false,true]
    # Probability of using individual reps
    prob_values = 0.0:0.2:1.0
    # Rate of updating reps
    rate_values = 0.0:0.2:1.0
    # Initial generations without averaging
    burn_in = 5_000
    # Repetitions and length
    repetitions = 100
    initial_repetition = 0
    generations = 50_000
    # Title
    simulation_title = "sweep_src-prob-rate"
end

"running simulations..." |> println
@time run_simulations(
        N, game_pars, generations,
        repetitions, initial_repetition,
        simulation_title, social_norms[1:2],
        all_strategies, group_sizes,
        ind_reps_public, grp_reps_public,
        ind_reps_base_values, grp_reps_base_values,
        prob_values[id], rate_values,
        burn_in
        )
"DONE!" |> println

# "extracting data..." |> println
# extract_data(simulation_title)
# "...done!" |> println
