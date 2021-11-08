# Script to test run the module
begin
    (@__DIR__) == pwd() || cd(@__DIR__)
    any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
    # Load module
    using Revise
    using GroupReputations
    using StatsBase
    using Distributed, JLD
end

# Fixed parameters
@everywhere begin
    # Population size
    const N    = 10
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

# Simulation parameters to sweep
@everywhere begin
    all_strategies = [3]
    group_sizes = [0.5, 0.5]
    # Reputation type
    ind_reps_scale = [0,1,2]
    grp_reps_scale = [1]#0,1,2]
    # Recipient's membership sampling
    ind_recipient_membership = [0]#,1,2]
    grp_recipient_membership = [0]#,1,2]
    # Based or not on behavior
    ind_reps_base_values = true # [false,true]
    grp_reps_base_values = true # [false,true]
    # Probability of interacting with outgroup
    bias_values = 1.0 # 0.0:0.2:1.0
    # Probability of using group reps
    # prob_values = [ [0.0,i] for i in 0.0:0.2:1.0 ]
    prob_values = 0.0
    # Rate of updating reps
    rate_values = 1.0 # 0.0:0.2:1.0
    # Costs of using individual reps
    cost_values = 0.0 # :0.02:0.1
    # Based or not on temselves
    ind_reps_src_values = true # [false,true]
    grp_reps_src_values = true # [false,true]
    # Make assumptions about reputations
    ind_reps_assume = [0]
    grp_reps_assume = [0,1,2,3,4]
    # Initial generations without averaging
    burn_in = 0
    # Repetitions and length
    repetitions = 3
    initial_repetition = 0
    generations = 1_000
    # Title
    simulation_title = "DISC-assume"
end


@time run_simulations(
        N, game_pars, generations,
        repetitions, initial_repetition,
        simulation_title, social_norms,
        all_strategies, group_sizes,
        bias_values, prob_values, rate_values, cost_values,
        ind_reps_scale, grp_reps_scale,
        ind_recipient_membership, grp_recipient_membership,
        ind_reps_base_values, grp_reps_base_values,
        ind_reps_src_values, grp_reps_src_values,
        ind_reps_assume, grp_reps_assume,
        burn_in
        )