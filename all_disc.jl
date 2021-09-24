"loading module..." |> println
(@__DIR__) == pwd() || cd(@__DIR__)
any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
begin
    using GroupReputations
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
begin
    # Strategies and groups
    all_strategies = [3]
    group_sizes = [0.5, 0.5]
    # Reputation type
    ind_reps_scale = [0,1,2]
    grp_reps_scale = [0,1,2]
    # Based or not on behavior
    ind_reps_base_values = true # [false,true]
    grp_reps_base_values = true # [false,true]
    # Probability of interacting with outgroup
    bias_values = 1.0 # 0.0:0.2:1.0
    # Probability of using group reps
    prob_values = 0.0:0.2:1.0
    # Rate of updating reps
    rate_values = [0.0,1.0] # 0.0:0.2:1.0
    # Costs of using individual reps
    cost_values = 0.0:0.02:0.1
    # Based or not on temselves
    ind_reps_src_values = true # [false,true]
    grp_reps_src_values = true # [false,true]
    # Initial generations without averaging
    burn_in = 5_000
    # Repetitions and length
    repetitions = 100
    initial_repetition = 0
    generations = 50_000
    # Title
    simulation_title = "DISC-scale-prob-cost"
    # Parameters
    parameters = [ (bias,prob,rate,cost,ir,gr,ib,gb,is,gs) for
        ir in [ind_reps_scale...],
        gr in [grp_reps_scale...],
        ib in [ind_reps_base_values...],
        gb in [grp_reps_base_values...],
        bias in [bias_values...],
        prob in [prob_values...],
        rate in [rate_values...],
        cost in [cost_values...],
        is in [ind_reps_src_values...],
        gs in [grp_reps_src_values...]][:]
end

"running simulations..." |> println
@time run_simulations(
        N, game_pars, generations,
        repetitions, initial_repetition,
        simulation_title, social_norms,
        all_strategies, group_sizes,
        parameters[id]...
        )
"DONE!" |> println
