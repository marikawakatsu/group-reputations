# Methods for the main simulation

"""
Sample vector of `size` parameters from `values` proportional to `weights`.
Or sample uniformly if `weights` not given.
"""
function sample_parameters(
    size,
    values = [0,1],
    weights = [0.5,0.5]
    )
    # Given weights
    if length(values) == length(weights)
        result = sample([values...], Weights([weights...]),size)
    # Sample uniformly
    else
        result = sample([values...],size)
    end

    return result
end

"""
Assign group membership given relative sizes of the groups
"""
function assign_membership(
        N::Int64,
        group_sizes::Array{Float64, 1},
        num_groups::Int64
    )
    # Allocate memory
    membership = ones(Int64, N)
    # Index
    group_index = Int.(floor.(group_sizes./sum(group_sizes)*N)) |> cumsum
    # Assignment
    for i in 2:num_groups
        membership[group_index[i-1]+1:group_index[i]] .= i
    end

    return membership
end

"""
Function to generate an instance of Population
with randomized strategies and reputations
"""
function random_population(
    N::Int64,
    game::Game,
    norm = "SJ",
    all_strategies = [1,2,3],
    group_sizes = [0.5, 0.5],
    ind_reps_public = true,      # public
    grp_reps_public = true,      # public
    ind_reps_base_values = true,     # based on behavior
    grp_reps_base_values = true,     # based on behavior
    prob_values = 0.5,
    rate_values = 1.0,
    cost_values = 0.0,
    ind_reps_src_ind_values = true,  # based on ind rep
    grp_reps_src_grp_values = false,  # based on ind rep
    prob_weights = [0.5, 0.5],
    rate_weights = [0.5, 0.5],
    cost_weights = [0.5, 0.5],
    ind_reps_base_weights = [0.5,0.5],
    grp_reps_base_weights = [0.5,0.5],
    ind_reps_src_ind_weights = [0.5,0.5],
    grp_reps_src_grp_weights = [0.5,0.5],
    )
    # Strategies
    num_strategies      = length(all_strategies)
    strategies          = sample_parameters( N, all_strategies)
    # Groups
    num_groups          = length(group_sizes)
    all_groups          = collect(1:num_groups)
    membership          = assign_membership( N, group_sizes, num_groups)
    # Mechanisms
    probs               = sample_parameters( N, prob_values, prob_weights)
    rates               = sample_parameters( N, rate_values, rate_weights)
    costs               = sample_parameters( N, cost_values, cost_weights)
    # Based or not in behavior
    ind_reps_base       = sample_parameters( N, ind_reps_base_values, ind_reps_base_weights)
    grp_reps_base       = sample_parameters( N, grp_reps_base_values, grp_reps_base_weights)
    # Sources of information
    ind_reps_src_ind    = sample_parameters( N, ind_reps_src_ind_values, ind_reps_src_ind_weights)
    grp_reps_src_grp    = sample_parameters( N, grp_reps_src_grp_values, grp_reps_src_grp_weights)
    # Initial conditions
    reps_ind            = sample_parameters((N, N))
    prev_reps_ind       = sample_parameters((N, N))
    reps_grp            = sample_parameters((N, num_groups))
    prev_reps_grp       = sample_parameters((N, num_groups))
    if ind_reps_public
        for j in 1:N
            reps_ind[:,j] .= reps_ind[1,j]
            prev_reps_ind[:,j] .= prev_reps_ind[1,j]
        end
    end
    if grp_reps_public
        for j in 1:num_groups
            reps_grp[:,j] .= reps_grp[1,j]
            prev_reps_grp[:,j] .= prev_reps_grp[1,j]
        end
    end
    actions             = sample_parameters((N, N))
    fitness             = sample_parameters( N, 0.0)
    # Set generation count
    generation = 0

    return Population(
        N, game, norm,
        num_strategies, all_strategies,
        num_groups, all_groups, group_sizes,
        ind_reps_public, ind_reps_base, ind_reps_src_ind,
        grp_reps_public, grp_reps_base, grp_reps_src_grp,
        strategies, membership,
        reps_ind, reps_grp,
        prev_reps_ind, prev_reps_grp,
        actions, fitness,
        probs, rates, costs, generation
        )
end

"""
Run simulations in parallel.
Sweeping parameters:
    - social norms
    - reputation public or private
    - reputation based or nor on behavior
    - probability of using individual over group reputations
    - rate of reputation updating
"""
function run_simulations(
            N::Int64,
            game_pars::Vector,
            generations::Int64,
            repetitions::Int64,
            initial_repetition::Int64,
            simulation_title::String,
            social_norms = "SJ",
            all_strategies = [1,2,3],
            group_sizes = [0.5, 0.5],
            ind_reps_public = true,
            grp_reps_public = true,
            ind_reps_base_values = true,
            grp_reps_base_values = true,
            prob_values = 0.5,
            rate_values = 1.0,
            cost_values = 0.0,
            burn_in = 5_000,
            ind_reps_src_values = true,
            grp_reps_src_values = false,
            report = Inf
        )


    reps = initial_repetition:(initial_repetition+repetitions-1)
    index = [ (r,norm,ir,gr,ib,gb,is,gs,prob,rate) for  r in reps,
                                                norm in [social_norms...],
                                                ir in [ind_reps_public...],
                                                gr in [grp_reps_public...],
                                                ib in [ind_reps_base_values...],
                                                gb in [grp_reps_base_values...],
                                                is in [ind_reps_src_values...],
                                                gs in [grp_reps_src_values...],
                                                prob in [prob_values...],
                                                rate in [rate_values...],
                                                cost in [cost_values...]][:]

    @sync @distributed for i in index
        (r,norm,ir,gr,ib,gb,is,gs,prob,rate,cost) = i
        # Parameters path
        path  = "results/"*
                "$simulation_title/"*
                "norm$norm-"*
                "type$(Int(ir))$(Int(gr))-"*
                "base$(Int(ib))$(Int(gb))-"*
                "src$(Int(is))$(Int(gs))-"*
                "prob$prob-rate$rate"
        !ispath(path) && mkpath(path)
        # Files
        pop_file = path * "/pop_$r.jld"
        tracker_file = path * "/tracker_$r.jld"
        # Get population and tracker
        if !isfile(pop_file)
            # Get Game
            game = Game(game_pars...)
            # Get Population
            pop  = random_population( N, game, norm, all_strategies, group_sizes,
                                    ir, gr, ib, gb, prob, rate, cost, is, gs)
            # Burn in generations
            [ evolve!(pop) for _ in burn_in ]
            pop.generation = 0
            # Get Tracker
            tracker = init_tracker(pop)
        else
            # if population exists, load it
            pop = load(pop_file,"pop")
            # load tracker for expanding simulation
            tracker = load(tracker_file,"tracker")
        end
        # Relate population with tracker
        tracker.population_path = pop_file
        # if generations not reached
        if pop.generation < generations
            # Run generations
            for gen in 1:(generations-pop.generation)
                # evolve
                evolve!(pop)
                # update tracker
                track!(tracker,pop)
                # report
                (gen % report == 0) && _report(tracker,pop)
            end
        end
        # Save Population
        save(pop_file, "pop", pop)
        # Save Tracker
        save(tracker_file, "tracker", tracker)
        # Report finish
        types = ["private","public "]
        bases = ["random  ","behavior"]
        sources = ["other","self "]
        ">>  $norm  |  "*
        "ind : $(types[Int(ir)+1]) - $(bases[Int(ib)+1]) - $(sources[Int(is)+1])  |  "*
        "grp : $(types[Int(gr)+1]) - $(bases[Int(gb)+1]) - $(sources[Int(gs)+1])  |  "*
        "prob : $prob  |  rate : $rate  |  cost : $cost" |> println
    end
end
