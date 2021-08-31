# Methods for the main simulation
"""
Function to generate an instance of Population
with randomized strategies and reputations
"""
function random_population(
    N::Int64,
    game::Game,
    norm::String = "SJ",
    prob::Float64 = 0.5,
    rate::Float64 = 1.0,
    group_sizes::Array{Int64, 1} = [25, 25],
    ind_reps_public::Bool = false,    # public
    grp_reps_public::Bool = false,    # public
    ind_reps_base_fix::Bool = true, # based on behavior
    grp_reps_base_fix::Bool = true, # based on behavior
    ind_reps_src_ind_fix::Bool = true,  # based on ind rep
    grp_reps_src_grp_fix::Bool = true,  # based on grp rep
    num_strategies::Int64 = 3,
    threshold::Float64 = 0.5
    )
    # Check group sizes
    if sum(group_sizes) != N @error("!!! sum of group sizes does not match total popualtion size !!!") end

    # Alloed strategies and groups
    num_groups     = length(group_sizes)
    all_groups     = collect(1:num_groups)
    all_strategies = collect(1:num_strategies)
    # Reputation update information
    ind_reps_base = repeat([ind_reps_base_fix], N) 
    grp_reps_base = repeat([grp_reps_base_fix], N) 
    ind_reps_src_ind  = repeat([ind_reps_src_ind_fix], N) 
    grp_reps_src_grp  = repeat([grp_reps_src_grp_fix], N) 
    # Random array of strategies
    strategies = rand(all_strategies, N)
    # Initialize group memberships
    membership = zeros(Int64, N)
    for i in 2:num_groups
        group_index = collect( group_sizes[i-1]+1:group_sizes[i-1]+group_sizes[i] )
        membership[group_index] .= i-1
    end
    # Random matrices of reputations
    reps_ind = rand([0,1], N, N)
    reps_grp = rand([0,1], N, num_groups)
    prev_reps_ind = rand([0,1], N, N)
    prev_reps_grp = rand([0,1], N, num_groups)
    # Random actions and fitnesses
    actions = rand([0,1], N, N)
    fitness = zeros(Float64, N)
    # Set 
    probs = repeat([prob], N)
    rates = repeat([rate], N)
    # Set generation count
    generation = 0

    return Population(
        N, game, norm,
        num_strategies, all_strategies,
        num_groups, all_groups, group_sizes,
        ind_reps_public, ind_reps_base, ind_reps_src,
        grp_reps_public, grp_reps_base, grp_reps_src, 
        threshold, strategies, membership, 
        reps_ind, reps_grp,
        prev_reps_ind, prev_reps_grp, 
        actions, fitness,
        probs, rates, generation
        )
end