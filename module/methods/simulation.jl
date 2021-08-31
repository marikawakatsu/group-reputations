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
    norm::String = "SJ";
    ind_reps_public::Bool = true,      # public
    grp_reps_public::Bool = true,      # public
    all_strategies::Array{Int64,1} = [1,2,3],
    threshold::Float64 = 0.5,
    group_sizes = [0.5, 0.5],
    prob_values = 0.5,
    rate_values = 1.0,
    ind_reps_base_values = true,     # based on behavior
    grp_reps_base_values = true,     # based on behavior
    ind_reps_src_ind_values = true,  # based on ind rep
    grp_reps_src_grp_values = false,  # based on ind rep
    prob_weights = [0.5, 0.5],
    rate_weights = [0.5, 0.5],
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
        threshold, strategies, membership,
        reps_ind, reps_grp,
        prev_reps_ind, prev_reps_grp,
        actions, fitness,
        probs, rates, generation
        )
end
