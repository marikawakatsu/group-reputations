struct Game

    b::Float64      # benefit
    c::Float64      # cost
    w::Float64      # selection strength
    u_s::Float64    # mutation rate
    u_p::Float64    # performance error
    u_a::Float64    # assignment error

    function Game(
        b::Float64,
        c::Float64,
        w::Float64,
        u_s::Float64,
        u_p::Float64,
        u_a::Float64,
        )
        return new(b, c, w, u_s, u_p, u_a)
    end
end

mutable struct Population
    # Size
    N::Int64                            # number of individuals
    # Interactions
    game::Game                          # game parameters
    norm::String                        # social norm for reps updating
    # Strategies
    num_strategies::Int64               # number of strategies
    all_strategies::Array{Int64, 1}     # array of allowed strategies
    # Groups
    num_groups::Int64                   # number of groups
    all_groups::Array{Int64, 1}         # array of allowed groups
    group_sizes::Array{Float64, 1}      # array of relative group sizes
    # Individual reputations
    ind_reps_scale::Int32               # individual reputation type: public or private
    ind_reps_base::Array{Bool, 1}       # individuals reps are based on behavior (1) or not (0)
    ind_reps_src_ind::Array{Bool, 1}    # update individual reps based on individual (1) or group (0) reputation
    # Group reputations
    grp_reps_scale::Int32               # group reputation type: public or private
    grp_reps_base::Array{Bool, 1}       # group reps are based on behavior (1) or not (0)
    grp_reps_src_grp::Array{Bool, 1}    # update group reps based on individual (0) or group (1) reputation
    # Storage
    strategies::Array{Int64, 1}         # array of strategies
    membership::Array{Int64, 1}         # array of group memberships
    reps_ind::Array{Int64, 2}           # matrix of individual reputations
    reps_grp::Array{Int64, 2}           # matrix of group reputations
    prev_reps_ind::Array{Int64, 2}      # matrix of previous individual reputations
    prev_reps_grp::Array{Int64, 2}      # matrix of previous group reputations
    actions::Array{Int64, 2}            # matrix of actions
    fitness::Array{Float64, 1}          # array of fitness
    probs::Array{Float64, 1}            # array of probs of acting using group reps
    rates::Array{Float64, 1}            # array of rates of updating group reps
    costs::Array{Float64, 1}            # array of costs of using individual reps
    # Current generation
    generation::Int64
end

mutable struct Tracker

    initial_generation::Int64          # initial generation of the corresponding simulation run
    final_generation::Int64            # final generation of the corresponding simulation run
    avg_cooperation::Array{Float64, 2} # a num_groups-by-num_groups matrix tracking average within- and between-group cooperation
    avg_frequencies::Array{Float64, 2} # a num_groups-by-num_strategies matrix tracking average strategy frequencies per group
    avg_reps_grp::Array{Float64, 2}    # a num_groups-by_num_groups matrix tracking average group reputations
    avg_reps_ind::Array{Float64, 2}    # a num_groups-by_num_groups matrix tracking average individual reputations per group
    avg_fitness::Array{Float64, 2}     # a num_groups-by-num_strategies matrix tracking average fitness of each strategy per group
    avg_global_cooperation::Float64    # a float tracking average global cooperation
    population_path::String            # path to the stored population

end
