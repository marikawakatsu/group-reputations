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
    all_strategies::Array{Int64, 1}     # allowed strategies
    # Groups
    num_groups::Int64                   # number of groups
    all_groups::Array{Int64, 1}         # allowed groups
    group_sizes::Array{Int64, 1}        # size of each group
    # Individual reputations
    ind_reps_type::Bool                 # public or private
    ind_reps_base::Array{Bool, 1}       # based on behavior or not
    ind_reps_src::Array{Bool, 1}        # use RI or RG for updating
    # Group reputations
    grp_reps_type::Bool                 # public or private
    grp_reps_base::Array{Bool, 1}       # based on behavior or not
    grp_reps_src::Array{Bool, 1}        # use RI or RG for updating
    # Storage
    strategies::Array{Int64, 1}         # strategies
    membership::Array{Int64, 1}         # group membership
    reps_ind::Array{Int64, 2}           # individual reputations
    reps_grp::Array{Int64, 2}           # group reputations
    actions::Array{Int64, 2}            # actions
    fitnesses::Array{Float64, 1}        # fitness
    probs::Array{Float64, 1}            # probs of acting using individual reps
    rates::Array{Float64, 1}            # rate of updating group reps
    # Current generation
    generation::Int64
end

mutable struct Tracker
    initial_generation::Int64
    final_generation::Int64
    group_cooperation::Array{Float64, 2}
    group_frequencies::Array{Float64, 2}
    group_reputations::Array{Float64, 2}
    group_avg_fitness::Array{Float64, 2}
    global_cooperation::Float64
    population_path::String
end
mutable struct Tracker

    initial_generation::Int64          # initial generation of the corresponding simulation run 
    final_generation::Int64            # final generation of the corresponding simulation run
    avg_cooperation::Array{Float64, 2} # a num_groups-by-num_groups matrix tracking average within- and between-group cooperation
    avg_frequencies::Array{Float64, 2} # a num_groups-by-num_strategies matrix tracking average strategy frequencies per group
    avg_reps_grp::Array{Float64, 1}    # a num_groups-element array tracking average group reputations
    avg_reps_ind::Array{Float64, 1}    # a num_groups-element array tracking average individual reputations per group
    avg_fitness::Array{Float64, 2}     # a num_groups-by-num_strategies matrix tracking average fitness of each strategy per group
    avg_global_cooperation::Float64    # a float tracking average global cooperation
    population_path::String            # path to the stored population

end