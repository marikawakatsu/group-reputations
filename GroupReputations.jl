module GroupReputations

    #include("./module/requirements.jl")
    using Random, StatsBase, Statistics
    using JLD, Distributed
    using SharedArrays, CSV, DataFrames

    # functions
    include("./module/structs.jl")
    export Game, Population, Tracker

    include("./module/methods/evolution.jl")
    export update_individual_reputations!
    export update_group_reputations!
    export update_actions!
    export update_fitness!
    export update_strategies!
    export mutate!
    export evolve!
    export _ind_reputation, _grp_reputation
    export _norm, _action

    include("./module/methods/simulation.jl")
    export random_population
    export run_simulations
    ### !!! ADD OTHER FUNCTIONS HERE !!! ###

    include("./module/methods/tracker.jl")
    export init_tracker
    export track!
    export _merge
    export _report

    include("./module/methods/get_functions.jl")
    export _round
    export get_cooperation
    export get_frequencies
    export get_reps_ind
    export get_reps_grp
    export get_stereotypes
    export get_avg_fitness

    # variables
    const social_norms = ["SJ", "SS", "SC", "SH"]
    export social_norms

end
