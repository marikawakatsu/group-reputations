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

    # variables
    const parameters_game = ["weak_selection","medium_selection","strong_selection"]
    const social_norms = ["SJ", "SS", "SC", "SH"]
    export parameters_game
    export social_norms

end
