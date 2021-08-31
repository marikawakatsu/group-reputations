# Methods for tracking using struct Tracker

"""
Initialize Tracker
"""
function init_tracker(
    pop::Population,
    generations::Int64
    )

    inx = get_frequencies(pop) .!== 0.0  # indices for group-strategy combinations with nonzero freqs
    avg_fitness = zeros(Float64, pop.num_tags, pop.num_strategies)  # initialize avg_fitness

    # Allocate memory
    avg_cooperation        = get_cooperation(pop)
    avg_frequencies        = get_frequencies(pop)
    avg_reps_grp           = get_reps_grp(pop)
    avg_reps_ind           = get_reps_ind(pop)
    avg_fitness[inx]       = get_avg_fitness(pop)[inx]
    avg_global_cooperation = mean(pop.actions)

    # Register initial generation
    initial_generation = pop.generation+1
    final_generation   = pop.generation

    # Register population
    population_path = "Not Defined"
    # Register tracker in population
    pop.num_trackers +=1

    return Tracker( initial_generation,
                    final_generation,
                    avg_cooperation,
                    avg_frequencies,
                    avg_reps_grp,
                    avg_reps_ind,
                    avg_fitness,
                    avg_global_cooperation,
                    population_path
                )
end

"""
Update tracker during simulations
"""
function track!(
    tracker::Tracker,
    pop::Population
    )

    inx = get_frequencies(pop) .!== 0.0
    tracker.avg_cooperation        += (get_cooperation(pop)-tracker.avg_cooperation) / pop.generation
    tracker.avg_frequencies        += (get_frequencies(pop)-tracker.avg_frequencies) / pop.generation
    tracker.avg_reps_grp           += (get_reps_grp(pop)-tracker.avg_reps_grp) / pop.generation
    tracker.avg_reps_ind           += (get_reps_ind(pop)-tracker.avg_reps_ind) / pop.generation
    tracker.avg_fitness[inx]       += ( (get_avg_fitness(pop)-tracker.avg_fitness) / pop.generation )[inx]
    tracker.avg_global_cooperation += (mean(pop.actions)-tracker.avg_global_cooperation) / pop.generation
    # Count generation
    tracker.final_generation += 1
end

_round(x) = round.( x; digits=3 )

"""
Report progress during simulation
!!! CHECK THE SHAPE OF avg_reps_grp and avg_reps_ind !!!
"""
function _report(
    tracker::Tracker,
    pop::Population,
    gen::Int64
    )

    cooperation = tracker.avg_cooperation |> _round
    frequencies = tracker.avg_frequencies |> _round
    reps_grp    = tracker.avg_reps_grp    |> _round
    reps_ind    = tracker.avg_reps_ind    |> _round
    fitness     = tracker.avg_fitness     |> _round

    if pop.num_groups <= 2
        "Generation           =\t$gen" |> println
        "Group 1: \n" |> print
        "\tSize           =\t$(pop.N1) \n"           |> print
        "\tCooperation    =\t$(cooperation[1,:])"    |> println
        "\tStrategies     =\t$(frequencies[1,:]) \n" |> print
        "\tGroup reps     =\t$(reps_grp[1,:]) \n"    |> print
        "\tIndiv reps     =\t$(reps_ind[1,:]) \n"    |> print
        "\tAvg Fitness    =\t$(fitness[1,:]) \n"     |> print
        "Group 2: \n" |> print
        "\tSize           =\t$(pop.N-pop.N1) \n"     |> print
        "\tCooperation    =\t$(cooperation[2,:])"    |> println
        "\tStrategies     =\t$(frequencies[2,:]) \n" |> print
        "\tGroup reps     =\t$(reps_grp[2,:]) \n"    |> print
        "\tIndiv reps     =\t$(reps_ind[2,:]) \n"    |> print
        "\tAvg Fitness    =\t$(fitness[2,:]) \n"     |> print
        "-"^50  |> println
    else
        "There are more than 2 groups; udpate _report function to display intermediate results" |> print
    end

end

"""
Merge a list of trackers
"""

# mutable struct Tracker

#     initial_generation::Int64          # initial generation of the corresponding simulation run
#     final_generation::Int64            # final generation of the corresponding simulation run
#     avg_cooperation::Array{Float64, 2} # a num_groups-by-num_groups matrix tracking average within- and between-group cooperation
#     avg_frequencies::Array{Float64, 2} # a num_groups-by-num_strategies matrix tracking average strategy frequencies per group
#     avg_reps_grp::Array{Float64, 1}    # a num_groups-element array tracking average group reputations
#     avg_reps_ind::Array{Float64, 1}    # a num_groups-element array tracking average individual reputations per group
#     avg_fitness::Array{Float64, 2}     # a num_groups-by-num_strategies matrix tracking average fitness of each strategy per group
#     avg_global_cooperation::Float64    # a float tracking average global cooperation
#     population_path::String            # path to the stored population

# end