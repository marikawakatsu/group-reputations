# Script to test run the module
begin
    (@__DIR__) == pwd() || cd(@__DIR__)
    any(LOAD_PATH .== pwd()) || push!(LOAD_PATH, pwd())
    # Load module
    using Revise
    using GroupReputations
    using StatsBase
end

# Set parameters
begin
    # Population size
    const N    = 10
    # Payoff parameters
    const b    = 1.0
    const c    = 0.2
    # Selection strength
    const simulation_title = "strong_selection"   # "weak_selection"
    const w    = 0.1     # 0.01
    # Mutation rates
    const u_s  = 0.1/N
    const u_p  = 0.1/N
    const u_a  = 0.1/N
    # Game parameters
    const game_pars = [b, c, w, u_s, u_p, u_a]
end

# General conditions
N
game = Game(game_pars...)
pop = random_population(N,game)

# Test random_population
# Default population:
#   - Three strategies
#   - Two groups of sizes ~ [0.5,0.5]
#   - Public individual and public group reputations
#   - Randomly use individual or group (fixed value of p=0.5)
#   - Update reputations every round (fixed value of λ=1.0 )
#   - Individual and group reputations based on behavior
#   - Both individual and group reps using individual previous reps as source
pop = random_population(N,game)

# Changing variables
# Example:
#   - Private individual and group reputations
#   - Three groups with different sizes
#   - Individuals that use only individual, only group and randomly both reps
#   - Half of individuals update every round and other half do not update
begin
    pop = random_population(
        N,
        game,
        ind_reps_public = true,
        grp_reps_public = true,
        group_sizes = [0.25, 0.25, 0.5],
        prob_values = [0.0, 0.5, 1.0],
        prob_weights = [0.5, 0.25, 0.25],
        rate_values = [0.0, 1.0],
        rate_weights = [0.5, 0.5]
        )

    dict = proportionmap(pop.probs)
    "Sampled values: " |> print
    dict |> keys |> collect |> println

    "Sampled proportion: " |> print
    dict |> values |> collect |> println
end
