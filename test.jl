# Script to test run the module
begin
    # Load module
    include("GroupReputations.jl")
    using .GroupReputations
end

# Set parameters
begin
    # Population size
    const N    = 50
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

# Generate a random population
game = Game(game_pars...)
pop  = random_population(N, game)