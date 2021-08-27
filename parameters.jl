using DataFrames, CSV
using DelimitedFiles
!isdir("parameters/") && mkdir("parameters/")
# Game parameters
begin
    N    = 50
    b    = 1.0
    c    = 0.2
    u_s  = 1/N
    u_p  = 1/N
    u_a  = 1/N

    w =  1.0
    game_pars = [b,c,w,u_s,u_p,u_a]
    writedlm("parameters/strong_selection.csv",game_pars,",")

    w =  0.1
    game_pars = [b,c,w,u_s,u_p,u_a]
    writedlm("parameters/medium_selection.csv",game_pars,",")

    w =  0.01
    game_pars = [b,c,w,u_s,u_p,u_a]
    writedlm("parameters/weak_selection.csv",game_pars,",")
end
# Exploration parameters
begin
    sizes = [1,5,10,15,20,25]
    norms = ["SJ", "SS", "SC", "SH"]
    stype1 = [true,false]
    stype2 = [true,false]
    q1 = 0.5
    q2 = 0.5
    obs = 1
    o_i = [.0,.01,.25,.5,.75,1.0]
    o_s = [.0,.01,.25,.5,.75,1.0]
    colnames = [:N, :norm, :N1, :stype1, :stype2, :q1, :q2, :obs, :o_i, :o_s]
    pars = hcat([[row...] for row in Base.Iterators.product(N,norms,sizes,stype1,stype2,q1,q2,obs,o_i,o_s)]...)
    parameters = DataFrame(permutedims(pars), colnames)
    CSV.write("parameters/pars.csv",parameters)
end
# group size sweep
begin
    Ns = [20,30,40,50,60,70,80,90,100]
    sizes = [10,20,30,40,50]
    norms = ["SJ", "SS", "SC", "SH"]
    stype1 = [true,false]
    stype2 = [true,false]
    q1 = 0.5
    q2 = 0.5
    obs = 1
    o_i = 1.0 # [.0,.01,.25,.5,.75,1.0]
    o_s = 1.0 # [.0,.01,.25,.5,.75,1.0]
    colnames = [:N, :norm, :N1, :stype1, :stype2, :q1, :q2, :obs, :o_i, :o_s]
    pars = hcat([[row...] for row in Base.Iterators.product(Ns,norms,sizes,stype1,stype2,q1,q2,obs,o_i,o_s)]...)
    parameters = DataFrame(permutedims(pars), colnames)
    parameters = parameters[ (parameters.N .> parameters.N1).&(parameters.N .- parameters.N1 .<= 50), : ]# select rows where N is greater than N1
    CSV.write("parameters/pars_v2.csv",parameters)
end
# in-out stereotyping and multiple norms parameters
begin
    Ns = [50]#[20,30,40,50,60,70,80,90,100]
    sizes = [25]#[10,20,30,40,50]
    norms1 = ["SJ", "SS", "SC", "SH"]
    norms2 = ["SJ", "SS", "SC", "SH"]
    stype11 = [true,false]
    stype12 = [true,false]
    stype21 = [true,false]
    stype22 = [true,false]
    q1 = 0.5
    q2 = 0.5
    obs = 1
    o_i = 1.0 # [.0,.01,.25,.5,.75,1.0]
    o_s = 1.0 # [.0,.01,.25,.5,.75,1.0]
    colnames = [:N, :N1, :stype11, :stype12, :stype21, :stype22, :norm1, :norm2, :q1, :q2, :obs, :o_i, :o_s]
    valid_norms = []
    for n1 in norms1, n2 in norms2
        push!( valid_norms, Set([n1,n2]) )
    end
    valid_norms = Set(valid_norms)

    _rows = []
    i = 0
    for N in Ns
        for size in sizes
            _norms = copy(valid_norms)
            for n1 in norms1, n2 in norms2
                rows = []
                if Set([n1,n2]) in _norms
                    filter!(i->i!=Set([n1,n2]),_norms)
                    for s11 in stype11, s12 in stype12, s21 in stype21, s22 in stype22
                        push!(rows,[N,size,s11,s12,s21,s22,n1,n2,q1,q2,obs,o_i,o_s])
                    end
                    push!(_rows,rows)
                    pars = hcat(rows...)
                    parameters = DataFrame(permutedims(pars), colnames)
                    CSV.write("parameters/pars_v3_$i.csv",parameters)
                    i+=1
                end
            end
        end
    end
    # pars = hcat([[row...] for row in Base.Iterators.product(Ns,sizes,stype11,stype12,stype21,stype22,norms1,norms2,q1,q2,obs,o_i,o_s)]...)
    # pars = hcat(rows...)
    # parameters = DataFrame(permutedims(pars), colnames)
    # parameters = parameters[ (parameters.N .> parameters.N1).&
    #                         (parameters.N .- parameters.N1 .<= 50).&
    #                         (parameters.N .== 50), : ]# select rows where N is greater than N1
    # CSV.write("parameters/pars_v3.csv",parameters)
end

# _rows
# hcat(_rows[1]...)
# DataFrame(permutedims(hcat(_rows[1]...)), colnames)
# _rows