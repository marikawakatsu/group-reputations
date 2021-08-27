using Pkg
isinstalled(pkg::String) = any(x -> x.name == pkg && x.is_direct_dep, values(Pkg.dependencies()))
pkgs_names = ["Random","StatsBase","JLD","Distributed"]
for p in pkgs_names
    !isinstalled(p) && Pkg.add(p)
end