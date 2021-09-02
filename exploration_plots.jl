using Statistics, StatsBase
using CSV, DataFrames
using StatsPlots, Plots

simulation_title = "sweep_src-prob-rate"
DF = CSV.File("data/$simulation_title/s_data.csv") |> DataFrame

bases = ["random","behavior"]

for ib in [0,1], gb in [0,1]
    df = DF[ (DF[!,:ind_base] .== ib) .& (DF[!,:grp_base] .== gb), :]
    _types = eltype.(eachcol(df))
    _summarize = names(df)[findall(_types .!= String )]
    _sweep = [:ind_public, :grp_public, :ind_base, :grp_base, :ind_src_ind, :grp_src_grp, :prob, :rate]
    df = groupby(df, [:norm, _sweep...])
    df = combine(df, _summarize .=> mean, _summarize .=> std, _summarize .=> sem)
    title = "Individials - $(bases[ib+1])        Groups - $(bases[gb+1])\n"

    cols = [:blue :red :green]
    for v in groupby(df[df.norm .== "SJ",:],:rate)
        ps_r = @df v plot(
            :prob,
            [:freq1_ALLC_mean, :freq1_ALLD_mean, :freq1_DISC_mean,
            :freq2_ALLC_mean, :freq2_ALLD_mean, :freq2_DISC_mean],
            yerror=[:freq1_ALLC_std :freq1_ALLD_std :freq1_DISC_std :freq2_ALLC_std :freq2_ALLD_std :freq2_DISC_std],
            ylims  = (0-0.01,1+0.01),
            framestyle = :box,
            color  = [cols cols],
            lc     = [cols cols],
            xlabel = "p",
            ylabel = "Strategy frequency",
            title  = title*"λ = $(round(mean(:rate);digits=1))",
            label  = ["AllC-1" "AllD-1" "DISC-1" "AllC-2" "AllD-2" "DISC-2"],
            linestyle   = [:solid :solid :solid :dot :dot :dot],
            markerwidth = 1.,
            markersize  = 5.,
            markershape = [:circle :circle :circle :circle :circle :circle],
            markerstrokecolor = [cols cols],
            )
        ps_r |> display
    end

    for v in groupby(df[df.norm .== "SJ",:],:prob)
        ps_p = @df v plot(
            :rate,
            [:freq1_ALLC_mean, :freq1_ALLD_mean, :freq1_DISC_mean,
            :freq2_ALLC_mean, :freq2_ALLD_mean, :freq2_DISC_mean],
            yerror=[:freq1_ALLC_std :freq1_ALLD_std :freq1_DISC_std :freq2_ALLC_std :freq2_ALLD_std :freq2_DISC_std],
            ylims  = (0-0.01,1+0.01),
            framestyle = :box,
            color  = [cols cols],
            lc     = [cols cols],
            xlabel = "λ",
            ylabel = "Strategy frequency",
            title  = title*"p = $(round(mean(:prob);digits=1))",
            label  = ["AllC-1" "AllD-1" "DISC-1" "AllC-2" "AllD-2" "DISC-2"],
            linestyle   = [:solid :solid :solid :dot :dot :dot],
            markerwidth = 1.,
            markersize  = 5.,
            markershape = [:circle :circle :circle :circle :circle :circle],
            markerstrokecolor = [cols cols],
            )
        ps_p |> display
    end

    cols = [colorant"#2b8cbe" colorant"#2b8cbe" colorant"#a8ddb5" colorant"#a8ddb5"]

    for v in groupby(df[df.norm .== "SJ",:],:rate)
        pc_r = @df v plot(
            :prob,
            [:coop_11_mean, :coop_21_mean, :coop_22_mean, :coop_12_mean, ],
            yerror = [:coop_11_std :coop_21_std :coop_22_std :coop_12_std ],
            ylims  = (0-0.01,1+0.01),
            framestyle = :box,
            color  = cols,
            lc     = cols,
            xlabel = "p",
            title  = title*"λ = $(round(mean(:rate);digits=1))",
            ylabel = "Cooperation level",
            label  = ["1->1" "2->1" "1->2" "2->2"],
            linestyle   = [:solid :dot :solid :dot],
            markerwidth = 1.,
            markersize  = 5,
            markershape = :circle,
            markerstrokecolor = cols,
            )
        pc_r |> display
    end

    for v in groupby(df[df.norm .== "SJ",:],:prob)
        pc_p = @df v plot(
            :rate,
            [:coop_11_mean, :coop_21_mean, :coop_22_mean, :coop_12_mean, ],
            yerror = [:coop_11_std :coop_21_std :coop_22_std :coop_12_std ],
            ylims  = (0-0.01,1+0.01),
            framestyle = :box,
            color  = cols,
            lc     = cols,
            xlabel = "λ",
            title  = title*"p = $(round(mean(:prob);digits=1))",
            ylabel = "Cooperation level",
            label  = ["1->1" "2->1" "1->2" "2->2"],
            linestyle   = [:solid :dot :solid :dot],
            markerwidth = 1.,
            markersize  = 5,
            markershape = :circle,
            markerstrokecolor = cols,
            )
        pc_p |> display
    end

    #plot(ps_r, ps_p, pc_r, pc_p, layout=(2,2)) |> display
end
