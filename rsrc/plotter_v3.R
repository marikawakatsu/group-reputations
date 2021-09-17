################################################################################
#
# Script for basic plots
#
################################################################################

rm(list = ls())
source("rsrc/functions.R")

# path to the main directory
setwd("~/Documents/GitHub/group-reputations/")

########################
# LOAD SIMULATION DATA 
########################
# path to the data directory
data_dir     <- "~/Dropbox (Princeton)/Stereotypes_results/group-reputations/"
data_sub_dir <- "scale-prob-rate-cost"

# load and merge data
simdata_m <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data.csv"), header = TRUE)
simdata_s <- read.csv( paste0(data_dir, data_sub_dir, "/", "s_data.csv"), header = TRUE)
simdata   <- rbind(simdata_m, simdata_s)
# simdata   <- simdata_m

casecount <- simdata %>% group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp) %>% summarize(COUNT = n())

##########################
# PREP DATA FOR PLOTTING
##########################
# select columns
id_vars         <- c("N","norm","all_strategies","num_groups","group_sizes","generation",
                     "ind_scale","grp_scale","ind_base","grp_base","ind_src_ind","grp_src_grp","prob","rate","cost")
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 
measure_rep_ind <- c("reps_ind_11","reps_ind_12","reps_ind_21","reps_ind_22") 
measure_rep_grp <- c("reps_grp_11","reps_grp_12","reps_grp_21","reps_grp_22") 
measure_freq    <- c("freq1_ALLC","freq1_ALLD","freq1_DISC","freq2_ALLC","freq2_ALLD","freq2_DISC")
measure_fitness <- c("fitness1_ALLC","fitness1_ALLD","fitness1_DISC","fitness2_ALLC","fitness2_ALLD","fitness2_DISC")

# select columns
simdata_bymeasure <- simdata %>% 
  gather("variable","value",-N,-norm,-all_strategies,-num_groups,-group_sizes,-generation,
         -ind_scale,-grp_scale,-ind_base,-grp_base,-ind_src_ind,-grp_src_grp,-prob,-rate,-cost)
simdata_bymeasure$value = as.numeric(simdata_bymeasure$value)

# compute means and vars
compute_mean_vars <- function( simdata_bymeasure, variables ){
  simdata_mean <- simdata_bymeasure[which(simdata_bymeasure$variable %in% variables),] %>%
    rename( Fraction = value, Metric = variable ) %>%
    group_by( N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp, prob, rate, cost, Metric ) %>%
    summarise(
      Mean = mean(Fraction),
      SD = sd(Fraction),
      SE = sd(Fraction) / sqrt(length(Fraction)),
      numCases = length(Fraction)
    )
  # add interpretable labels
  simdata_mean$ind_base_label[simdata_mean$ind_base == 0] <- "Individual reps random"
  simdata_mean$ind_base_label[simdata_mean$ind_base == 1] <- "Individual reps based on behavior"
  simdata_mean$grp_base_label[simdata_mean$grp_base == 0] <- "Group reps random"
  simdata_mean$grp_base_label[simdata_mean$grp_base == 1] <- "Group reps based on behavior"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 0] <- "Individual reps public"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 1] <- "Individual reps by group"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 2] <- "Individual reps private"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 0] <- "Group reps public"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 1] <- "Group reps by group"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 2] <- "Group reps private"
  
  # only for reputations
  # simdata_mean$acting_group[simdata_mean$Metric %in% c("reps_ind_11", "reps_ind_12", "reps_grp_11", "reps_grp_12")] <- "Group 1"
  # simdata_mean$acting_group[simdata_mean$Metric %in% c("reps_ind_21", "reps_ind_22", "reps_grp_21", "reps_grp_22")] <- "Group 2"
  
  simdata_mean <- simdata_mean %>% mutate( rate_label = paste0( "λ = ", rate ))
  simdata_mean <- simdata_mean %>% mutate( cost_label = paste0( "α = ", rate ))
  simdata_mean$rate <- round(simdata_mean$rate, 2)
  
  return(simdata_mean)
}

simdata_coop    <- compute_mean_vars(simdata_bymeasure, measure_coop)
simdata_rep_ind <- compute_mean_vars(simdata_bymeasure, measure_rep_ind)
simdata_rep_grp <- compute_mean_vars(simdata_bymeasure, measure_rep_grp)
simdata_freq    <- compute_mean_vars(simdata_bymeasure, measure_freq)
simdata_fitness <- compute_mean_vars(simdata_bymeasure, measure_fitness)

################
# PLOT HEATMAP
################
plot_heatmap_fixed_cost <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                    label = "Average\ncooperation", cost = 0.0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$cost == paste0("",cost,""), ]
  
  # check that the subsetting is done correctly
  if(verbose){
    print( paste0("costs: ", unique(subdata$cost)))
    print( paste0("rates: ", unique(subdata$rate)))
    print( paste0("probs: ", unique(subdata$prob)))
  }
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = rate, fill = Mean)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           # legend.key.size = unit(0.04, "npc"),
           # panel.spacing = unit(0.25,"lines"),
           # legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm ", norm, " (cost α = ", cost, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_fill_gradient2(low = "black", mid = "#CFD5D9", high = "white",
                         # low = "#BD1513", mid = "#CFD5D9", high = "#00428B",
                         # low = "white", mid = "#CFD5D9", high = "#00428B",
                         midpoint = 0.5,
                         limit    = c(0., 1.),
                         space    = "Lab",
                         name     = label) +
    labs(x = "Probability of DISC using group reputations (p)",
         y = "Probability of group reputation update per round (λ)") +
    geom_tile( show.legend = TRUE ) + 
    facet_grid(ind_scale_label ~ grp_scale_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside") 
  
  return(fig)
}

plot_heatmap_fixed_rate <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                    label = "Average\ncooperation", rate = 0.0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$rate == paste0("",rate,""), ]
  if(max(subdata$cost) < 0.11){
    ybreaks = c(0, 0.1, 0.02)
    ylimits = c(-0.01, 0.11)
  }else if(max(subdata$cost) < 1.1){
    ybreaks = c(0, 1.0, 0.2)
    ylimits = c(-0.1, 1.1)
  }
  
  # check that the subsetting is done correctly
  if(verbose){
    print( paste0("costs: ", unique(subdata$cost) ))
    print( paste0("rates: ", unique(subdata$rate) ))
    print( paste0("probs: ", unique(subdata$prob) ))
  }
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = cost, fill = Mean)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           # legend.key.size = unit(0.04, "npc"),
           # panel.spacing = unit(0.25,"lines"),
           # legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm ", norm, " (rate λ = ", rate, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = ybreaks, limits = ylimits) +
    scale_fill_gradient2(low = "black", mid = "#CFD5D9", high = "white",
                         # low = "#BD1513", mid = "#CFD5D9", high = "#00428B",
                         # low = "white", mid = "#CFD5D9", high = "#00428B",
                         midpoint = 0.5,
                         limit    = c(0., 1.),
                         space    = "Lab",
                         name     = label) +
    labs(x = "Probability of DISC using group reputations (p)",
         y = "Cost of using individual reputations (α)") +
    geom_tile( show.legend = TRUE ) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside")
  
  return(fig)
}

################
# PLOT LINES
################
plot_line <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], 
                      label = "Average\ncooperation", cost = 0.0, grp_scale = 0){
  
  # choose colors
  if( "coop_11" %in% metric ){
    colors    = c("#3B528BFF", "#3B528BFF", "#5DC863FF", "#5DC863FF") # viridis(5)[1:4]
    linetypes = c("solid", "dotted", "dotted", "solid")
  }else if( "reps_ind_11" %in% metric || "reps_grp_11" %in% metric ){
    colors    = c("#810f7c", "#810f7c", "#8c96c6", "#8c96c6") # magma(6)[2:5]
    linetypes = c("solid", "dotted", "dotted", "solid")
  }else if( "freq1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
  }else if( "fitness1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
  }
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_scale == grp_scale, ]
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           # legend.key.size = unit(0.04, "npc"),
           # panel.spacing = unit(0.25,"lines"),
           # legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm ", norm ) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = seq(0, 1, 0.2),
                        limits = c(0, 1)) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using group reputations (p)",
         y = "Frequency") +
    facet_grid(ind_scale_label ~ rate_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    ) 
  
  return(fig)
}

plot_line_fixrate <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], 
                              label = "Average\ncooperation", rate = 1.0, grp_scale = 0){
  
  # choose colors
  if( "coop_11" %in% metric ){
    colors    = c("#3B528BFF", "#3B528BFF", "#5DC863FF", "#5DC863FF") # viridis(5)[1:4]
    linetypes = c("solid", "dotted", "dotted", "solid")
  }else if( "reps_ind_11" %in% metric || "reps_grp_11" %in% metric ){
    colors    = c("#810f7c", "#810f7c", "#8c96c6", "#8c96c6") # magma(6)[2:5]
    linetypes = c("solid", "dotted", "dotted", "solid")
  }else if( "freq1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
  }else if( "fitness1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
  }
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$cost == paste0("",rate,"") &
                           simdata_sub$grp_scale == grp_scale, ]
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           # legend.key.size = unit(0.04, "npc"),
           # panel.spacing = unit(0.25,"lines"),
           # legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm ", norm ) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = seq(-0.8, 0.8, 0.2),
                        limits = c(-0.6, 0.6)) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using group reputations (p)",
         y = "Fitness") +
    facet_grid(ind_scale_label ~ cost_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    ) 
  
  return(fig)
}

##############
# SAVE PLOTS
##############
# printing function
print_figure <- function(){
  dev.off()
  Sys.sleep(1)
  print("done")
}

# test plots
norms       <- unique(simdata$norm)
costs       <- unique(simdata$cost)
rates       <- unique(simdata$rate)

for(norm in norms){
  
  # cooperation, all
  simdata_sub <- simdata_coop
  metric      <- "cooperation"
  label       <- "Average\ncooperation"
  width       <- 6
  height      <- 5.5
  
  for(cost in costs){
    png(filename = paste0("plots/", "cooperation", "_heatmap_", norm, "_cost_", cost, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
        width = width, height = height, units = "in", res = 600)
    print(plot_heatmap_fixed_cost(simdata_sub, norm, metric, label, cost, TRUE))
    print_figure()
  }
  
  # for(rate in rates){
  #   png(filename = paste0("plots/", "cooperation", "_heatmap_", norm, "_rate_", rate, "_costmax_", max(simdata$cost), "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_heatmap_fixed_rate(simdata_sub, norm, metric, label, rate, TRUE))
  #   print_figure()
  # }

  # for(grp_scale in c(0,1,2)){
  # 
  #   # cooperation, by group
  #   simdata_sub <- simdata_coop
  #   metric      <- measure_coop[2:5]
  #   label       <- "Average\ncooperation"
  #   width       <- 9
  #   height      <- 5
  #   cost        <- 0.02 # 0.2
  # 
  #   png(filename = paste0("plots/", "cooperation_bygroup", "_line_", norm, "_cost_", cost, "_grp_", grp_scale, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_line(simdata_sub, norm, metric, label, cost))
  #   print_figure()
  # 
  #   # strat frequencies
  #   simdata_sub <- simdata_freq
  #   metric      <- measure_freq
  #   label       <- "Average\nfrequency"
  # 
  #   png(filename = paste0("plots/","freq", "_line_", norm, "_cost_", cost, "_grp_", grp_scale, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_line(simdata_sub, norm, metric, label, cost))
  #   print_figure()
  # 
  #   # indiv reputations
  #   simdata_sub <- simdata_rep_ind
  #   metric      <- measure_rep_ind
  #   label       <- "Fraction good"
  # 
  #   png(filename = paste0("plots/","rep_ind", "_line_", norm, "_cost_", cost, "_grp_", grp_scale, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_line(simdata_sub, norm, metric, label, cost))
  #   print_figure()
  # 
  #   # group reputations
  #   simdata_sub <- simdata_rep_grp
  #   metric      <- measure_rep_grp
  #   label       <- "Fraction good"
  # 
  #   png(filename = paste0("plots/","rep_grp", "_line_", norm, "_cost_", cost, "_grp_", grp_scale, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_line(simdata_sub, norm, metric, label, cost))
  #   print_figure()
  # 
  #   # fitnesses
  #   simdata_sub <- simdata_fitness
  #   metric      <- measure_fitness
  #   label       <- "Average fitness"
  # 
  #   png(filename = paste0("plots/","fitness", "_line_", norm, "_cost_", cost, "_grp_", grp_scale, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
  #       width = width, height = height, units = "in", res = 600)
  #   print(plot_line_fixrate(simdata_sub, norm, metric, label, cost))
  #   print_figure()
  # }
  
}
