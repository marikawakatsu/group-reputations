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
data_sub_dir <- "sweep_src-prob-rate"

# load and merge data
simdata_m <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data.csv"), header = TRUE)
simdata_s <- read.csv( paste0(data_dir, data_sub_dir, "/", "s_data.csv"), header = TRUE)
simdata   <- rbind(simdata_m, simdata_s)
# simdata_ss <- read.csv( paste0(data_dir,"sweep_base-src-prob", "/", "data.csv"), header = TRUE)

casecount <- simdata %>% group_by(N, norm, ind_base, grp_base, ind_src_ind, grp_src_grp) %>% summarize(COUNT = n())

##########################
# PREP DATA FOR PLOTTING
##########################
# select columns
id_vars         <- c("N","norm","all_strategies","num_groups","group_sizes","generation",
                     "ind_public","grp_public","ind_base","grp_base","ind_src_ind","grp_src_grp","prob","rate")
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 
measure_rep_ind <- c("reps_ind_11","reps_ind_12","reps_ind_21","reps_ind_22") 
measure_rep_grp <- c("reps_grp_11","reps_grp_12","reps_grp_21","reps_grp_22") 
measure_freq    <- c("freq1_ALLC","freq1_ALLD","freq1_DISC","freq2_ALLC","freq2_ALLD","freq2_DISC")
measure_fitness <- c("fitness1_ALLC","fitness1_ALLD","fitness1_DISC","fitness2_ALLC","fitness2_ALLD","fitness2_DISC")

# simdata$ind_base_label[simdata$ind_base == 0] <- "Individual reps random"
# simdata$ind_base_label[simdata$ind_base == 1] <- "Individual reps based on behavior"
# simdata$grp_base_label[simdata$grp_base == 0] <- "Group reps random"
# simdata$grp_base_label[simdata$grp_base == 1] <- "Group reps based on behavior"

# select columns
simdata_bymeasure <- simdata %>% 
  gather("variable","value",-N,-norm,-all_strategies,-num_groups,-group_sizes,-generation,
         -ind_public,-grp_public,-ind_base,-grp_base,-ind_src_ind,-grp_src_grp,-prob,-rate)
simdata_bymeasure$value = as.numeric(simdata_bymeasure$value)

# compute means and vars
compute_mean_vars <- function( simdata_bymeasure, variables ){
  simdata_mean <- simdata_bymeasure[which(simdata_bymeasure$variable %in% variables),] %>%
    rename( Fraction = value, Metric = variable ) %>%
    group_by( N, norm, ind_base, grp_base, ind_src_ind, grp_src_grp, prob, rate, Metric ) %>%
    summarise(
      Mean = mean(Fraction),
      SD = sd(Fraction),
      SE = sd(Fraction) / sqrt(length(Fraction)),
      numCases = length(Fraction)
    )
  
  simdata_mean$ind_base_label[simdata_mean$ind_base == 0] <- "Individual reps random"
  simdata_mean$ind_base_label[simdata_mean$ind_base == 1] <- "Individual reps based on behavior"
  simdata_mean$grp_base_label[simdata_mean$grp_base == 0] <- "Group reps random"
  simdata_mean$grp_base_label[simdata_mean$grp_base == 1] <- "Group reps based on behavior"
  simdata_mean <- simdata_mean %>% mutate( rate_label = paste0( "λ =", rate ))
  
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
plot_heatmap <- function(simdata_sub, norm = "SJ", metric = "cooperation", label = "Average\ncooperation"){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                         simdata_sub$norm == norm, ]
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = rate, fill = Mean)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           legend.key.size = unit(0.04, "npc"),
           panel.spacing = unit(0.25,"lines"),
           legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm: ", norm) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = seq(0, 1, 0.2)) +
    scale_fill_gradient2(low = "#BD1513", mid = "#CFD5D9", high = "#00428B",
                         midpoint = 0.5,
                         limit    = c(0., 1.),
                         space    = "Lab",
                         name     = label) +
    labs(x = "Probability of DISC using individual reputations (p)",
         y = "Rate of group reputation update (λ)") +
    geom_tile( show.legend = TRUE ) + 
    facet_grid(grp_base_label ~ ind_base_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          ) 
  
  return(fig)
}

################
# PLOT LINES
################
plot_line <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], label = "Average\ncooperation"){
  
  # choose colors
  if( "coop_11" %in% metric ){
    colors = viridis(5)[1:4]
  }else if( "reps_ind_11" %in% metric || "reps_grp_11" %in% metric ){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00") # !!! CHANGE !!!
  }else if( "freq1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
  }
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$rate %in% c(0,1), ]
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, label = Metric)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           legend.key.size = unit(0.04, "npc"),
           panel.spacing = unit(0.25,"lines"),
           legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm: ", norm) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = seq(0, 1, 0.2),
                        limits = c(0, 1)) +
    scale_color_manual(values = colors, # c("#2b8cbe","#2b8cbe","#a8ddb5","#a8ddb5"),
                       name   = "Average\ncooperation") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1, lty = 1) +
    geom_point(stat="identity", size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using individual reputations (p)",
         y = "Frequency") +
    facet_grid(grp_base_label ~ rate_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
          ) 
  
  return(fig)
}

plot_line_strat <- function(simdata_sub, norm = "SJ", metric = measure_freq, label = "Average\nreputation"){
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$rate %in% c(0,1), ]
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, label = Metric)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 10, 
                                    margin = margin(0,0,0,0) ) ) +
    theme (legend.text = element_text (size = 7),
           legend.title = element_text(size = 8),
           legend.key.size = unit(0.04, "npc"),
           panel.spacing = unit(0.25,"lines"),
           legend.margin = margin(t = 0, unit="npc")
    ) +
    ggtitle( paste0("norm: ", norm) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = seq(0, 1, 0.2),
                        limits = c(0, 1)) +
    scale_color_manual(values = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00"),
                       name   = "Average\ncooperation") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1, lty = 1) +
    geom_point(stat="identity", size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using individual reputations (p)",
         y = "Frequency") +
    facet_grid(grp_base_label ~ rate_label, 
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
# test plots
norms       <- unique(simdata$norm)

for(norm in norms){
  
  # cooperation, all
  simdata_sub <- simdata_coop
  metric      <- "cooperation"
  label       <- "Average\ncooperation"
  
  png(filename = paste0("plots/", "cooperation", "_heatmap_", norm, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
      width = 6, height = 5, units = "in", res = 600)
  print(plot_heatmap(simdata_sub, norm, metric, label))
  dev.off()
  Sys.sleep(1)
  print("done")
  
  # cooperation, by group
  simdata_sub <- simdata_coop
  metric      <- measure_coop[2:5]
  label       <- "Average\ncooperation"

  png(filename = paste0("plots/", metric, "_line_", norm, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
      width = 6, height = 5, units = "in", res = 600)
  print(plot_line(simdata_sub, norm, metric, label))
  dev.off()
  Sys.sleep(1)
  
  print("done")
  # strat frequencies
  simdata_sub <- simdata_freq
  metric      <- measure_freq
  label       <- "Average\nfrequency"
  
  png(filename = paste0("plots/","freq", "_line_", norm, "_", format(Sys.Date(), format="%y%m%d"), ".png"),
      width = 6, height = 5, units = "in", res = 600)
  print(plot_line(simdata_sub, norm, metric, label))
  dev.off()
  Sys.sleep(1)
  print("done")
  
}
