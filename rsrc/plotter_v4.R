################################################################################
#
# Script for basic plots
#
################################################################################

rm(list = ls())
# path to the main directory
setwd("~/Documents/GitHub/group-reputations/")
source("rsrc/utils/__utils__.R")

########################
# LOAD SIMULATION DATA 
########################
# path to the data directory
data_dir     <- "~/Dropbox (Princeton)/Stereotypes_results/group-reputations/"

# load and merge data
data_sub_dir <- "DISC-scale-prob-cost"
tag          <- "_DISC"
simdata_m <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data", tag, ".csv"), header = TRUE)
simdata_s <- read.csv( paste0(data_dir, data_sub_dir, "/", "s_data", tag, ".csv"), header = TRUE)
simdata   <- rbind(simdata_m, simdata_s)

# manual option 1 — plot variation along rates - ADD to above
# data_sub_dir <- "DISC-scale-prob-rate"
# tag          <- "_DISC_rates"
# simdata_m    <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data", tag, ".csv"), header = TRUE)
# simdata      <- rbind(simdata, simdata_m)
# simdata      <- simdata[simdata$cost == 0 ,]

# manual option 2 - REPLACE options above
# data_sub_dir <- "scale-prob-rate-cost"
# tag          <- "_lowalpha"
# simdata_m    <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data", tag, ".csv"), header = TRUE)
# simdata_s    <- read.csv( paste0(data_dir, data_sub_dir, "/", "s_data", tag, ".csv"), header = TRUE)
# simdata      <- rbind(simdata_m, simdata_s)

casecount <- casecounter(simdata)
# simdata   <- simdata %>%
#   group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp) %>%
#   slice_head( n = min(casecount$COUNT) )

##########################
# PREP DATA FOR PLOTTING
##########################
# see rsrc/utils/processing_functions.R for measuring variables
# select columns
id_vars         <- c("N","norm","all_strategies","num_groups","group_sizes","generation",
                     "ind_scale","grp_scale","ind_base","grp_base","ind_src_ind","grp_src_grp","prob","rate","cost")
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
  simdata_mean$grp_base_label[simdata_mean$grp_base == 0] <- "Stereotypes random"
  simdata_mean$grp_base_label[simdata_mean$grp_base == 1] <- "Stereotypes based on behavior"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 0] <- "Individual reps public"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 1] <- "Individual reps by group"
  simdata_mean$ind_scale_label[simdata_mean$ind_scale == 2] <- "Individual reps private"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 0] <- "Stereotypes public"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 1] <- "Stereotypes by group"
  simdata_mean$grp_scale_label[simdata_mean$grp_scale == 2] <- "Stereotypes private"
  simdata_mean$ind_src_label[simdata_mean$ind_src_ind == 0] <- "Ind reps based on ind reps"
  simdata_mean$ind_src_label[simdata_mean$ind_src_ind == 1] <- "Ind reps based on stereotypes"
  simdata_mean$grp_src_label[simdata_mean$grp_src_grp == 0] <- "Stereotypes based on ind reps"
  simdata_mean$grp_src_label[simdata_mean$grp_src_grp == 1] <- "Stereotypes based on stereotypes"
  simdata_mean$grp_scale_label <- factor(simdata_mean$grp_scale_label, 
                                         levels = c("Stereotypes private", "Stereotypes by group", "Stereotypes public"))
  simdata_mean$ind_scale_label <- factor(simdata_mean$ind_scale_label, 
                                         levels = c("Individual reps public", "Individual reps by group", "Individual reps private"))
  
  # only for reputations
  # simdata_mean$acting_group[simdata_mean$Metric %in% c("reps_ind_11", "reps_ind_12", "reps_grp_11", "reps_grp_12")] <- "Group 1"
  # simdata_mean$acting_group[simdata_mean$Metric %in% c("reps_ind_21", "reps_ind_22", "reps_grp_21", "reps_grp_22")] <- "Group 2"
  
  simdata_mean <- simdata_mean %>% mutate( rate_label = paste0( "λ = ", rate ))
  simdata_mean <- simdata_mean %>% mutate( cost_label = paste0( "α = ", cost ))
  simdata_mean$rate <- round(simdata_mean$rate, 2)
  
  return(simdata_mean)
}

simdata_coop    <- compute_mean_vars(simdata_bymeasure, measure_coop)
simdata_rep_ind <- compute_mean_vars(simdata_bymeasure, measure_rep_ind)
simdata_rep_grp <- compute_mean_vars(simdata_bymeasure, measure_rep_grp)
simdata_freq    <- compute_mean_vars(simdata_bymeasure, measure_freq)
simdata_fitness <- compute_mean_vars(simdata_bymeasure, measure_fitness)

##############
# SAVE PLOTS
##############
# test plots
norms       <- c("SJ") # unique(simdata$norm)
costs       <- unique(simdata$cost)
rates       <- unique(simdata$rate)
biases      <- unique(simdata$bias)
srcs        <- unique(simdata$grp_src_grp)
probs       <- unique(simdata$prob)

if(length(biases) > 1){stop("more than one bias values, check plots")}

for(norm in norms){
  
  for(grp_src_grp in srcs){
    
    # cooperation, all
    simdata_sub <- simdata_coop
    metric      <- "cooperation"
    label       <- "Average\ncooperation"
    width       <- 6
    height      <- 5.5
    
    if( length(rates) > 2 ){
      for(cost in costs){
        filename <- paste0("plots/", "cooperation", "_heatmap_", norm, "_cost_", cost, "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                           "_", format(Sys.Date(), format="%y%m%d"), ".png")
        png(filename = filename,
            width = width, height = height, units = "in", res = 600)
        print(plot_heatmap_fixed_cost(simdata_sub, norm, metric, label, cost, grp_src_grp, FALSE))
        print_figure(filename)
      }
      rm(cost)
      
      for(prob in probs){
        filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_cost_", min(costs), "_prob_", prob, "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                           "_", format(Sys.Date(), format="%y%m%d"), ".png")
        png(filename = filename,
            width = width, height = height, units = "in", res = 600)
        print(plot_linegrid_fixed_cost_prob_bias(simdata_sub, norm, measure_coop[2:5], label, min(costs), prob, grp_src_grp, TRUE))
        print_figure(filename)
        
        if(tag == "_DISC_rates"){
          filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_cost_", min(costs), "_prob_", prob, "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                             "_", format(Sys.Date(), format="%y%m%d"), "_select.png")
          png(filename = filename, width = width/1.75, height = height/2.25, units = "in", res = 600)
          print(plot_linegrid_fixed_cost_prob_bias_select(simdata_sub, norm, measure_coop[2:5], label, min(costs), prob, grp_src_grp, TRUE))
          print_figure(filename)
        }
      }
    }
    if( length(costs) > 2 ){
      for(rate in rates){
        
        filename <- paste0("plots/", "cooperation", "_heatmap_", norm, "_rate_", rate, "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                           "_", format(Sys.Date(), format="%y%m%d"), ".png")
        png(filename = filename,
            width = width, height = height, units = "in", res = 600)
        print(plot_heatmap_fixed_rate(simdata_sub, norm, metric, label, rate, grp_src_grp, FALSE))
        print_figure(filename)
        
        # NEW
        # metric <- measure_coop[2:5]
        # cost   <- min(costs)
        
        filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                           "_", format(Sys.Date(), format="%y%m%d"), ".png")
        png(filename = filename,
            width = width, height = height, units = "in", res = 600)
        print(plot_linegrid_fixed_rate_cost(simdata_sub, norm, measure_coop[2:5], label, rate, min(costs), grp_src_grp, TRUE))
        print_figure(filename)
        
        # selectd panels
        if(tag == "_DISC"){
          filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                             "_", format(Sys.Date(), format="%y%m%d"), "_select.png")
          png(filename = filename, width = width, height = height/2.5, units = "in", res = 600)
          print(plot_linegrid_fixed_rate_cost_select(simdata_sub, norm, measure_coop[2:5], label, rate, min(costs), grp_src_grp, TRUE))
          print_figure(filename)
          
          
          filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, "_grpsrcgrp_", grp_src_grp, tag,
                             "_", format(Sys.Date(), format="%y%m%d"), "_select2.png")
          png(filename = filename, width = width/1.75, height = height/2.25, units = "in", res = 600)
          print(plot_linegrid_fixed_rate_cost_select2(simdata_sub, norm, measure_coop[2:5], label, rate, min(costs), grp_src_grp, TRUE))
          print_figure(filename)
        }
      }
      rm(rate)
    }
  
    if(TRUE){
      # for(rate in rates){
      #   for(grp_scale in c(0,1,2)){
      # 
      #     # cooperation, by group
      #     simdata_sub <- simdata_coop
      #     metric      <- measure_coop[2:5]
      #     label       <- "Average\ncooperation"
      #     width       <- 9
      #     height      <- 5
      #     # rate        <- 1.0
      #     # grp_src_grp <- 1
      # 
      #     filename    <- paste0("plots/", "cooperation_bygroup", "_line_", norm, "_rate_", rate, "_grp_", grp_scale,
      #                           "_grpsrcgrp_", grp_src_grp, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
      #     png(filename = filename,
      #         width = width, height = height, units = "in", res = 600)
      #     print(plot_line_fixrate(simdata_sub, norm, metric, label, rate, grp_scale, grp_src_grp, TRUE))
      #     print_figure(filename)
      # 
      #     # strat frequencies
      #     simdata_sub <- simdata_freq
      #     metric      <- measure_freq
      #     label       <- "Average\nfrequency"
      # 
      #     filename    <- paste0("plots/","freq", "_line_", norm, "_rate_", rate, "_grp_", grp_scale,
      #                           "_grpsrcgrp_", grp_src_grp, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
      #     png(filename = filename,
      #         width = width, height = height, units = "in", res = 600)
      #     print(plot_line_fixrate(simdata_sub, norm, metric, label, rate, grp_scale, grp_src_grp, FALSE))
      #     print_figure(filename)
      # 
      #     # indiv reputations
      #     simdata_sub <- simdata_rep_ind
      #     metric      <- measure_rep_ind
      #     label       <- "Fraction good"
      # 
      #     filename    <- paste0("plots/","rep_ind", "_line_", norm, "_rate_", rate, "_grp_", grp_scale,
      #                           "_grpsrcgrp_", grp_src_grp, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
      #     png(filename = filename,
      #         width = width, height = height, units = "in", res = 600)
      #     print(plot_line_fixrate(simdata_sub, norm, metric, label, rate, grp_scale, grp_src_grp, FALSE))
      #     print_figure(filename)
      # 
      #     # stereotypes
      #     simdata_sub <- simdata_rep_grp
      #     metric      <- measure_rep_grp
      #     label       <- "Fraction good"
      # 
      #     filename    <- paste0("plots/","rep_grp", "_line_", norm, "_rate_", rate, "_grp_", grp_scale,
      #                           "_grpsrcgrp_", grp_src_grp, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
      #     png(filename = filename,
      #         width = width, height = height, units = "in", res = 600)
      #     print(plot_line_fixrate(simdata_sub, norm, metric, label, rate, grp_scale, grp_src_grp, FALSE))
      #     print_figure(filename)
      # 
      #     # fitnesses
      #     simdata_sub <- simdata_fitness
      #     metric      <- measure_fitness
      #     label       <- "Average fitness"
      # 
      #     filename    <- paste0("plots/","fitness", "_line_", norm, "_rate_", rate, "_grp_", grp_scale,
      #                           "_grpsrcgrp_", grp_src_grp, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
      #     png(filename = filename,
      #         width = width, height = height, units = "in", res = 600)
      #     print(plot_line_fixrate(simdata_sub, norm, metric, label, rate, grp_scale, grp_src_grp, FALSE))
      #     print_figure(filename)
      #   }
      # }
    }
  }
}
