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
data_sub_dir <- "DISC-scale-prob-recip"
tag          <- "_DISC_recip"
simdata_m <- read.csv( paste0(data_dir, data_sub_dir, "/", "m_data", tag, ".csv"), header = TRUE)
simdata   <- simdata_m
# simdata_s <- read.csv( paste0(data_dir, data_sub_dir, "/", "s_data", tag, ".csv"), header = TRUE)
# simdata   <- rbind(simdata_m, simdata_s)

casecount <- casecounter(simdata)
# simdata   <- simdata %>%
#   group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp) %>%
#   slice_head( n = min(casecount$COUNT) )

##########################
# PREP DATA FOR PLOTTING
##########################
# select columns
id_vars         <- c("N","norm","all_strategies","num_groups","group_sizes","generation",
                     "ind_scale","grp_scale","ind_recip","grp_recip","ind_base","grp_base","ind_src_ind","grp_src_grp","prob","rate","cost")
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 
measure_rep_ind <- c("reps_ind_11","reps_ind_12","reps_ind_21","reps_ind_22") 
measure_rep_grp <- c("reps_grp_11","reps_grp_12","reps_grp_21","reps_grp_22") 
measure_freq    <- c("freq1_ALLC","freq1_ALLD","freq1_DISC","freq2_ALLC","freq2_ALLD","freq2_DISC")
measure_fitness <- c("fitness1_ALLC","fitness1_ALLD","fitness1_DISC","fitness2_ALLC","fitness2_ALLD","fitness2_DISC")
measure_agree   <- c("agree_ind", "agree_grp")

# select columns
simdata_bymeasure <- simdata %>% 
  gather("variable","value",-N,-norm,-all_strategies,-num_groups,-group_sizes,-generation,
         -ind_scale,-grp_scale,-ind_recip,-grp_recip,-ind_base,-grp_base,-ind_src_ind,-grp_src_grp,-prob,-rate,-cost)
simdata_bymeasure$value = as.numeric(simdata_bymeasure$value)

# compute means and vars
compute_mean_vars <- function( simdata_bymeasure, variables ){
  simdata_mean <- simdata_bymeasure[which(simdata_bymeasure$variable %in% variables),] %>%
    rename( Fraction = value, Metric = variable ) %>%
    group_by( N, norm, ind_scale, grp_scale, ind_recip, grp_recip, ind_base, grp_base, ind_src_ind, grp_src_grp, prob, rate, cost, Metric ) %>%
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
  
  # new 10/26/21
  simdata_mean <- simdata_mean %>% mutate( recip_label = paste0( "(", ind_recip, ",", grp_recip, ")" ) )
  simdata_mean$recip_label <- factor(simdata_mean$recip_label, 
                                     levels = c("(0,0)", "(0,1)", "(1,0)", "(1,1)", "(0,2)", "(2,0)", "(1,2)", "(2,1)", "(2,2)"))
  
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
simdata_agree   <- compute_mean_vars(simdata_bymeasure, measure_agree)

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
ind_recips  <- unique(simdata$ind_recip)
grp_recips  <- unique(simdata$grp_recip)
recip_cases <- unique(simdata_coop$recip_label)

if(length(biases) > 1){stop("more than one bias values, check plots")}

for(norm in norms){
  
  for(grp_src_grp in srcs){
    
    # cooperation, all
    simdata_sub <- simdata_coop
    metric      <- "cooperation"
    label       <- "Recipient\ntype"
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
      }
      rm(rate)
    }
    if( length(ind_recips) > 1 ){
      for(rate in rates){
        filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, 
                           tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
        png(filename = filename,
            width = width, height = height, units = "in", res = 600)
        print(plot_linegrid_recip(simdata_sub, norm, metric, label, rate, min(costs), grp_src_grp, TRUE)) 
        print_figure(filename)
        
        # selected panels
        filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, 
                           tag, "_", format(Sys.Date(), format="%y%m%d"), "_select.png")
        png(filename = filename, width = width/2, height = height/2.25, units = "in", res = 600)
        print(plot_linegrid_recip_select(simdata_sub, norm, metric, label, rate, min(costs), grp_src_grp, TRUE)) 
        print_figure(filename)
      }
    }
    
    # agreement
    simdata_sub <- simdata_agree
    metric      <- measure_agree
    label       <- "Agreement"
    width       <- 6
    height      <- 5.5
    
    if( length(ind_recips) > 1 ){
      for(rate in rates){
        for(recip_case in recip_cases){
          filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, 
                             "_recip_", recip_case, tag, "_", format(Sys.Date(), format="%y%m%d"), ".png")
          png(filename = filename,
              width = width, height = height, units = "in", res = 600)
          print(plot_linegrid_recip_agree(simdata_sub, norm, metric, label, rate, min(costs), grp_src_grp, recip_case, TRUE)) 
          print_figure(filename)
          
          # selected panels
          filename <- paste0("plots/", "cooperation", "_linegrid_", norm, "_rate_", rate, "_cost_", min(costs), "_bias_", biases, 
                             "_recip_", recip_case, tag, "_", format(Sys.Date(), format="%y%m%d"), "_select.png")
          png(filename = filename, width = width, height = height/2.5, units = "in", res = 600)
          print(plot_linegrid_recip_agree_select(simdata_sub, norm, metric, label, rate, min(costs), grp_src_grp, recip_case, TRUE)) 
          print_figure(filename)
        }
      }
    }
  }
}
