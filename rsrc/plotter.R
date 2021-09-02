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
data_dir  <- "~/Dropbox (Princeton)/Stereotypes_results/group-reputations/sweep_src-prob-rate/"

# load and merge data
simdata_m <- read.csv( paste0(data_dir, "m_data.csv"), header = TRUE)
simdata_s <- read.csv( paste0(data_dir, "s_data.csv"), header = TRUE)
simdata   <- rbind(simdata_m, simdata_s)

casecount <- simdata %>% group_by(N, norm, ind_base, grp_base) %>% summarize(COUNT = n())

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

simdata$ind_base[simdata$ind_base == 0] <- "Individual reps random"
simdata$ind_base[simdata$ind_base == 1] <- "Individual reps based on behavior"
simdata$grp_base[simdata$grp_base == 0] <- "Group reps random"
simdata$grp_base[simdata$grp_base == 1] <- "Group reps based on behavior"

# select columns
simdata_bymeasure <- simdata %>% 
  gather("variable","value",-N,-norm,-all_strategies,-num_groups,-group_sizes,-generation,
         -ind_public,-grp_public,-ind_base,-grp_base,-ind_src_ind,-grp_src_grp,-prob,-rate)
simdata_bymeasure$value = as.numeric(simdata_bymeasure$value)

# compute means and vars
compute_mean_vars <- function( simdata_bymeasure, variables ){
  simdata_bymeasure[which(simdata_bymeasure$variable %in% variables),] %>%
    rename( Fraction = value, Metric = variable ) %>%
    group_by( N, norm, ind_base, grp_base, prob, rate, Metric ) %>%
    summarise(
      Mean = mean(Fraction),
      SD = sd(Fraction),
      SE = sd(Fraction) / sqrt(length(Fraction)),
      numCases = length(Fraction)
    )
}

simdata_coop    <- compute_mean_vars(simdata_bymeasure, measure_coop)
simdata_rep_ind <- compute_mean_vars(simdata_bymeasure, measure_rep_ind)
simdata_rep_grp <- compute_mean_vars(simdata_bymeasure, measure_rep_grp)
simdata_freq    <- compute_mean_vars(simdata_bymeasure, measure_freq)
simdata_fitness <- compute_mean_vars(simdata_bymeasure, measure_fitness)

#############
# PLOT DATA
#############
plot_heatmap <- function(simdata_sub, metric = "cooperation", label = "Global\ncooperation", norm = "SJ"){
  
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
    facet_grid(grp_base ~ ind_base, 
               space="free", switch = "both", scales="free") +
    theme(strip.placement = "outside",
          strip.background = element_blank()) 
  
  return(fig)
}

# test plots
simdata_sub <- simdata_coop
metric      <- "cooperation"
label       <- "Global\ncooperation"
norms       <- unique(simdata$norm)

for(norm in norms){
  
  png(filename = paste0("plots/", metric, "_", norm, "_",
                        format(Sys.Date(), format="%y%m%d"), "_SD.png"),
      width = 6, height = 5, units = "in", res = 600)
  print(plot_heatmap(simdata_sub, metric, label, norm))
  dev.off()
  Sys.sleep(1)
}
