#########################
# Function to print and save plots
#########################
print_figure <- function(filename = "NONE"){
  dev.off()
  Sys.sleep(1)
  # print("done")
  print(paste0(filename, " -- DONE"))
}

#########################
# Function to print data summary
#########################
print_subdata <- function(subdata){
  print(casecounter(subdata))
  # print(casecounter2(subdata))
        
  print( paste0("costs: ", unique(subdata$cost) ))
  print( paste0("rates: ", unique(subdata$rate) ))
  print( paste0("probs: ", unique(subdata$prob) ))
}

#########################
# Variables to select
#########################
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 
measure_rep_ind <- c("reps_ind_11","reps_ind_12","reps_ind_21","reps_ind_22") 
measure_rep_grp <- c("reps_grp_11","reps_grp_12","reps_grp_21","reps_grp_22") 
measure_freq    <- c("freq1_ALLC","freq1_ALLD","freq1_DISC","freq2_ALLC","freq2_ALLD","freq2_DISC")
measure_fitness <- c("fitness1_ALLC","fitness1_ALLD","fitness1_DISC","fitness2_ALLC","fitness2_ALLD","fitness2_DISC")
measure_agree   <- c("agree_ind", "agree_grp")