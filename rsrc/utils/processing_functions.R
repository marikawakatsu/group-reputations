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