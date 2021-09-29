##################################################
#
# Various functions
#
################################################### 
require(reshape2)
require(igraph)
require(ggplot2)
require(msm)
require(tidyverse)
require(dplyr) # may need to revert
require(gtools)
require(R.matlab)
# require(ggnetwork)
# require(ggrepel)
require(viridis)
require(RColorBrewer)
require(varhandle)
require(data.table)
require(grid)
require(gtable)
# require(gridExtra)
# require(lemon)
# require(ggpubr)
# require(stargazer)

#########################
# Multiple plot function
#########################
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# from https://github.com/christokita/mixing-model

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#########################
# Colors
#########################
cool_warm <- function(n) {
  colormap <- Rgnuplot:::GpdivergingColormap(seq(0,1,length.out=n),
                                             rgb1 = colorspace::sRGB( 0.230, 0.299, 0.754),
                                             rgb2 = colorspace::sRGB( 0.706, 0.016, 0.150),
                                             outColorspace = "sRGB")
  colormap[colormap>1] <- 1 # sometimes values are slightly larger than 1
  colormap <- grDevices::rgb(colormap[,1], colormap[,2], colormap[,3])
  colormap
}

#########################
# Function to check nos. and types of of distinct cases
#########################
casecounter <- function(simdata){
  casecount <- simdata %>% 
    group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp) %>% 
    summarize(COUNT = n())
  
  return(casecount)
}
#########################
# printing function
#########################
print_figure <- function(filename = "NONE"){
  dev.off()
  Sys.sleep(1)
  # print("done")
  print(paste0(filename, " -- DONE"))
}


################
# PLOT HEATMAP
################
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 

plot_heatmap_fixed_cost <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                    label = "Average\ncooperation", cost = 0.0, 
                                    grp_src_grp = 0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print(casecounter(subdata))}
  
  if(max(subdata$cost) < 0.11){
    ybreaks = seq(0, 0.1, 0.02)
    ylimits = c(-0.01, 0.111)
  }else if(max(subdata$cost) < 1.1){
    ybreaks = seq(0, 1.0, 0.2)
    ylimits = c(-0.1, 1.1)
  }
  
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
    scale_y_continuous( breaks = ybreaks, limits = ylimits) +
    scale_fill_gradient2(low = "black", mid = "#CFD5D9", high = "white",
                         # low = "#BD1513", mid = "#CFD5D9", high = "#00428B",
                         # low = "white", mid = "#CFD5D9", high = "#00428B",
                         midpoint = 0.5,
                         limit    = c(0., 1.),
                         space    = "Lab",
                         name     = label) +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = "Probability of group reputation update per round (λ)") +
    geom_tile( show.legend = TRUE ) + 
    facet_grid(ind_scale_label ~ grp_scale_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside") 
  
  return(fig)
}

plot_heatmap_fixed_rate <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                    label = "Average\ncooperation", rate = 0.0, 
                                    grp_src_grp = 0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print(casecounter(subdata))}
  
  if(max(subdata$cost) < 0.11){
    ybreaks = seq(0, 0.1, 0.02)
    ylimits = c(-0.01, 0.111)
  }else if(max(subdata$cost) < 1.1){
    ybreaks = seq(0, 1.0, 0.2)
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
    labs(x = "Probability of DISC using stereotypes (p)",
         y = "Cost of using individual reputations (α)") +
    geom_tile( show.legend = TRUE ) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside")
  
  return(fig)
}

plot_heatmap_bias       <- function(simdata_sub, norm = "SJ", metric = "cooperation",
                                    label = "Average\ncooperation", rate = 0.0, cost = 0.0,
                                    grp_src_grp = 0, verbose = FALSE){

  subdata <- simdata_sub[simdata_sub$Metric == metric &
                           simdata_sub$norm == norm &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]

  if(verbose){print(casecounter(subdata))}

  if(max(subdata$cost) < 0.11){
    ybreaks = seq(0, 0.1, 0.02)
    ylimits = c(-0.01, 0.111)
  }else if(max(subdata$cost) < 1.1){
    ybreaks = seq(0, 1.0, 0.2)
    ylimits = c(-0.1, 1.1)
  }

  # check that the subsetting is done correctly
  if(verbose){
    print( paste0("costs: ", unique(subdata$cost) ))
    print( paste0("rates: ", unique(subdata$rate) ))
    print( paste0("probs: ", unique(subdata$prob) ))
  }

  fig <- ggplot(data = subdata,
                aes(x = prob, y = bias, fill = Mean)) +
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
    labs(x = "Probability of DISC using stereotypes (p)",
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
plot_line_fixcost <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], 
                              label = "Average\ncooperation", cost = 0.0, 
                              grp_scale = 0, grp_src_grp = 0, verbose = FALSE){
  
  # choose colors
  if( "coop_11" %in% metric ){
    colors    = c("#3B528BFF", "#3B528BFF", "#5DC863FF", "#5DC863FF") # viridis(5)[1:4]
    linetypes = c("solid", "dotted", "dotted", "solid")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "reps_ind_11" %in% metric || "reps_grp_11" %in% metric ){
    colors    = c("#810f7c", "#810f7c", "#8c96c6", "#8c96c6") # magma(6)[2:5]
    linetypes = c("solid", "dotted", "dotted", "solid")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "freq1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "fitness1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
    ybreaks   = seq(-0.8, 0.8, 0.2)
    ylimits   = c(-.25, .85)
    ylabel    = "Fitness"
  }
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_scale == grp_scale &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print(casecounter(subdata))}
  
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
    scale_y_continuous( breaks = ybreaks, limits = ylimits) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = ylabel) +
    facet_grid(ind_scale_label ~ cost_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    ) 
  
  return(fig)
}

plot_line_fixrate <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], 
                              label = "Average\ncooperation", rate = 1.0, 
                              grp_scale = 0, grp_src_grp = 0, verbose = TRUE){
  
  # choose colors
  if( "coop_11" %in% metric ){
    colors    = c("#3B528BFF", "#3B528BFF", "#5DC863FF", "#5DC863FF") # viridis(5)[1:4]
    linetypes = c("solid", "dotted", "dotted", "solid")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "reps_ind_11" %in% metric || "reps_grp_11" %in% metric ){
    colors    = c("#810f7c", "#810f7c", "#8c96c6", "#8c96c6") # magma(6)[2:5]
    linetypes = c("solid", "dotted", "dotted", "solid")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "freq1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
    ybreaks   = seq(0, 1, 0.2)
    ylimits   = c(0, 1)
    ylabel    = "Frequency"
  }else if( "fitness1_ALLC" %in% metric){
    colors = c("#377eb8","#e41a1c","#ff7f00","#377eb8","#e41a1c","#ff7f00")
    linetypes = c("solid", "solid", "solid", "dotted", "dotted", "dotted")
    ybreaks   = seq(-0.8, 0.8, 0.2)
    ylimits   = c(-.25, .85)
    ylabel    = "Fitness"
  }
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$grp_scale == grp_scale &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print(casecounter(subdata))}
  
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
    scale_y_continuous( breaks = ybreaks, limits = ylimits) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = ylabel) +
    facet_grid(ind_scale_label ~ cost_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    ) 
  
  return(fig)
}