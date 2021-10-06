#########################
# Function to check nos. and types of of distinct cases
#########################

casecounter <- function(simdata){
  casecount <- simdata %>% 
    group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp) %>% 
    summarize(COUNT = n())
  
  return(casecount)
}

# with more details
casecounter2 <- function(simdata){
  casecount <- simdata %>% 
    group_by(N, norm, ind_scale, grp_scale, ind_base, grp_base, ind_src_ind, grp_src_grp, prob, rate, cost, bias) %>% 
    summarize(COUNT = n())
  
  return(casecount)
}

################
# PLOT HEATMAPS
################
measure_coop    <- c("cooperation","coop_11","coop_12","coop_21","coop_22") 

plot_heatmap_fixed_cost <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                    label = "Average\ncooperation", cost = 0.0, 
                                    grp_src_grp = 0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric == metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print_subdata(subdata)}
  
  # something is not working with this function
  settings <- heatmap_colors_and_labels(subdata)
  settings$ybreaks <- seq(0, 1.0, 0.2)
  settings$ylimits <- c(-0.1, 1.1)
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = rate, fill = Mean)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (cost α = ", cost, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_fill_gradient2(
      # low = "black", mid = "#CFD5D9", high = "white",
      low = "#f7fbff", mid = "#6baed6", high = "#08306b",
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
  
  if(verbose){print_subdata(subdata)}

  settings <- heatmap_colors_and_labels(subdata)
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = cost, fill = Mean)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (rate λ = ", rate, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_fill_gradient2(
      # low = "black", mid = "#CFD5D9", high = "white",
      low = "#f7fbff", mid = "#6baed6", high = "#08306b",
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
  
  # settings <- heatmap_colors_and_labels(subdata)
  ybreaks  <- seq(0, 1.0, 0.2)
  ylimits  <- c(-0.1, 1.1)
  settings <- list(ybreaks = ybreaks, ylimits = ylimits)
  
  if(verbose){print_subdata(subdata)}
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = bias, fill = Mean)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (rate λ = ", rate, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_fill_gradient2(
      # low = "black", mid = "#CFD5D9", high = "white",
      low = "#f7fbff", mid = "#6baed6", high = "#08306b",
      # low = "#BD1513", mid = "#CFD5D9", high = "#00428B",
      # low = "white", mid = "#CFD5D9", high = "#00428B",
      midpoint = 0.5,
      limit    = c(0., 1.),
      space    = "Lab",
      name     = label) +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = "Probability of interacting with out-group members (out_bias)") +
    geom_tile( show.legend = TRUE ) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside")
  
  return(fig)
}

################################
# PLOT GRID OF LINE PLOTS
################################
plot_linegrid_fixed_rate_cost <- function(simdata_sub, norm = "SJ", metric = "cooperation", 
                                          label = "Average\ncooperation", rate = 1.0, cost = 0.0,
                                          grp_src_grp = 0, verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print_subdata(subdata)}
  
  settings <- lineplot_colors_and_labels(metric)
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (rate λ = ", rate, ", cost α = ", cost, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_color_manual( values = settings$colors) +
    scale_linetype_manual(values = settings$linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = settings$ylabel) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside",
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    )
  
  return(fig)
}

plot_linegrid_fixed_rate_cost_prob <- function(simdata_sub, norm = "SJ", metric = "cooperation",
                                          label = "Average\ncooperation", rate = 1.0, cost = 0.0, prob = 1.0,
                                          grp_src_grp = 0,  verbose = FALSE){

  print(rate)
  subdata <- simdata_sub[simdata_sub$Metric %in% metric &
                           simdata_sub$norm == norm &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$prob == paste0("",prob,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]

  if(verbose){print_subdata(subdata)}

  settings <- lineplot_colors_and_labels(metric)

  fig <- ggplot(data = subdata,
                aes(x = bias, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (rate λ = ", rate, ", prob p = ", prob, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_color_manual( values = settings$colors) +
    scale_linetype_manual(values = settings$linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of interacting with out-group members (out_bias)",
         y = settings$ylabel) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside",
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    )

  return(fig)
}

plot_linegrid_fixed_cost_prob_bias <- function(simdata_sub, norm = "SJ", metric = "cooperation",
                                               label = "Average\ncooperation", cost = 0.0, prob = 1.0,
                                               grp_src_grp = 0,  verbose = FALSE){
  
  subdata <- simdata_sub[simdata_sub$Metric %in% metric &
                           simdata_sub$norm == norm &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$prob == paste0("",prob,"") &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if("bias" %in% colnames(subdata)){warning("check bias parameters")}
  
  if(verbose){print_subdata(subdata)}
  
  settings <- lineplot_colors_and_labels(metric)
  
  fig <- ggplot(data = subdata,
                aes(x = rate, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm, " (cost α = ", cost, ", prob p = ", prob, ")") ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.1)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_color_manual( values = settings$colors) +
    scale_linetype_manual(values = settings$linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of group reputation update per round (λ)",
         y = settings$ylabel) +
    facet_grid(ind_scale_label ~ grp_scale_label,
               space="free", scales="free") +
    theme(strip.placement = "outside",
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    )
  
  return(fig)
}
################
# PLOT LINES
################
plot_line_fixcost <- function(simdata_sub, norm = "SJ", metric = measure_coop[2:5], 
                              label = "Average\ncooperation", cost = 0.0, 
                              grp_scale = 0, grp_src_grp = 0, verbose = FALSE){
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$cost == paste0("",cost,"") &
                           simdata_sub$grp_scale == grp_scale &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  settings <- lineplot_colors_and_labels(metric)
  
  if(verbose){print_subdata(subdata)}
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm ) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_color_manual( values = settings$colors) +
    scale_linetype_manual(values = settings$linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line(size = 0.4, alpha = 1) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = settings$ylabel) +
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
  settings <- lineplot_colors_and_labels(metric)
  
  # fix ind_base = 1
  subdata <- simdata_sub[simdata_sub$Metric %in% metric & 
                           simdata_sub$norm == norm & 
                           simdata_sub$ind_base == TRUE &
                           simdata_sub$rate == paste0("",rate,"") &
                           simdata_sub$grp_scale == grp_scale &
                           simdata_sub$grp_src_grp == grp_src_grp, ]
  
  if(verbose){print_subdata(subdata)}
  
  fig <- ggplot(data = subdata,
                aes(x = prob, y = Mean, color = Metric, linetype = Metric, label = Metric)) +
    theme_mk() +
    ggtitle( paste0("norm ", norm ) ) +
    scale_x_continuous( breaks = seq(0, 1, 0.2)) +
    scale_y_continuous( breaks = settings$ybreaks, limits = settings$ylimits) +
    scale_color_manual( values = settings$colors) +
    scale_linetype_manual(values = settings$linetypes) +
    labs(color = label, linetype = label) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0, size = 0.3) +
    geom_line( size = 0.4, alpha = 1 ) +
    geom_point(size = 1.1, alpha = 1, stroke = 0.5) + #, shape = 21, fill = "white") +
    labs(x = "Probability of DISC using stereotypes (p)",
         y = settings$ylabel) +
    facet_grid(ind_scale_label ~ cost_label, 
               space="free", scales="free") +
    theme(strip.placement = "outside",
          # strip.background = element_blank()
          panel.grid.major = element_line(size = 0.2, colour = "grey95")
    ) 
  
  return(fig)
}