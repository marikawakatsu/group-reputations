##################################################
#
# Miscelleanous settings and preferences
#
##################################################

#########################
# Default theme
#########################
theme_mk <- function(){
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
    )
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

lineplot_colors_and_labels <- function(metric){
  # choose custom colors for line plots based on metric
  
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
  
  return( list(colors=colors, linetypes=linetypes, ybreaks=ybreaks, ylimits=ylimits, ylabel=ylabel) )
}

heatmap_colors_and_labels <- function(subdata){
  # choose custom colors for heatmaps based on subdata
  
  if(max(subdata$cost) < 0.11){
    ybreaks = seq(0, 0.1, 0.02)
    ylimits = c(-0.01, 0.111)
  }else if(max(subdata$cost) < 1.1){
    ybreaks = seq(0, 1.0, 0.2)
    ylimits = c(-0.1, 1.1)
  }
  
  return( list(ybreaks=ybreaks, ylimits=ylimits) )
}

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
